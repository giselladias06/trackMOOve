library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(epinemo) 
library(Matrix)
library(igraph)
library(DT)
library(RColorBrewer)
library(data.table)
library(EpiContactTrace)


data <- fread("GTA_bovinos_2014_jan.csv", encoding = "UTF-8")

global_palette <- brewer.pal(8, "Accent")

# Define server logic
server <- function(input, output, session) {
  
  # Cache filtered data for better performance
  filtered_data_cache <- reactiveVal()
  
  # Update date range based on available dates in the dataset
  observe({
    updateDateRangeInput(session, "date_range", min = min(data$data_emissao), max = max(data$data_emissao))
  })
  
  # Efficiently filter data using data.table and cache the result
  observe({
    req(input$date_range)
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    filtered_data <- data[data_emissao >= start_date & data_emissao <= end_date]
    filtered_data_cache(filtered_data)
  })
  
  # Descriptive Analysis outputs ---------
    
output$animais <- renderValueBox({
    filtered_data_df <- filtered_data_cache()
    
    animais <- filtered_data_df %>% 
      group_by(finalidade_fix, 
               cod_prop_origem,
               cod_prop_destino, 
               tipo,
               data_emissao) %>% 
      summarise(
        contagem = n(),
        qtde_animais = sum(as.numeric(quant_animais))) %>% 
      ungroup() %>% 
      mutate(qtde_animais = as.numeric(qtde_animais)) %>%
      summarise(qtde_animais = sum(qtde_animais, na.rm = T))
    valueBox(
      paste0(animais$qtde_animais), "Número de animais movimentados na rede", icon = icon("cow"),
      color = "light-blue"
    )
  })
    
  output$movimentacoes <- renderValueBox({
    filtered_data_df <- filtered_data_cache()
    
    movimentacoes<- filtered_data_df %>% 
      summarise(contagem = n()) 
    
    valueBox(
      paste0(movimentacoes$contagem), "Número de lotes movimentados na rede", icon = icon("truck"),
      color = "light-blue"
    )
  })
  
  output$abatedouros <- renderValueBox({
    filtered_data_df <- filtered_data_cache()
    
    abatedouros <- filtered_data_df %>%  
      select(cod_prop_destino, tipo) %>%
      filter((tipo %in% c("SIM", "SIE", "SIF"))) %>%
      distinct(cod_prop_destino) %>% 
      summarise(count = n())
    
    valueBox(
      paste0(abatedouros$count), "Número de abatedouros na rede", icon = icon("drumstick-bite"),
      color = "light-blue"
    )
  })
  
  output$propriedades <- renderValueBox({
    filtered_data_df <- filtered_data_cache()
    
    abatedouros_count <- filtered_data_df %>%  
      select(cod_prop_destino, tipo) %>%
      filter((tipo %in% c("SIM", "SIE", "SIF"))) %>%
      distinct(cod_prop_destino) 
    
    
    propriedades <- filtered_data_df %>% 
      select(cod_prop_destino, tipo) %>%
      filter(!(tipo %in% c("SIM", "SIE", "SIF"))) %>%
      select(-tipo) %>%
      bind_rows(., 
                filtered_data_df %>% select(cod_prop_origem)
      ) %>% 
      pivot_longer(cols = c(cod_prop_origem, cod_prop_destino),
                   values_to = "cod_prop") %>% 
      select(-name) %>% 
      filter(!is.na(cod_prop)) %>% 
      distinct() %>% 
      filter(!cod_prop %in% abatedouros_count$cod_prop_destino) %>% 
      summarise(count = n())
    
    valueBox(
      paste0(propriedades$count), "Número de estabelecimentos na rede", icon = icon("house-chimney-window"),
      color = "light-blue"
    )
  })
  
  output$chart1 <- renderPlotly({
    filtered_data_df <- filtered_data_cache()
    
    finalidade_count <- filtered_data_df %>%
      count(finalidade_fix) %>%
      arrange(desc(n)) %>%
      mutate(finalidade = factor(finalidade_fix, levels = finalidade_fix),
             color = global_palette[as.numeric(factor(finalidade_fix, levels = unique(finalidade_fix)))])
    
    plot_ly(
      data = finalidade_count,
      x = ~n,
      y = ~finalidade,
      type = 'bar',
      marker = list(color = ~color),
      orientation = 'h'
    ) %>%
      layout(
        title = list(text = 'Quantidade de movimentações por finalidade', font = list(color = '#063376', size = 12)),
        xaxis = list(title = ' ', showgrid = FALSE, zeroline = FALSE, color = '#5b626b'),
        yaxis = list(title = '', showgrid = FALSE, zeroline = FALSE),
        margin = list(l = 200),  # Adjust the left margin if necessary
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$chart2 <- renderPlotly({
    filtered_data_df <- filtered_data_cache()
    
    # Perform the data transformation
    data_animais <- filtered_data_df %>%
      group_by(finalidade_fix, cod_prop_origem, cod_prop_destino, tipo, data_emissao) %>%
      summarise(
        contagem = n(),
        qtde_animais = sum(as.numeric(quant_animais))
      ) %>%
      ungroup() %>%
      group_by(finalidade_fix) %>%
      summarise(total_qtde_animais = sum(qtde_animais, na.rm = TRUE)) %>%
      arrange(desc(total_qtde_animais)) %>%
      mutate(finalidade_fix = factor(finalidade_fix, levels = finalidade_fix),
             color = global_palette[as.numeric(factor(finalidade_fix, levels = unique(finalidade_fix)))])
    
    # Create the Plotly chart
    plot_ly(
      data = data_animais,
      x = ~total_qtde_animais,
      y = ~finalidade_fix,
      type = 'bar',
      marker = list(color = ~color),
      orientation = 'h'
    ) %>%
      layout(
        title = list(text = 'Quantidade de animais movimentados por finalidade', font = list(color = '#063376', size = 12)),
        xaxis = list(title = ' ', showgrid = FALSE, zeroline = FALSE, color = '#5b626b'),
        yaxis = list(title = '', showgrid = FALSE, zeroline = FALSE),
        margin = list(l = 200),  # Adjust the left margin if necessary
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$chart3 <- renderPlotly({
    filtered_data_df <- filtered_data_cache()
    
    # Convert 'data_emissao' to Date class
    filtered_data_df <- filtered_data_df %>%
      mutate(data_emissao = as.Date(data_emissao))
    
    # Calculate the duration of the selected date range
    start_date <- as.Date(input$date_range[1], format = "%Y-%m-%d")
    end_date <- as.Date(input$date_range[2], format = "%Y-%m-%d")
    duration_days <- as.numeric(difftime(end_date, start_date, units = "days"))
    
    # Determine the aggregation level
    if (duration_days <= 2) {
      # Directly use the dates as categories for the X-axis
      aggregated_data <- filtered_data_df %>%
        group_by(data_emissao) %>%
        summarise(
          contagem = n(),
          qtde_animais = sum(as.numeric(quant_animais))
        ) %>%
        ungroup()
      
      # Set X-axis as a discrete category
      plot_ly(
        data = aggregated_data,
        x = ~factor(data_emissao),  # Treat dates as discrete categories
        y = ~contagem,
        type = 'bar',
        marker = list(color = 'skyblue')
      ) %>%
        layout(
          title = list(text = 'Numero de movimentaçoes por dia', font = list(color = '#063376', size = 12)),
          xaxis = list(title = 'Day', tickangle = 45),
          yaxis = list(title = 'Quantidade'),
          margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4),
          autosize = TRUE
        ) %>%
        config(displayModeBar = FALSE)
      
    } else if (duration_days > 30) {
      # Aggregate by month
      aggregated_data <- filtered_data_df %>%
        mutate(year_month = floor_date(data_emissao, "month")) %>%
        group_by(year_month) %>%
        summarise(
          contagem = n(),
          qtde_animais = sum(as.numeric(quant_animais))
        ) %>%
        ungroup()
      
      plot_ly(
        data = aggregated_data,
        x = ~year_month,
        y = ~contagem,
        type = 'bar',
        marker = list(color = 'skyblue')
      ) %>%
        layout(
          title = list(text = 'Numero de movimentaçoes por mês', font = list(color = '#063376', size = 12)),
          xaxis = list(title = 'Month', type = 'date', tickformat = "%b %Y", tickangle = 45),
          yaxis = list(title = 'Quantidade'),
          margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4),
          autosize = TRUE
        ) %>%
        config(displayModeBar = FALSE)
      
    } else {
      # Aggregate by week
      aggregated_data <- filtered_data_df %>%
        mutate(year_week = floor_date(data_emissao, "week")) %>%
        group_by(year_week) %>%
        summarise(
          contagem = n(),
          qtde_animais = sum(as.numeric(quant_animais))
        ) %>%
        ungroup()
      
      plot_ly(
        data = aggregated_data,
        x = ~year_week,
        y = ~contagem,
        type = 'bar',
        marker = list(color = 'skyblue')
      ) %>%
        layout(
          title = list(text = 'Numero de lotes movimentados por semana', font = list(color = '#063376', size = 12)),
          xaxis = list(title = 'Week', type = 'date', tickformat = "%d %b %Y", tickangle = 45),
          yaxis = list(title = 'Quantidade'),
          margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4),
          autosize = TRUE
        ) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  output$chart4 <- renderPlotly({
    filtered_data_df <- filtered_data_cache()
    
    # Convert 'data_emissao' to Date class
    filtered_data_df <- filtered_data_df %>%
      mutate(data_emissao = as.Date(data_emissao))
    
    # Calculate the duration of the selected date range
    start_date <- as.Date(input$date_range[1], format = "%Y-%m-%d")
    end_date <- as.Date(input$date_range[2], format = "%Y-%m-%d")
    duration_days <- as.numeric(difftime(end_date, start_date, units = "days"))
    
    # Determine the aggregation level
    if (duration_days <= 2) {
      # Directly use the dates as categories for the X-axis
      aggregated_data <- filtered_data_df %>%
        group_by(data_emissao) %>%
        summarise(
          contagem = n(),
          qtde_animais = sum(as.numeric(quant_animais))
        ) %>%
        ungroup()
      
      # Set X-axis as a discrete category
      plot_ly(
        data = aggregated_data,
        x = ~factor(data_emissao),  # Treat dates as discrete categories
        y = ~qtde_animais,
        type = 'bar',
        marker = list(color = 'skyblue')
      ) %>%
        layout(
          title = list(text = 'Numero de animais movimentados por dia', font = list(color = '#063376', size = 12)),
          xaxis = list(title = 'Day', tickangle = 45),
          yaxis = list(title = 'Quantidade'),
          margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4),
          autosize = TRUE
        ) %>%
        config(displayModeBar = FALSE)
      
    } else if (duration_days > 30) {
      # Aggregate by month
      aggregated_data <- filtered_data_df %>%
        mutate(year_month = floor_date(data_emissao, "month")) %>%
        group_by(year_month) %>%
        summarise(
          contagem = n(),
          qtde_animais = sum(as.numeric(quant_animais))
        ) %>%
        ungroup()
      
      plot_ly(
        data = aggregated_data,
        x = ~year_month,
        y = ~qtde_animais,
        type = 'bar',
        marker = list(color = 'skyblue')
      ) %>%
        layout(
          title = list(text = 'Numero de animais movimentados por mês', font = list(color = '#063376', size = 12)),
          xaxis = list(title = 'Month', type = 'date', tickformat = "%b %Y", tickangle = 45),
          yaxis = list(title = 'Quantidade'),
          margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4),
          autosize = TRUE
        ) %>%
        config(displayModeBar = FALSE)
      
    } else {
      # Aggregate by week
      aggregated_data <- filtered_data_df %>%
        mutate(year_week = floor_date(data_emissao, "week")) %>%
        group_by(year_week) %>%
        summarise(
          contagem = n(),
          qtde_animais = sum(as.numeric(quant_animais))
        ) %>%
        ungroup()
      
      plot_ly(
        data = aggregated_data,
        x = ~year_week,
        y = ~qtde_animais,
        type = 'bar',
        marker = list(color = 'skyblue')
      ) %>%
        layout(
          title = list(text = 'Numero de animais movimentados por semana', font = list(color = '#063376', size = 12)),
          xaxis = list(title = 'Week', type = 'date', tickformat = "%d %b %Y", tickangle = 45),
          yaxis = list(title = 'Quantidade'),
          margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4),
          autosize = TRUE
        ) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  
  output$chart5 <- renderPlotly({
    filtered_data_df <- filtered_data_cache()
    
    aggregated_data <- filtered_data_df %>%
      group_by(finalidade_fix, cod_prop_origem, cod_prop_destino, tipo, data_emissao) %>%
      summarise(
        contagem = n(),
        qtde_animais = sum(as.numeric(quant_animais))
      ) %>%
      ungroup() %>% 
      filter(tipo %in% c("SIF", "SIE", "SIM")) %>%
      group_by(tipo) %>%
      summarise(total_qtde_animais = sum(qtde_animais, na.rm = TRUE))
    
    plot_ly(
      data = aggregated_data,
      labels = ~tipo,
      values = ~total_qtde_animais,
      type = 'pie',
      textinfo = 'label+percent',
      marker = list(colors = c('#063376','#C3B1E1', 'darkorange'))  # Example colors for each slice
    ) %>%
      layout(
        height = 300,  # Adjust height to fit better in the container
        margin = list(l = 10, r = 10, b = 10, t = 50, pad = 0),
        autosize = TRUE,
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  output$chart6 <- renderPlotly({
    
    filtered_data_df <- filtered_data_cache()
    
    aggregated_data <- filtered_data_df %>%
      group_by(municipio.origem) %>% 
      summarise(
        contagem = n(),
        qtde_animais = sum(as.numeric(quant_animais))
      ) %>%
      ungroup() %>% 
      arrange(desc(qtde_animais)) %>% 
      slice_head(n=10)%>%
      mutate(municipio.origem = factor(municipio.origem, levels = municipio.origem))  # Reorder factor levels
    
    
    plot_ly(
      data = aggregated_data,
      x = ~municipio.origem,
      y = ~qtde_animais,
      type = 'bar',
      marker = list(color = '#063376')
    ) %>%
      layout(
        title = list(text = '10 cidades que mais enviaram animais', font = list(color = '#063376', size = 13)),
        xaxis = list(
          title = '',
          tickangle = -45,  # Rotate labels to 45 degrees
          tickfont = list(size = 8)  # Adjust font size to be smaller
        ),
        yaxis = list(title = 'Quantidade'),
        margin = list(l = 50, r = 50, b = 70, t = 50, pad = 4),
        autosize = TRUE,
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  
  
  output$chart7 <- renderPlotly({
    
    filtered_data_df <- filtered_data_cache()
    
    aggregated_data <- filtered_data_df %>%
      group_by(municipio.destino) %>% 
      summarise(
        contagem = n(),
        qtde_animais = sum(as.numeric(quant_animais))
      ) %>%
      ungroup() %>% 
      arrange(desc(qtde_animais)) %>% 
      slice_head(n=10) %>% 
      mutate(municipio.destino = factor(municipio.destino, levels = municipio.destino))  # Reorder factor levels
    
    
    plot_ly(
      data = aggregated_data,
      x = ~municipio.destino,
      y = ~qtde_animais,
      type = 'bar',
      marker = list(color = '#063376')
    ) %>%
      layout(
        title = list(text = '10 cidades que mais receberam animais', font = list(color = '#063376', size = 13)),
        xaxis = list(
          title = '',
          tickangle = -45,  # Rotate labels to 45 degrees
          tickfont = list(size = 8)  # Adjust font size to be smaller
        ),
        yaxis = list(title = 'Quantidade'),
        margin = list(l = 50, r = 50, b = 70, t = 50, pad = 4),
        autosize = TRUE,
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  
  
  
  # Network Analysis Outputs ----
  resultados <- reactive({
    filtered_data_df <- filtered_data_cache()
    
    # Prepare data for network analysis
    propriedades_e_abatedouros <- filtered_data_df %>%
      group_by(finalidade_fix, 
               cod_prop_origem,
               cod_prop_destino, 
               tipo,
               data_emissao) %>% 
      summarise(
        contagem = n(),
        qtde_animais = sum(as.numeric(quant_animais))) %>% 
      ungroup() %>%
      mutate(data_emissao = as.Date(data_emissao, format= "%Y-%m-%d")) %>%
      mutate(cod_prop_origem = as.numeric(cod_prop_origem),
             cod_prop_destino = as.numeric(cod_prop_destino)) %>%
      as.data.frame()
    
    
    banco <- createUniqueIds(propriedades_e_abatedouros,
                             from = 'cod_prop_origem',
                             to = 'cod_prop_destino')
    
    matriz_vizinhos <- sparseMatrix(i = banco$movements$From, j=banco$movements$To,
                                    dims = rep(max(banco$movements$From, banco$movements$To) , 2))
    
    matriz_lotes <- sparseMatrix(i = banco$movements$From, j=banco$movements$To,
                                 x=banco$movements$contagem,
                                 dims = rep(max(banco$movements$From, banco$movements$To) , 2))
    
    matriz_animais <- sparseMatrix(i = banco$movements$From, j=banco$movements$To,
                                   x=banco$movements$qtde_animais,
                                   dims = rep(max(banco$movements$From, banco$movements$To) , 2))
    
    #Node centrality measures
    kin <- colSums(matriz_vizinhos)
    kout <- rowSums(matriz_vizinhos)
    ktotal <-  kin + kout
    
    matriz_nao_direcionada <- matriz_vizinhos + t(matriz_vizinhos) 
    matriz_vizinhos_nao_direcionada <- ((matriz_nao_direcionada > 0) * 1) 
    kvizinhos <- rowSums(matriz_vizinhos_nao_direcionada) 
    
    kin_animais <- colSums(matriz_animais)
    kout_animais <- rowSums(matriz_animais) 
    ktotal_animais <- kin_animais + kout_animais
    
    kin_lotes <- colSums(matriz_lotes)
    kout_lotes <- rowSums(matriz_lotes) 
    ktotal_lotes <- kin_lotes + kout_lotes
    
    lista_de_arestas <- unique(as.matrix(banco$movements[,c('From', 'To')]))
    grafo <- graph_from_edgelist(el = lista_de_arestas, directed = TRUE)
    
    betweenness_ <- betweenness(graph = grafo)
    
    closeness_in <- closeness(graph = grafo, mode = 'in' )
    
    closeness_out <- closeness(graph = grafo, mode = 'out' )
    
    ccc <- calculateContactChain(Data=propriedades_e_abatedouros,
                                 from = 'cod_prop_origem',
                                 to = 'cod_prop_destino',
                                 Time='data_emissao', simultaneous=T)
    
    
    #Network metrics
    
    network_density <- edge_density(grafo)
    
    connected_components <- components(grafo)
    num_components <- connected_components$no
    
    network_diameter <- diameter(grafo)
    average_shortest_path <- mean_distance(grafo, directed = TRUE)
    reciprocity_network <- reciprocity(grafo)
    
    #Making tables
    
    net_metricTable <- data.frame(network_density,num_components, network_diameter,  average_shortest_path, reciprocity_network )
    
    resultadosTable <-  data.frame(banco$correspondence$database.id, kin, kout, kvizinhos, kin_animais, 
                                   kout_animais, ktotal_animais, kin_lotes, kout_lotes, 
                                   ktotal_lotes, betweenness_, closeness_in, closeness_out, ccc$ingoing, ccc$outgoing) 
    
    
    
    
    list(
      resultadosTable = resultadosTable,
      net_metricTable = net_metricTable, 
      kin = kin,
      kout = kout,
      kvizinhos = kvizinhos,
      kin_animais = kin_animais,
      kout_animais = kout_animais,
      ktotal_animais = ktotal_animais,
      kin_lotes = kin_lotes,
      kout_lotes = kout_lotes,
      ktotal_lotes = ktotal_lotes,
      betweenness_ = betweenness_,
      closeness_in = closeness_in,
      closeness_out = closeness_out,
      ccc_ingoing = ccc$ingoing,
      ccc_outgoing = ccc$outgoing
      
    )
    
    
  })
  
  
  # Render DataTable
  output$table_metrics <- DT::renderDataTable({
    
    table_metrics_df <- resultados()$net_metricTable
    
    colnames(table_metrics_df) <- c(
      "Densidade da rede",
      "Numero de componentes conectados",
      "Diametro da rede",
      "Comprimento medio do caminho",
      "Reciprocidade da rede"
    )
    
    
    table_metrics <-  pivot_longer(table_metrics_df, 
                                   cols = everything(), 
                                   names_to = "Medidas da rede", 
                                   values_to = "Valores")
    
    DT::datatable(
      table_metrics, 
      options = list(
        dom = 't',                     # Only show the table, no controls
        paging = FALSE,                # Disable pagination
        ordering = FALSE,              # Disable column sorting
        searching = FALSE,             # Disable search filter
        autoWidth = TRUE,              # Auto-adjust column width
        scrollX = TRUE,                # Enable horizontal scrolling if necessary
        scrollY = "200px"              # Enable horizontal scrolling if necessary
      ),
      rownames = TRUE,                 # Keep rownames if needed
      class = "compact stripe hover"   # Add custom styling classes if desired
    )
  })
  
  
  output$table_centrality <- DT::renderDataTable({
    
    table_resultados_df<- resultados()$resultadosTable
    
    betweenness_summary <- table_resultados_df %>%
      select(betweenness_) %>% 
      summarise(
        Metric = "Betweenness",
        Mean = mean(betweenness_, na.rm = TRUE),
        Median = median(betweenness_, na.rm = TRUE),
        Range = paste(range(betweenness_, na.rm = TRUE), collapse = " - ")
      )
    
    closeness_out_summary <- table_resultados_df %>%
      select(closeness_out) %>%
      summarise(
        Metric = "Closeness Out",
        Mean = mean(closeness_out, na.rm = TRUE),
        Median = median(closeness_out, na.rm = TRUE),
        Range = paste(range(closeness_out, na.rm = TRUE), collapse = " - ")
      )
    
    closeness_in_summary <- table_resultados_df %>%
      select(closeness_in) %>%
      summarise(
        Metric = "Closeness In",
        Mean = mean(closeness_in, na.rm = TRUE),
        Median = median(closeness_in, na.rm = TRUE),
        Range = paste(range(closeness_in, na.rm = TRUE), collapse = " - ")
      )
    
    table_resultados_transposed <- bind_rows(
      betweenness_summary,
      closeness_in_summary,
      closeness_out_summary)
    
    DT::datatable(
      table_resultados_transposed, 
      options = list(
        dom = 't',                     # Only show the table, no controls
        paging = FALSE,                # Disable pagination
        ordering = FALSE,              # Disable column sorting
        searching = FALSE,             # Disable search filter
        autoWidth = TRUE,              # Auto-adjust column width
        scrollX = TRUE,                # Enable horizontal scrolling if necessary
        scrollY = "200px"              # Enable horizontal scrolling if necessary
      ),
      rownames = TRUE,                 # Keep rownames if needed
      class = "compact stripe hover"   # Add custom styling classes if desired
    )
  })
  
  
  output$resultadosTable <- DT::renderDataTable({
    DT::datatable(resultados()$resultadosTable, 
                  options = list(
                    pageLength = 20,                # Number of rows to show per page
                    lengthMenu = c(5, 10, 15),     # Options for the number of rows per page
                    scrollX = TRUE,                # Enable horizontal scrolling
                    scrollY = "200px",             # Set fixed height with vertical scrolling
                    searching = TRUE,              # Enable search functionality
                    ordering = TRUE,               # Enable column sorting
                    autoWidth = TRUE,              # Auto-adjust column widths
                    dom = 'ftip'                   # Control table elements (f=filter, t=table, i=info, p=pagination)
                  ),
                  class = "compact stripe hover",   # Additional styling
                  rownames = FALSE                  # Remove row names
    )
  })
  
  
  # Render Plots
  output$boxplotGrausVizinhos <- renderPlotly({
    plot_ly() %>%
      add_boxplot(y = ~resultados()$kin, name = "kin", boxpoints = "all", jitter = 0.3, pointpos = -1.8) %>%
      add_boxplot(y = ~resultados()$kout, name = "kout", boxpoints = "all", jitter = 0.3, pointpos = -1.8) %>%
      layout(title = "Grau de Vizinhos", yaxis = list(title = "Values"), xaxis = list(title = "Type"))
  })
  
  output$scatterGrausVizinhos <- renderPlotly({
    plot_ly(x = ~resultados()$kout, y = ~resultados()$kin, type = 'scatter', mode = 'markers') %>%
      layout(title = "Grau de Vizinhos", xaxis = list(title = "kout"), yaxis = list(title = "kin", range = c(0, 50)))
  })
  
  
  output$boxplotGrausAnimais <- renderPlotly({
    plot_ly() %>%
      add_boxplot(y = ~resultados()$kin_animais, name = "kin_animais", boxpoints = "all", jitter = 0.3, pointpos = -1.8) %>%
      add_boxplot(y = ~resultados()$kout_animais, name = "kout_animais", boxpoints = "all", jitter = 0.3, pointpos = -1.8) %>%
      layout(title = "Grau de Animais", yaxis = list(title = "Values", range = c(0, 20000)), xaxis = list(title = "Type"))
  })
  
  output$boxplotconctactchain<- renderPlotly({
    plot_ly() %>%
      add_boxplot(x = ~resultados()$ccc_ingoing, name = "Entrada", boxpoints = "all", jitter = 0.3, pointpos = -1.8) %>%
      add_boxplot(x = ~resultados()$ccc_outgoing, name = "Saída", boxpoints = "all", jitter = 0.3, pointpos = -1.8) %>%
      layout(title = "Cadeia de contato", yaxis = list(title = "Values", range = c(0, 200)), xaxis = list(title = "Type"))
  })
  
  output$scatterGrausAnimais <- renderPlotly({
    plot_ly(x = ~resultados()$kout_animais, y = ~resultados()$kin_animais, type = 'scatter', mode = 'markers') %>%
      layout(title = "Grau de Animais", xaxis = list(title = "kout_animais", range = c(0, 2000)), yaxis = list(title = "kin_animais", range = c(0, 50)))
  })
  
  
  
  # Render ValueBoxes
  output$network_density <- renderValueBox({
    valueBox(
      paste0(round(resultados()$network_density, 4)), "Network Density", 
      icon = icon("project-diagram")
    )
  })
  
  output$num_components <- renderValueBox({
    valueBox(
      paste0(resultados()$num_components), "Number of connected components", 
      icon = icon("project-diagram")
    )
  })
  
  # Contact Chain outputs ----
  cadeia <- eventReactive(input$update, {
    req(input$Estabelecimento, input$date_range)
    
    estabelecimento_object <- input$Estabelecimento
    estabelecimento_object <- as.numeric(estabelecimento_object)
    data.final <- input$date_range[2]
    dias <- as.numeric(difftime(data.final, input$date_range[1], units = "days"))
    
    tabela <- data.frame(
      source = as.character(data$cod_prop_origem),
      destination = as.character(data$cod_prop_destino),
      t = as.Date(data$data_emissao)
    )
    
    Trace(tabela, root = estabelecimento_object, tEnd = data.final, days = dias)
  })
  
  # Output for ingoing contacts
  output$ingoingContactsBox <- renderPrint({
    cadeia_data <- cadeia()
    ingoing_contacts <- cadeia_data@ingoingContacts
    
    # Capture the output of the show function
    capture.output(show(ingoing_contacts))
  })
  
  # Output for outgoing contacts
  output$outgoingContactsBox <- renderPrint({
    cadeia_data <- cadeia()
    outgoing_contacts <- cadeia_data@outgoingContacts
    
    # Capture the output of the show function
    capture.output(show(outgoing_contacts))
  })  
  
} 

  