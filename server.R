# Server logic for TrackMOOve.
# Libraries and the data set are loaded in global.R.

server <- function(input, output, session) {

  # Cache filtered data for better performance
  filtered_data_cache <- reactiveVal()

  # Update date range based on available dates in the dataset
  observe({
    updateDateRangeInput(session, "date_range",
                         min = min(data$issue_date), max = max(data$issue_date))
  })

  # Efficiently filter data using data.table and cache the result
  observe({
    req(input$date_range)
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    filtered_data <- data[issue_date >= start_date & issue_date <= end_date]
    filtered_data_cache(filtered_data)
  })

  # Descriptive Analysis outputs ---------

  output$animais <- renderValueBox({
    filtered_data_df <- filtered_data_cache()

    animals <- filtered_data_df %>%
      group_by(purpose, origin_farm_id, destination_id, inspection_type, issue_date) %>%
      summarise(
        lot_count = n(),
        animal_count = sum(as.numeric(head_count))
      ) %>%
      ungroup() %>%
      mutate(animal_count = as.numeric(animal_count)) %>%
      summarise(animal_count = sum(animal_count, na.rm = TRUE))

    valueBox(
      paste0(animals$animal_count), "Número de animais movimentados na rede", icon = icon("cow"),
      color = "light-blue"
    )
  })

  output$movimentacoes <- renderValueBox({
    filtered_data_df <- filtered_data_cache()

    movements <- filtered_data_df %>%
      summarise(lot_count = n())

    valueBox(
      paste0(movements$lot_count), "Número de lotes movimentados na rede", icon = icon("truck"),
      color = "light-blue"
    )
  })

  output$abatedouros <- renderValueBox({
    filtered_data_df <- filtered_data_cache()

    abattoirs <- filtered_data_df %>%
      select(destination_id, inspection_type) %>%
      filter(inspection_type %in% c("SIM", "SIE", "SIF")) %>%
      distinct(destination_id) %>%
      summarise(count = n())

    valueBox(
      paste0(abattoirs$count), "Número de abatedouros na rede", icon = icon("drumstick-bite"),
      color = "light-blue"
    )
  })

  output$propriedades <- renderValueBox({
    filtered_data_df <- filtered_data_cache()

    abattoir_ids <- filtered_data_df %>%
      select(destination_id, inspection_type) %>%
      filter(inspection_type %in% c("SIM", "SIE", "SIF")) %>%
      distinct(destination_id)

    farms <- filtered_data_df %>%
      select(destination_id, inspection_type) %>%
      filter(!(inspection_type %in% c("SIM", "SIE", "SIF"))) %>%
      select(-inspection_type) %>%
      bind_rows(.,
                filtered_data_df %>% select(origin_farm_id)
      ) %>%
      pivot_longer(cols = c(origin_farm_id, destination_id),
                   values_to = "farm_id") %>%
      select(-name) %>%
      filter(!is.na(farm_id)) %>%
      distinct() %>%
      filter(!farm_id %in% abattoir_ids$destination_id) %>%
      summarise(count = n())

    valueBox(
      paste0(farms$count), "Número de estabelecimentos na rede", icon = icon("house-chimney-window"),
      color = "light-blue"
    )
  })

  output$chart1 <- renderPlotly({
    filtered_data_df <- filtered_data_cache()

    purpose_count <- filtered_data_df %>%
      count(purpose) %>%
      arrange(desc(n)) %>%
      mutate(purpose_factor = factor(purpose, levels = purpose),
             color = global_palette[as.numeric(factor(purpose, levels = unique(purpose)))])

    plot_ly(
      data = purpose_count,
      x = ~n,
      y = ~purpose_factor,
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
    data_animals <- filtered_data_df %>%
      group_by(purpose, origin_farm_id, destination_id, inspection_type, issue_date) %>%
      summarise(
        lot_count = n(),
        animal_count = sum(as.numeric(head_count))
      ) %>%
      ungroup() %>%
      group_by(purpose) %>%
      summarise(total_animals = sum(animal_count, na.rm = TRUE)) %>%
      arrange(desc(total_animals)) %>%
      mutate(purpose = factor(purpose, levels = purpose),
             color = global_palette[as.numeric(factor(purpose, levels = unique(purpose)))])

    # Create the Plotly chart
    plot_ly(
      data = data_animals,
      x = ~total_animals,
      y = ~purpose,
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

    # Convert 'issue_date' to Date class
    filtered_data_df <- filtered_data_df %>%
      mutate(issue_date = as.Date(issue_date))

    # Calculate the duration of the selected date range
    start_date <- as.Date(input$date_range[1], format = "%Y-%m-%d")
    end_date <- as.Date(input$date_range[2], format = "%Y-%m-%d")
    duration_days <- as.numeric(difftime(end_date, start_date, units = "days"))

    # Determine the aggregation level
    if (duration_days <= 2) {
      # Directly use the dates as categories for the X-axis
      aggregated_data <- filtered_data_df %>%
        group_by(issue_date) %>%
        summarise(
          lot_count = n(),
          animal_count = sum(as.numeric(head_count))
        ) %>%
        ungroup()

      avg_value <- mean(aggregated_data$lot_count, na.rm = TRUE)

      # Set X-axis as a discrete category
      plot_ly(
        data = aggregated_data,
        x = ~factor(issue_date),  # Treat dates as discrete categories
        y = ~lot_count,
        type = 'bar',
        marker = list(color = 'skyblue')
      ) %>%
        add_trace(
          x = ~factor(issue_date),
          y = rep(avg_value, length(aggregated_data$issue_date)),
          type = "scatter",
          mode = "lines",
          line = list(color = "orange", dash = "dash"),
          name = "Average"
        ) %>%
        layout(
          title = list(text = 'Numero de movimentaçoes por dia', font = list(color = '#063376', size = 12)),
          xaxis = list(title = 'Day', tickangle = 45),
          yaxis = list(title = 'Quantidade'),
          margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4),
          autosize = TRUE,
          showlegend = FALSE  # Hides legend for the entire plot
        ) %>%
        config(displayModeBar = FALSE)

    } else if (duration_days > 30) {
      # Aggregate by month
      aggregated_data <- filtered_data_df %>%
        mutate(year_month = floor_date(issue_date, "month")) %>%
        group_by(year_month) %>%
        summarise(
          lot_count = n(),
          animal_count = sum(as.numeric(head_count))
        ) %>%
        ungroup()

      avg_value <- mean(aggregated_data$lot_count, na.rm = TRUE)

      plot_ly(
        data = aggregated_data,
        x = ~year_month,
        y = ~lot_count,
        type = 'bar',
        marker = list(color = 'skyblue')
      ) %>%
        add_trace(
          x = ~year_month,
          y = rep(avg_value, length(aggregated_data$year_month)),
          type = "scatter",
          mode = "lines",
          line = list(color = "orange", dash = "dash"),
          name = "Average"
        ) %>%
        layout(
          title = list(text = 'Numero de movimentaçoes por mês', font = list(color = '#063376', size = 12)),
          xaxis = list(title = 'Month', type = 'date', tickformat = "%b %Y", tickangle = 45),
          yaxis = list(title = 'Quantidade'),
          margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4),
          autosize = TRUE,
          showlegend = FALSE  # Hides legend for the entire plot
        ) %>%
        config(displayModeBar = FALSE)

    } else {
      # Aggregate by week
      aggregated_data <- filtered_data_df %>%
        mutate(year_week = floor_date(issue_date, "week")) %>%
        group_by(year_week) %>%
        summarise(
          lot_count = n(),
          animal_count = sum(as.numeric(head_count))
        ) %>%
        ungroup()

      avg_value <- mean(aggregated_data$lot_count, na.rm = TRUE)

      plot_ly(
        data = aggregated_data,
        x = ~year_week,
        y = ~lot_count,
        type = 'bar',
        marker = list(color = 'skyblue')
      ) %>%
        add_trace(
          x = ~year_week,
          y = rep(avg_value, length(aggregated_data$year_week)),
          type = "scatter",
          mode = "lines",
          line = list(color = "orange", dash = "dash"),
          name = "Average"
        ) %>%
        layout(
          title = list(text = 'Numero de lotes movimentados por semana', font = list(color = '#063376', size = 12)),
          xaxis = list(title = 'Week', type = 'date', tickformat = "%d %b %Y", tickangle = 45),
          yaxis = list(title = 'Quantidade'),
          margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4),
          autosize = TRUE,
          showlegend = FALSE  # Hides legend for the entire plot
        ) %>%
        config(displayModeBar = FALSE)
    }
  })

  output$chart4 <- renderPlotly({
    filtered_data_df <- filtered_data_cache()

    # Convert 'issue_date' to Date class
    filtered_data_df <- filtered_data_df %>%
      mutate(issue_date = as.Date(issue_date))

    # Calculate the duration of the selected date range
    start_date <- as.Date(input$date_range[1], format = "%Y-%m-%d")
    end_date <- as.Date(input$date_range[2], format = "%Y-%m-%d")
    duration_days <- as.numeric(difftime(end_date, start_date, units = "days"))

    # Determine the aggregation level
    if (duration_days <= 2) {
      # Directly use the dates as categories for the X-axis
      aggregated_data <- filtered_data_df %>%
        group_by(issue_date) %>%
        summarise(
          lot_count = n(),
          animal_count = sum(as.numeric(head_count))
        ) %>%
        ungroup()

      avg_value <- mean(aggregated_data$animal_count, na.rm = TRUE)

      # Set X-axis as a discrete category
      plot_ly(
        data = aggregated_data,
        x = ~factor(issue_date),  # Treat dates as discrete categories
        y = ~animal_count,
        type = 'bar',
        marker = list(color = 'skyblue')
      ) %>%
        add_trace(
          x = ~factor(issue_date),
          y = rep(avg_value, length(aggregated_data$issue_date)),
          type = "scatter",
          mode = "lines",
          line = list(color = "orange", dash = "dash"),
          showlegend = FALSE  # Hides legend for this trace
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
        mutate(year_month = floor_date(issue_date, "month")) %>%
        group_by(year_month) %>%
        summarise(
          lot_count = n(),
          animal_count = sum(as.numeric(head_count))
        ) %>%
        ungroup()

      avg_value <- mean(aggregated_data$animal_count, na.rm = TRUE)

      plot_ly(
        data = aggregated_data,
        x = ~year_month,
        y = ~animal_count,
        type = 'bar',
        marker = list(color = 'skyblue')
      ) %>%
        add_trace(
          x = ~year_month,
          y = rep(avg_value, length(aggregated_data$year_month)),
          type = "scatter",
          mode = "lines",
          line = list(color = "orange", dash = "dash"),
          showlegend = FALSE  # Hides legend for this trace
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
        mutate(year_week = floor_date(issue_date, "week")) %>%
        group_by(year_week) %>%
        summarise(
          lot_count = n(),
          animal_count = sum(as.numeric(head_count))
        ) %>%
        ungroup()

      avg_value <- mean(aggregated_data$animal_count, na.rm = TRUE)

      plot_ly(
        data = aggregated_data,
        x = ~year_week,
        y = ~animal_count,
        type = 'bar',
        marker = list(color = 'skyblue')
      ) %>%
        add_trace(
          x = ~year_week,
          y = rep(avg_value, length(aggregated_data$year_week)),
          type = "scatter",
          mode = "lines",
          line = list(color = "orange", dash = "dash"),
          showlegend = FALSE  # Hides legend for this trace
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
      group_by(purpose, origin_farm_id, destination_id, inspection_type, issue_date) %>%
      summarise(
        lot_count = n(),
        animal_count = sum(as.numeric(head_count))
      ) %>%
      ungroup() %>%
      filter(inspection_type %in% c("SIF", "SIE", "SIM")) %>%
      group_by(inspection_type) %>%
      summarise(total_animals = sum(animal_count, na.rm = TRUE))

    plot_ly(
      data = aggregated_data,
      labels = ~inspection_type,
      values = ~total_animals,
      type = 'pie',
      textinfo = 'label+percent',
      marker = list(colors = c('#063376', '#C3B1E1', 'darkorange'))  # Example colors for each slice
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
      group_by(origin_district) %>%
      summarise(
        lot_count = n(),
        animal_count = sum(as.numeric(head_count))
      ) %>%
      ungroup() %>%
      arrange(desc(animal_count)) %>%
      slice_head(n = 10) %>%
      mutate(origin_district = factor(origin_district, levels = origin_district))  # Reorder factor levels

    plot_ly(
      data = aggregated_data,
      x = ~origin_district,
      y = ~animal_count,
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
      group_by(destination_district) %>%
      summarise(
        lot_count = n(),
        animal_count = sum(as.numeric(head_count))
      ) %>%
      ungroup() %>%
      arrange(desc(animal_count)) %>%
      slice_head(n = 10) %>%
      mutate(destination_district = factor(destination_district, levels = destination_district))  # Reorder factor levels

    plot_ly(
      data = aggregated_data,
      x = ~destination_district,
      y = ~animal_count,
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
  network_results <- reactive({
    filtered_data_df <- filtered_data_cache()

    # Prepare data for network analysis
    farms_and_abattoirs <- filtered_data_df %>%
      group_by(purpose, origin_farm_id, destination_id, inspection_type, issue_date) %>%
      summarise(
        lot_count = n(),
        animal_count = sum(as.numeric(head_count))
      ) %>%
      ungroup() %>%
      mutate(issue_date = as.Date(issue_date, format = "%Y-%m-%d")) %>%
      mutate(origin_farm_id = as.numeric(origin_farm_id),
             destination_id = as.numeric(destination_id)) %>%
      as.data.frame()

    node_map <- createUniqueIds(farms_and_abattoirs,
                                from = 'origin_farm_id',
                                to = 'destination_id')

    neighbour_matrix <- sparseMatrix(i = node_map$movements$From, j = node_map$movements$To,
                                     dims = rep(max(node_map$movements$From, node_map$movements$To), 2))

    lot_matrix <- sparseMatrix(i = node_map$movements$From, j = node_map$movements$To,
                               x = node_map$movements$lot_count,
                               dims = rep(max(node_map$movements$From, node_map$movements$To), 2))

    animal_matrix <- sparseMatrix(i = node_map$movements$From, j = node_map$movements$To,
                                  x = node_map$movements$animal_count,
                                  dims = rep(max(node_map$movements$From, node_map$movements$To), 2))

    #Node centrality measures
    kin <- colSums(neighbour_matrix)
    kout <- rowSums(neighbour_matrix)
    ktotal <- kin + kout

    undirected_matrix <- neighbour_matrix + t(neighbour_matrix)
    undirected_neighbour_matrix <- ((undirected_matrix > 0) * 1)
    kneighbors <- rowSums(undirected_neighbour_matrix)

    kin_animais <- colSums(animal_matrix)
    kout_animais <- rowSums(animal_matrix)
    ktotal_animais <- kin_animais + kout_animais

    kin_lotes <- colSums(lot_matrix)
    kout_lotes <- rowSums(lot_matrix)
    ktotal_lotes <- kin_lotes + kout_lotes

    edge_list <- unique(as.matrix(node_map$movements[, c('From', 'To')]))
    movement_graph <- graph_from_edgelist(el = edge_list, directed = TRUE)

    betweenness_values <- betweenness(graph = movement_graph)

    closeness_in <- closeness(graph = movement_graph, mode = 'in')

    closeness_out <- closeness(graph = movement_graph, mode = 'out')

    contact_chain_stats <- calculateContactChain(Data = farms_and_abattoirs,
                                                 from = 'origin_farm_id',
                                                 to = 'destination_id',
                                                 Time = 'issue_date', simultaneous = T)


    #Network metrics

    network_density <- edge_density(movement_graph)

    connected_components <- components(movement_graph)
    num_components <- connected_components$no

    network_diameter <- diameter(movement_graph)
    average_shortest_path <- mean_distance(movement_graph, directed = TRUE)
    reciprocity_network <- reciprocity(movement_graph)

    #Making tables

    network_metrics <- data.frame(network_density, num_components, network_diameter,
                                  average_shortest_path, reciprocity_network)

    results_table <- data.frame(node_map$correspondence$database.id, kin, kout, kneighbors,
                                kin_animais, kout_animais, ktotal_animais,
                                kin_lotes, kout_lotes, ktotal_lotes,
                                betweenness_values, closeness_in, closeness_out,
                                contact_chain_stats$ingoing, contact_chain_stats$outgoing)

    list(
      results_table = results_table,
      network_metrics = network_metrics,
      kin = kin,
      kout = kout,
      kneighbors = kneighbors,
      kin_animais = kin_animais,
      kout_animais = kout_animais,
      ktotal_animais = ktotal_animais,
      kin_lotes = kin_lotes,
      kout_lotes = kout_lotes,
      ktotal_lotes = ktotal_lotes,
      betweenness_values = betweenness_values,
      closeness_in = closeness_in,
      closeness_out = closeness_out,
      ingoing_contact_chain = contact_chain_stats$ingoing,
      outgoing_contact_chain = contact_chain_stats$outgoing
    )

  })


  # Render DataTable
  output$table_metrics <- DT::renderDataTable({

    table_metrics_df <- network_results()$network_metrics

    colnames(table_metrics_df) <- c(
      "Densidade da rede",
      "Numero de componentes conectados",
      "Diametro da rede",
      "Comprimento medio do caminho",
      "Reciprocidade da rede"
    )


    table_metrics <- pivot_longer(table_metrics_df,
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

    results_df <- network_results()$results_table

    betweenness_summary <- results_df %>%
      select(betweenness_values) %>%
      summarise(
        Metric = "Betweenness",
        Mean = mean(betweenness_values, na.rm = TRUE),
        Median = median(betweenness_values, na.rm = TRUE),
        Range = paste(range(betweenness_values, na.rm = TRUE), collapse = " - ")
      )

    closeness_out_summary <- results_df %>%
      select(closeness_out) %>%
      summarise(
        Metric = "Closeness Out",
        Mean = mean(closeness_out, na.rm = TRUE),
        Median = median(closeness_out, na.rm = TRUE),
        Range = paste(range(closeness_out, na.rm = TRUE), collapse = " - ")
      )

    closeness_in_summary <- results_df %>%
      select(closeness_in) %>%
      summarise(
        Metric = "Closeness In",
        Mean = mean(closeness_in, na.rm = TRUE),
        Median = median(closeness_in, na.rm = TRUE),
        Range = paste(range(closeness_in, na.rm = TRUE), collapse = " - ")
      )

    kin_summary <- results_df %>%
      select(kin) %>%
      summarise(
        Metric = "K in",
        Mean = mean(kin, na.rm = TRUE),
        Median = median(kin, na.rm = TRUE),
        Range = paste(range(kin, na.rm = TRUE), collapse = " - ")
      )

    kout_summary <- results_df %>%
      select(kout) %>%
      summarise(
        Metric = "K out",
        Mean = mean(kout, na.rm = TRUE),
        Median = median(kout, na.rm = TRUE),
        Range = paste(range(kout, na.rm = TRUE), collapse = " - ")
      )

    kneighbors_summary <- results_df %>%
      select(kneighbors) %>%
      summarise(
        Metric = "K vizinhos",
        Mean = mean(kneighbors, na.rm = TRUE),
        Median = median(kneighbors, na.rm = TRUE),
        Range = paste(range(kneighbors, na.rm = TRUE), collapse = " - ")
      )

    kin_animais_summary <- results_df %>%
      select(kin_animais) %>%
      summarise(
        Metric = "K in animais",
        Mean = mean(kin_animais, na.rm = TRUE),
        Median = median(kin_animais, na.rm = TRUE),
        Range = paste(range(kin_animais, na.rm = TRUE), collapse = " - ")
      )

    kout_animais_summary <- results_df %>%
      select(kout_animais) %>%
      summarise(
        Metric = "K out animais",
        Mean = mean(kout_animais, na.rm = TRUE),
        Median = median(kout_animais, na.rm = TRUE),
        Range = paste(range(kout_animais, na.rm = TRUE), collapse = " - ")
      )

    centrality_table <- bind_rows(
      kin_summary,
      kout_summary,
      kneighbors_summary,
      kin_animais_summary,
      kout_animais_summary,
      betweenness_summary,
      closeness_in_summary,
      closeness_out_summary)

    DT::datatable(
      centrality_table,
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
    DT::datatable(network_results()$results_table,
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
  # Axis ranges are left automatic so hubs and outliers are not clipped.

  output$scatterGrausVizinhos <- renderPlotly({
    plot_ly(x = ~network_results()$kout, y = ~network_results()$kin, type = 'scatter', mode = 'markers') %>%
      layout(title = "Grau de Vizinhos", xaxis = list(title = "kout"), yaxis = list(title = "kin"))
  })


  output$boxplotconctactchain <- renderPlotly({
    plot_ly() %>%
      add_boxplot(x = ~network_results()$ingoing_contact_chain, name = "Entrada", boxpoints = "all", jitter = 0.3, pointpos = -1.8) %>%
      add_boxplot(x = ~network_results()$outgoing_contact_chain, name = "Saída", boxpoints = "all", jitter = 0.3, pointpos = -1.8) %>%
      layout(title = "Cadeia de contato", yaxis = list(title = "Values"), xaxis = list(title = "Type"))
  })

  output$scatterGrausAnimais <- renderPlotly({
    plot_ly(x = ~network_results()$kout_animais, y = ~network_results()$kin_animais, type = 'scatter', mode = 'markers') %>%
      layout(title = "Grau de Animais", xaxis = list(title = "kout_animais"), yaxis = list(title = "kin_animais"))
  })


  # Contact Chain outputs ----
  contact_chain_result <- eventReactive(input$update, {
    req(input$Estabelecimento, input$date_range)

    farm_code <- as.numeric(input$Estabelecimento)
    end_date <- input$date_range[2]
    days <- as.numeric(difftime(end_date, input$date_range[1], units = "days"))

    contact_table <- data.frame(
      source = as.character(data$origin_farm_id),
      destination = as.character(data$destination_id),
      t = as.Date(data$issue_date)
    )

    Trace(contact_table, root = farm_code, tEnd = end_date, days = days)
  })

  # Output for ingoing contacts
  output$ingoingContactsBox <- renderPrint({
    trace_data <- contact_chain_result()
    ingoing_contacts <- trace_data@ingoingContacts

    # Capture the output of the show function
    capture.output(show(ingoing_contacts))
  })

  # Output for outgoing contacts
  output$outgoingContactsBox <- renderPrint({
    trace_data <- contact_chain_result()
    outgoing_contacts <- trace_data@outgoingContacts

    # Capture the output of the show function
    capture.output(show(outgoing_contacts))
  })

}
