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

# Define UI
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    tags$div(
      tags$img(src = "TrackMOOve.png", style = "width: 100%; padding: 10px; display: block; margin: auto;"),
      style = "text-align: center; padding: 10px;"
    ),
    sidebarMenu(
      menuItem("Sobre", tabName = "sobre", icon = icon("info-circle")),
      menuItem("Análise Descritiva", tabName = "descriptive_analysis", icon = icon("chart-simple")), 
      menuItem("Análise de redes", tabName = "network_analysis", icon = icon("circle-nodes")), 
      menuItem("Cadeia de contatos", tabName = "contact_chain", icon = icon("dashboard"))
      
    ),
    dateRangeInput("date_range", "Selecione o período", start = max(data$data_emissao), end = max(data$data_emissao))
  ),
  dashboardBody(
    includeCSS("www/custom.css"),  # Include the custom CSS
    tabItems(
      
      #Tab "Sobre"----
      tabItem(tabName = "sobre",
              fluidRow(
                column(width = 12,
                       tags$div(
                         id = "sobre-content",
                         tags$p("TrackMOOve é um dashboard feito para auxiliar na análise e interpretação do banco de dados de movimentação de animais em um estado brasileiro em um período determinado. A ferramenta tem o objetivo de possibilitar a epidemiologistas e tomadores de decisão informações acessíveis a respeito da movimentação de bovinos entre propriedades, incluindo a possibilidade de identificar as movimentações diretas e indiretas realizadas por e para propriedades que sejam alvo de investigações epidemiológicas. A ferramenta contém uma aba para análises descritivas da rede, uma aba para a análise de redes sociais e a aba de rastreio de propriedades."),
                         tags$br(),
                         tags$p("Abaixo pode-se encontrar a descrição dos parâmetros de análise de redes sociais utilizados na ferramenta."),
                         tags$h3("Grau"),
                         tags$p("O grau pode ser definido como o número de arestas conectadas a um nó. Uma rede direcionada apresenta grau de entrada (Kin) e grau de saída (Kout), que são, respectivamente, o número de arestas que têm destino no nó, e o número de arestas que têm origem no nó. O grau total (Ktotal) é determinado pela soma do grau de entrada e do grau de saída. Para a análise do grau as arestas podem ser definidas de 3 formas: grau de animais movimentados, que é determinado pela quantidade de bovinos que entraram e saíram do nó; grau de lotes, determinado pela quantidade de lotes que entraram e saíram do nó; e o grau de vizinhos que é determinado pela quantidade de estabelecimentos que um nó comercializou. O grau de um estabelecimento indica o quanto o mesmo está conectado na rede, demonstrando sua importância na movimentação de animais. Nesse trabalho foram determinados o grau de vizinhos, de lotes e de animais movimentados."),
                         tags$h3("Lei de potências"),
                         tags$p("A análise de redes permite identificar as características da rede em relação a sua estrutura. Redes podem ser classificadas como aleatórias, livres de escala ou redes de mundo pequeno. Geralmente, redes de trânsito animal possuem características livres de escala, pois apresentam poucos nós com muitas conexões e a maior parte dos nós com poucas conexões. As redes livres de escala são assim chamadas pois seguem uma distribuição de graus de acordo com a lei de potência. Dessa forma, avaliar se a rede analisada segue uma lei de potência é interessante para entender a sua estrutura e a disseminação de uma doença diante desse perfil."),
                         tags$h3("Coeficiente de Aglomeração"),
                         tags$p("O coeficiente de aglomeração (CA) também é conhecido como “clustering coefficient”. A medida representa a probabilidade de vizinhos de um nó serem também vizinhos entre si, avaliando a conectividade da rede. O coeficiente de aglomeração pode ser calculado levando em consideração as arestas que entram e as arestas que saem de um nó, dessa forma é possível calcular um CA de entrada e um CA de saída."),
                         tags$h3("Betweenness e Closeness"),
                         tags$p("A centralidade de intermediação também chamada de “betweenness” indica a proporção de geodésicas que incluem um nó de referência. Essa medida avalia a importância de uma propriedade em ligar outras duas propriedades ou dois grupos diferentes de propriedades. Um nó com alto betweenness tem grande influência no fluxo de animais, pois é provável que muitas movimentações passem por esse estabelecimento. A centralidade de proximidade, cujo nome em inglês é “closeness”, indica o quanto um determinado nó está próximo de outros nós na rede. Na rede de movimentação de animais, é possível determinar o closeness de entrada e de saída. Quanto maior o closeness menos passos são necessários para uma propriedade comprar ou vender animais para outras propriedades."),
                         tags$h3("Cadeia de contatos"),
                         tags$p("A Cadeia de Contatos de Entrada e a Cadeia de Contatos de Saída indicam as conexões (diretas ou indiretas) de um nó, levando em conta as datas das movimentações, indicando o caminho possível de um animal ou infecção. A Cadeia de Contatos de Entrada indica todas as conexões indiretas que um nó recebe, enquanto a Cadeia de Contatos de Saída (do inglês Outgoing Contact Chain, ou OCC) indica todas as conexões indiretas que um nó envia.")
                       )
                ))), 
      
      #Tab "Analise descritiva da rede"----
      tabItem(tabName = "descriptive_analysis",
              fluidRow(
                column(width = 3, box(title = "Estabelecimentos", width = NULL, height = 180, valueBoxOutput("propriedades", width = 100))),
                column(width = 3, box(title = "Animais", width = NULL, height = 180, valueBoxOutput("animais", width = 80))),
                column(width = 3, box(title = "Abatedouros", width = NULL, height = 180, valueBoxOutput("abatedouros", width = 100))), 
                column(width = 3, box(title = "Movimentaçöes", width = NULL, height = 180, valueBoxOutput("movimentacoes", width = 100)))
                ),
              fluidRow(
                column(width = 6,
                       box(
                         status = "primary",
                         solidHeader = FALSE,
                         width = 12,
                         tabBox(
                           id = "tabset1",
                           width = 12,
                           tabPanel("Lotes movimentados por finalidade", plotlyOutput("chart1", width = "100%", height = "300px")),
                           tabPanel("Animais movimentados por finalidade", plotlyOutput("chart2", width = "100%", height = "300px"))
                         )
                       )
                ),
                column(width = 6,
                       box(
                         status = "primary",
                         solidHeader = FALSE,
                         width = 12,
                         tabBox(
                           id = "tabset2",
                           width = 12,
                           tabPanel("Lotes movimentados no período", plotlyOutput("chart3", width = "100%", height = "300px")),
                           tabPanel("Animais movimentados no período", plotlyOutput("chart4", width = "100%", height = "300px"))
                         )
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         status = "primary",
                         solidHeader = FALSE,
                         width = 12,
                         tabBox(
                           id = "tabset2",
                           width = 12,
                           tabPanel("Cidades que mais receberam animais", plotlyOutput("chart7", width = "100%", height = "300px")),
                           tabPanel("Cidades que mais enviaram animais", plotlyOutput("chart6", width = "100%", height = "300px"))
                         )
                       )
                ), 
                
                column(width = 6, box(title = "Animais recebidos de acordo com tipo de inspeçao", width = 12, 
                                      plotlyOutput("chart5", width = "100%", height = "300px")))
              )
      ), 
      
      #Tab "Network analysis"----
      
      tabItem(tabName = "network_analysis",
              fluidRow(
                column(width = 6,
                       box(
                         status = "primary",
                         solidHeader = FALSE,
                         width = 12,
                         tabBox(
                           id = "tabset1",
                           width = 12,
                           tabPanel("Boxplot de grau de vizinhos", plotlyOutput("boxplotGrausVizinhos", width = "100%", height = "300px")),
                           tabPanel("Boxplot de grau de animais", plotlyOutput("boxplotGrausAnimais", width = "100%", height = "300px"))
                         )
                       )
                ),
                column(width = 6,
                       box(
                         status = "primary",
                         solidHeader = FALSE,
                         width = 12,
                         tabBox(
                           id = "tabset2",
                           width = 12,
                           tabPanel("Scatterplot de grau de vizinhos ", plotlyOutput("scatterGrausVizinhos", width = "100%", height = "300px")),
                           tabPanel("Scatterplot de grau de Animais", plotlyOutput("scatterGrausAnimais"), width = "100%", height = "300px"))
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         status = "primary",
                         solidHeader = FALSE,
                         width = 12,
                         plotlyOutput("boxplotconctactchain", width = "100%", height = "300px"))
                ),
                
                column(width = 6,
                       box( status = "primary", solidHeader = FALSE, width = 12, 
                            tabBox(
                              id = "tabset3",
                              width = 12,
                              tabPanel("Medidas da rede", DT::dataTableOutput("table_metrics"), width = "100%", height = "400px"), 
                              tabPanel("Medidas de centralidade", DT::dataTableOutput("table_centrality"), width = "100%", height = "400px")
                            )))),
              
              
              fluidRow(
                box(title = "Resultados por nós", 
                    status = "primary", solidHeader = FALSE,
                    width = 12, height = 380,   # Adjust the width and height as needed
                    DT::dataTableOutput("resultadosTable")
                )
              )
      ), 
      
      #Tab track premise ----
      
      tabItem(tabName = "contact_chain",
              fluidPage(
                titlePanel(tags$h1("Cadeia de contatos", style = "color: #1C4E80;") ),
                sidebarLayout(
                  sidebarPanel(
                    tags$label("Código do estabelecimento", style = "color: #1C4E80;"),
                    textInput("Estabelecimento", label = NULL, value = "51000008386"),
                    actionButton("update", "Atualizar a análise")
                  ),
                  mainPanel(
                    
                    fluidRow(
                      box(title = "Contatos recebidos", status = "primary", solidHeader = TRUE, width = 12,
                          verbatimTextOutput("ingoingContactsBox")),
                      box(title = "Contatos enviados", status = "danger", solidHeader = TRUE, width = 12,
                          verbatimTextOutput("outgoingContactsBox"))
                    )
                  )
                )
              )
      )
      
      
    )
  )
)
