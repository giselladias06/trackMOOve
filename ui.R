# UI definition for TrackMOOve.
# Libraries and the data set are loaded in global.R.

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
    dateRangeInput("date_range", "Selecione o período",
                   start = max(data$issue_date), end = max(data$issue_date))
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
                         tags$p("O TrackMOOve é um dashboard para análise da movimentação de bovinos entre estabelecimentos. Ele apoia epidemiologistas e tomadores de decisão na vigilância de doenças, permitindo identificar estabelecimentos centrais na rede e rastrear os contatos diretos e indiretos de propriedades investigadas."),
                         tags$p(tags$b("Dados desta versão: "), "os dados exibidos são simulados e correspondem ao país fictício Sentara, com movimentações de janeiro a junho de 2024. Eles não representam animais, propriedades ou eventos reais."),
                         tags$h3("Como usar"),
                         tags$ul(
                           tags$li("Use o filtro \"Selecione o período\", na barra lateral, para limitar as movimentações analisadas. Todas as abas são atualizadas."),
                           tags$li(tags$b("Análise Descritiva:"), " resumo do período, com número de estabelecimentos, animais, lotes e abatedouros, além da distribuição por finalidade, ao longo do tempo e pelas cidades com maior envio e recebimento de animais."),
                           tags$li(tags$b("Análise de redes:"), " métricas de coesão da rede e de centralidade dos nós, gráficos de graus e a tabela de resultados por nó."),
                           tags$li(tags$b("Cadeia de contatos:"), " informe o código de um estabelecimento e clique em \"Atualizar a análise\" para ver os contatos recebidos e enviados.")
                         ),
                         tags$br(),
                         tags$p("Os parâmetros de análise de redes utilizados na ferramenta são descritos a seguir."),
                         tags$h3("Medidas de centralidade de nós"),
                         tags$p(tags$b("Grau. "), "O grau de um nó é o número de arestas ligadas a ele, ou seja, de movimentações das quais ele participa. Em uma rede direcionada, o grau de entrada (Kin) conta as movimentações que chegam ao nó, e o grau de saída (Kout) conta as que saem dele. O grau total (Ktotal) é a soma dos dois. A ferramenta calcula o grau de três formas: pela quantidade de animais movimentados, pela quantidade de lotes e pelo número de vizinhos, ou seja, de estabelecimentos com os quais o nó teve movimentações, de entrada ou de saída."),
                         tags$p(tags$b("Betweenness e closeness. "), "A centralidade de intermediação, também chamada de “betweenness”, indica a proporção de caminhos mais curtos da rede que passam por um nó de referência. Essa medida avalia a importância de um estabelecimento para ligar outros dois estabelecimentos ou dois grupos diferentes deles. Um nó com alto betweenness tem grande influência no fluxo de animais, pois é provável que muitas movimentações passem por ele. A centralidade de proximidade, ou “closeness”, indica o quanto um nó está próximo dos outros nós da rede. A ferramenta calcula o closeness de entrada e de saída. Quanto maior o closeness, menos passos são necessários para que um estabelecimento compre ou venda animais para os demais."),
                         tags$p(tags$b("Cadeia de contatos. "), "A Cadeia de Contatos de Entrada e a Cadeia de Contatos de Saída indicam as conexões (diretas ou indiretas) de um nó, levando em conta as datas das movimentações e o caminho possível de um animal ou de uma infecção. A Cadeia de Contatos de Entrada indica todas as conexões indiretas que um nó recebe, enquanto a Cadeia de Contatos de Saída (do inglês Outgoing Contact Chain, ou OCC) indica todas as conexões indiretas que um nó envia."),
                         tags$h3("Medidas de coesão da rede"),
                         tags$ul(
                           tags$li(tags$b("Densidade:"), " proporção das ligações existentes entre todos os pares possíveis de estabelecimentos. Valores próximos de zero indicam que poucos estabelecimentos trocam animais entre si."),
                           tags$li(tags$b("Componentes conectados:"), " número de subgrupos de estabelecimentos que se movimentam entre si, mas não têm ligação com os demais."),
                           tags$li(tags$b("Diâmetro:"), " maior distância entre dois estabelecimentos da rede, medida pelo menor número de movimentações necessárias para ir de um ao outro."),
                           tags$li(tags$b("Comprimento médio do caminho:"), " média das menores distâncias entre os pares de estabelecimentos que estão conectados."),
                           tags$li(tags$b("Reciprocidade:"), " proporção das ligações em que também existe a ligação no sentido contrário, ou seja, com que frequência dois estabelecimentos trocam animais nos dois sentidos.")
                         ),
                         tags$h3("Limitações"),
                         tags$ul(
                           tags$li("Os dados são estáticos e não são atualizados em tempo real, o que limita o uso imediato em emergências."),
                           tags$li("O desempenho depende da capacidade do computador, já que os cálculos são feitos com base no volume de dados carregado."),
                           tags$li("A ferramenta não detecta comunidades da rede automaticamente.")
                         ),
                         tags$h3("Versão e licença"),
                         tags$p("Versão: [preencher]. Licença: [preencher]. Como citar: [preencher]. Contato: [preencher].")
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
                         tabsetPanel(
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
                         tabsetPanel(
                           tabPanel("Lotes movimentados no período", plotlyOutput("chart3", width = "100%", height = "300px")),
                           tabPanel("Animais movimentados no período", plotlyOutput("chart4", width = "100%", height = "300px"))
                         )
                       )
                )
              ),
              fluidRow(
                  column(6, box(status = "primary", solidHeader = F, width = 12,
                                tabsetPanel(
                                  tabPanel("Cidades que mais receberam animais", plotlyOutput("chart7", width = "100%", height = "300px")),
                                  tabPanel("Cidades que mais enviaram animais", plotlyOutput("chart6", width = "100%", height = "300px"))
                                )
                  )),

                column(width = 6, box(title = "Animais recebidos de acordo com tipo de inspeçao", width = 12,
                                      plotlyOutput("chart5", width = "100%", height = "300px")))
              )
      ),

      #Tab "Network analysis"----

      tabItem(tabName = "network_analysis",
              fluidRow(
                column(6,
                       box(status = "primary", solidHeader = FALSE, width = 12, height = "100%",
                           title = "Medidas de coesão da rede",
                           div(style = "width: 100%;", DT::dataTableOutput("table_metrics"))
                       )
                ),
                column(6,
                       box(status = "primary", solidHeader = FALSE, width = 12, height = "100%",
                           title = "Medidas de centralidade de nós",
                           div(style = "width: 100%;", DT::dataTableOutput("table_centrality"))
                       )
                ))

              ,
              fluidRow(
                column(6, box(status = "primary", solidHeader = F, width = 12,
                              plotlyOutput("boxplotconctactchain", height = "300px"))
                ),
                column(6, box(status = "primary", solidHeader = F, width = 12,
                              tabsetPanel(
                                tabPanel("Grau de Vizinhos", plotlyOutput("scatterGrausVizinhos", height = "300px")),
                                tabPanel("Grau de Animais", plotlyOutput("scatterGrausAnimais", height = "300px"))
                              ))
                )
              ),

              fluidRow(
                column(12,
                       box(
                         title = "Resultados por nós",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         collapsible = TRUE,  # Enables collapsing
                         collapsed = TRUE,    # Starts collapsed
                         DT::dataTableOutput("resultadosTable")
                       )
                )
              )),


      #Tab track premise ----

      tabItem(tabName = "contact_chain",
              fluidPage(
                titlePanel(tags$h1("Cadeia de contatos", style = "color: #1C4E80;") ),
                sidebarLayout(
                  sidebarPanel(
                    tags$label("Código do estabelecimento", style = "color: #1C4E80;"),
                    textInput("Estabelecimento", label = NULL, value = "130"),
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
  ))
