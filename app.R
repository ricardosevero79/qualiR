# =============================================================================
#  QualiR — análise qualitativa colaborativa em R
#  Execute:  shiny::runApp("caminho/para/qualiR")
#  Equipe:   shiny::runApp("qualiR", host = "0.0.0.0", port = 3838)
#  Projetos: ficam na pasta "projetos" (um arquivo .sqlite por projeto).
#            Para usar outra pasta: Sys.setenv(QUALIR_PROJETOS = "C:/Pesquisas")
# =============================================================================
library(shiny)

options(shiny.maxRequestSize = 200 * 1024^2)   # arquivos de até 200 MB

migrar_projeto_antigo()                # versões anteriores: move ./projeto_qualir.sqlite para ./projetos
qr_reg <- registro_conexoes()          # uma conexão por projeto, compartilhada entre as sessões
onStop(function() qr_reg$fechar())

# usuários conectados neste servidor (compartilhado entre sessões)
qr_online <- reactiveVal(list())

shinyApp(ui = app_ui(), server = app_server(qr_reg, qr_online))
