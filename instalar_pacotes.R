# Instala os pacotes necessários para o QualiR (execute uma vez)
pkgs <- c("shiny", "bslib", "shinyjs", "DBI", "RSQLite", "DT", "jsonlite", "htmltools",
          "sodium", "officer", "pdftools", "openxlsx", "igraph", "wordcloud")
faltando <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando)) install.packages(faltando) else message("Todos os pacotes já estão instalados.")
