# ---------------------------------------------------------------------------
# QualiR - gerenciamento de projetos (cada projeto é um arquivo .sqlite)
# ---------------------------------------------------------------------------

# Pasta onde ficam os projetos (pode ser trocada com a variável QUALIR_PROJETOS)
dir_projetos <- function() {
  d <- Sys.getenv("QUALIR_PROJETOS", unset = file.path(getwd(), "projetos"))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  normalizePath(d, winslash = "/")
}

# Versões antigas guardavam o projeto em ./projeto_qualir.sqlite: move para a pasta de projetos
migrar_projeto_antigo <- function() {
  antigo <- file.path(getwd(), "projeto_qualir.sqlite")
  if (!file.exists(antigo)) return(invisible(FALSE))
  destino <- file.path(dir_projetos(), "projeto_qualir.sqlite")
  if (file.exists(destino)) return(invisible(FALSE))
  for (suf in c("", "-wal", "-shm")) {
    if (file.exists(paste0(antigo, suf))) file.rename(paste0(antigo, suf), paste0(destino, suf))
  }
  message("QualiR: projeto antigo movido para ", destino)
  invisible(TRUE)
}

ler_nome_projeto <- function(path) {
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbGetQuery(con, "SELECT value FROM meta WHERE key='project_name'")$value[1]
  }, error = function(e) NA_character_)
}

# Lista os projetos disponíveis: data.frame(id, path, name, modified)
listar_projetos <- function() {
  f <- list.files(dir_projetos(), pattern = "\\.sqlite$", full.names = TRUE)
  extra <- Sys.getenv("QUALIR_DB", unset = "")
  if (nzchar(extra) && file.exists(extra)) f <- unique(c(normalizePath(extra, winslash = "/"), f))
  if (!length(f)) return(data.frame(id = character(), path = character(), name = character(), modified = character()))
  mod <- vapply(f, function(x) {
    t <- max(file.info(c(x, paste0(x, "-wal"))[file.exists(c(x, paste0(x, "-wal")))])$mtime)
    format(t, "%Y-%m-%d %H:%M:%S")
  }, "")
  nm <- vapply(f, ler_nome_projeto, "")
  nm[is.na(nm) | !nzchar(nm)] <- tools::file_path_sans_ext(basename(f[is.na(nm) | !nzchar(nm)]))
  df <- data.frame(id = basename(f), path = f, name = nm, modified = mod, stringsAsFactors = FALSE, row.names = NULL)
  df[order(df$modified, decreasing = TRUE), ]
}

slug_projeto <- function(nome) {
  s <- iconv(nome, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  s <- gsub("[^a-z0-9]+", "_", tolower(s %||% ""))
  s <- gsub("^_+|_+$", "", s)
  if (!nzchar(s)) s <- "projeto"
  substr(s, 1, 50)
}

caminho_livre <- function(base) {
  d <- dir_projetos(); p <- file.path(d, paste0(base, ".sqlite")); k <- 2
  while (file.exists(p)) { p <- file.path(d, sprintf("%s_%d.sqlite", base, k)); k <- k + 1 }
  p
}

# Cria um projeto novo com o administrador informado (senha em texto ou hash já pronto)
criar_projeto <- function(nome, usuario, nome_usuario, senha = NULL, hash = NULL, cor = NULL) {
  path <- caminho_livre(slug_projeto(nome))
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL")
  DBI::dbExecute(con, "PRAGMA foreign_keys=ON")
  db_inicializar(con, admin = list(user = usuario, name = nome_usuario, pass = senha, hash = hash, color = cor))
  DBI::dbExecute(con, "UPDATE meta SET value=? WHERE key='project_name'", params = list(nome))
  path
}

# Importa um arquivo .sqlite (cópia de segurança ou projeto recebido de outra pessoa)
importar_projeto <- function(arquivo, nome_original) {
  ok <- tryCatch({
    c0 <- DBI::dbConnect(RSQLite::SQLite(), arquivo, flags = RSQLite::SQLITE_RO)
    tabs <- DBI::dbListTables(c0); DBI::dbDisconnect(c0)
    all(c("meta", "users", "documents", "nodes", "codings") %in% tabs)
  }, error = function(e) FALSE)
  if (!ok) stop("O arquivo não é um projeto do QualiR.")
  existentes <- listar_projetos()$name
  destino <- caminho_livre(slug_projeto(tools::file_path_sans_ext(nome_original)))
  file.copy(arquivo, destino)
  nm <- ler_nome_projeto(destino)
  if (!is.na(nm) && nm %in% existentes) {   # evita dois projetos com o mesmo nome na lista
    con <- DBI::dbConnect(RSQLite::SQLite(), destino); on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(con, "UPDATE meta SET value=? WHERE key='project_name'",
                   params = list(sprintf("%s (importado em %s)", nm, format(Sys.Date(), "%d/%m/%Y"))))
  }
  destino
}

# Uma conexão por arquivo de projeto, compartilhada entre as sessões
registro_conexoes <- function() {
  e <- new.env()
  e$cons <- list()
  e$get <- function(path) {
    k <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (is.null(e$cons[[k]]) || !DBI::dbIsValid(e$cons[[k]])) e$cons[[k]] <- db_conectar(k)
    e$cons[[k]]
  }
  e$fechar <- function() for (c in e$cons) try(DBI::dbDisconnect(c), silent = TRUE)
  e
}

# ---------------------------------------------------------------------------
# Interface: telas e janelas de projeto
# ---------------------------------------------------------------------------
server_projetos <- function(input, output, session, S, reg) {
  logado <- function() !is.null(isolate(S$user()))

  # ---- novo projeto
  modal_novo <- function() {
    showModal(modalDialog(title = tagList(icon("folder-plus"), " Novo projeto"), size = "m",
      textInput("np_nome", "Nome do projeto", "", width = "100%", placeholder = "ex.: Trajetórias escolares 2026"),
      if (logado()) div(class = "alert alert-info py-2", icon("circle-info"),
        sprintf(" Você (%s) será o administrador do novo projeto, com o mesmo usuário e senha. Os demais membros da equipe são cadastrados dentro de cada projeto.",
                isolate(S$user())$username))
      else tagList(
        tags$p(tags$b("Administrador do projeto"), tags$br(),
               tags$small("Quem cria o projeto é o administrador e depois cadastra a equipe.")),
        textInput("np_user", "Usuário (login)", "", width = "100%"),
        textInput("np_uname", "Seu nome", "", width = "100%"),
        passwordInput("np_pass", "Senha (mínimo 4 caracteres)", width = "100%"),
        passwordInput("np_pass2", "Repita a senha", width = "100%")),
      footer = tagList(modalButton("Cancelar"), actionButton("np_ok", "Criar projeto", class = "btn-primary"))))
  }
  observeEvent(input$lp_new, modal_novo())
  observeEvent(input$rb_novo_projeto, modal_novo())
  observeEvent(input$np_ok, {
    nome <- trimws(input$np_nome %||% "")
    if (!nzchar(nome)) { showNotification("Informe o nome do projeto.", type = "error"); return() }
    if (logado()) {
      u <- S$user()
      hash <- DBI::dbGetQuery(S$con, "SELECT pass FROM users WHERE id=?", params = list(u$id))$pass
      path <- criar_projeto(nome, u$username, u$name, hash = hash, cor = u$color)
      db_log(S$con, u$id, "criou novo projeto", nome)
      removeModal()
      showModal(modalDialog(title = "Projeto criado", size = "s",
        sprintf("O projeto “%s” foi criado. Deseja abri-lo agora?", nome),
        footer = tagList(modalButton("Agora não"),
                         actionButton("np_abrir", "Abrir projeto", class = "btn-primary",
                                      onclick = sprintf("QualiR.irProjeto('%s')", basename(path))))))
    } else {
      us <- trimws(input$np_user %||% ""); p1 <- input$np_pass %||% ""
      if (!nzchar(us) || nchar(p1) < 4 || !identical(p1, input$np_pass2)) {
        showNotification("Informe o usuário e a senha (mínimo 4 caracteres, repetida igual).", type = "error"); return() }
      nm <- trimws(input$np_uname %||% ""); if (!nzchar(nm)) nm <- us
      path <- criar_projeto(nome, us, nm, senha = p1)
      removeModal()
      S$proj_sel(basename(path)); S$proj_ver(S$proj_ver() + 1)
      showNotification(sprintf("Projeto “%s” criado. Entre com o usuário %s.", nome, us), duration = 8)
    }
  })

  # ---- abrir projeto
  observeEvent(input$rb_abrir_projeto, {
    pr <- listar_projetos()
    atual <- basename(S$projeto %||% "")
    showModal(modalDialog(title = tagList(icon("folder-open"), " Abrir projeto"), size = "l", easyClose = TRUE,
      if (!nrow(pr)) p("Nenhum projeto encontrado.") else
        tags$table(class = "qlist",
          tags$thead(tags$tr(tags$th("Projeto"), tags$th("Arquivo"), tags$th("Modificado"), tags$th(""))),
          tags$tbody(lapply(seq_len(nrow(pr)), function(i) tags$tr(class = "qrow",
            tags$td(div(class = "namecell", icon("folder"), tags$b(pr$name[i]))),
            tags$td(pr$id[i]), tags$td(fmt_data(pr$modified[i])),
            tags$td(if (pr$id[i] == atual) tags$span(class = "badge bg-secondary", "aberto")
                    else tags$button(class = "btn btn-sm btn-primary", onclick = sprintf("QualiR.irProjeto('%s')", pr$id[i]), "Abrir")))))),
      tags$small(style = "color:#6B7280;display:block;margin-top:8px",
                 "Ao abrir outro projeto, você entra novamente com o usuário e a senha cadastrados nele. Pasta dos projetos: ",
                 tags$code(dir_projetos())),
      footer = tagList(actionButton("rb_novo_projeto2", "Novo projeto", icon = icon("folder-plus")), modalButton("Fechar"))))
  })
  observeEvent(input$rb_novo_projeto2, modal_novo())

  # ---- importar projeto (.sqlite)
  modal_importar <- function() {
    showModal(modalDialog(title = tagList(icon("file-import"), " Importar projeto"), size = "m",
      p("Escolha um arquivo de projeto do QualiR (.sqlite): uma cópia de segurança ou um projeto recebido de outra pessoa. ",
        "Ele é copiado para a pasta de projetos e passa a aparecer na lista."),
      fileInput("ip_file", NULL, accept = ".sqlite", buttonLabel = "Escolher…", width = "100%"),
      footer = tagList(modalButton("Cancelar"), actionButton("ip_ok", "Importar", class = "btn-primary"))))
  }
  observeEvent(input$lp_import, modal_importar())
  observeEvent(input$rb_importar_projeto, modal_importar())
  observeEvent(input$ip_ok, {
    f <- input$ip_file
    if (is.null(f)) { showNotification("Escolha o arquivo.", type = "warning"); return() }
    path <- tryCatch(importar_projeto(f$datapath, f$name), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(path)) return()
    removeModal()
    nm <- ler_nome_projeto(path)
    if (logado()) {
      showModal(modalDialog(title = "Projeto importado", size = "s",
        sprintf("“%s” foi importado. Deseja abri-lo agora? Você entrará com um usuário cadastrado nesse projeto.", nm),
        footer = tagList(modalButton("Agora não"),
                         tags$button(class = "btn btn-primary", onclick = sprintf("QualiR.irProjeto('%s')", basename(path)), "Abrir projeto"))))
    } else {
      S$proj_sel(basename(path)); S$proj_ver(S$proj_ver() + 1)
      showNotification(sprintf("Projeto “%s” importado.", nm))
    }
  })
}
