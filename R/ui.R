# ---------------------------------------------------------------------------
# QualiR - interface
# ---------------------------------------------------------------------------

rb_btn <- function(id, label, ic, title = label) {
  actionButton(id, label = tagList(icon(ic), tags$span(label)), class = "rb-btn", title = title)
}
rb_dl <- function(id, label, ic, title = label) {
  a <- downloadButton(id, label = label, class = "rb-btn", icon = icon(ic), title = title)
  a$children <- list(icon(ic), tags$span(label))
  a
}
rb_mini <- function(id, label, ic) actionButton(id, tagList(icon(ic), label), class = "rb-mini")
rb_pilha <- function(...) div(class = "rb-stack", ...)
rb_grupo <- function(rotulo, ...) {
  div(class = "rb-group", div(class = "rb-items", ...), if (nzchar(rotulo)) div(class = "rb-label", rotulo))
}

sb_item <- function(sec, rotulo) tags$a(class = "qnav-item", `data-sec` = sec, href = "#", rotulo)
sb_grupo <- function(id, rotulo, ic, ..., aberto = TRUE) {
  div(class = paste("sb-group", if (!aberto) "closed"), `data-grp` = id,
      div(class = "sb-head", icon(ic), tags$span(rotulo), tags$i(class = "sb-chev")),
      div(class = "sb-items", ...))
}

ui_login <- function(projetos, selecionado = NULL) {
  tem <- nrow(projetos) > 0
  div(class = "login-wrap",
    div(class = "login-card",
      div(class = "login-logo", tags$img(src = "img/qualir-selo-512.png", alt = "QualiR — Análise qualitativa colaborativa",
                                         width = "150", height = "150")),
      if (tem) tagList(
        selectInput("login_proj", "Projeto", setNames(projetos$id, projetos$name),
                    selected = if (!is.null(selecionado) && selecionado %in% projetos$id) selecionado else projetos$id[1],
                    width = "100%", selectize = FALSE),
        textInput("login_user", "Usuário", width = "100%"),
        passwordInput("login_pass", "Senha", width = "100%"),
        actionButton("login_go", "Entrar", class = "btn-primary w-100"),
        div(class = "login-msg", textOutput("login_msg"))
      ) else div(class = "login-first",
        p(tags$b("Bem-vindo!"), " Ainda não há nenhum projeto. Crie o primeiro: você será o administrador e depois poderá cadastrar a equipe."),
        actionButton("lp_new", "Criar o primeiro projeto", icon = icon("folder-plus"), class = "btn-primary w-100")),
      div(class = "login-links",
          if (tem) actionLink("lp_new", tagList(icon("folder-plus"), " Novo projeto")),
          actionLink("lp_import", tagList(icon("file-import"), " Importar projeto (.sqlite)")))
    )
  )
}

ui_principal <- function(user, projeto) {
  admin <- identical(user$role, "admin")
  div(id = "qr-app",
    # ======================= BARRA LATERAL ==================================
    div(id = "sidebar",
      div(class = "sb-top",
          div(class = "sb-logo", tags$img(src = "img/qualir-icone-simples.svg", alt = "", width = "26", height = "26", class = "sb-icon"),
              tags$b(HTML('Quali<span class="sb-r">R</span>')),
              tags$a(id = "sb-toggle", href = "#", title = "Recolher painel", icon("angle-left"))),
          div(class = "sb-proj", id = "tb-proj", projeto)),
      div(class = "sb-scroll",
        tags$a(class = "qnav-item sb-quick", `data-sec` = "quick", href = "#", icon("clock-rotate-left"), tags$span("Itens recentes")),
        div(class = "sb-sec", "MATERIAL"),
        sb_grupo("dados", "Dados", "table-list",
                 sb_item("docs", "Arquivos"), sb_item("classif_docs", "Classificações de arquivo")),
        div(class = "sb-sec", "ORGANIZAÇÃO"),
        sb_grupo("codif", "Nós", "circle-nodes",
                 sb_item("codes", "Nós"), sb_item("rels", "Relacionamentos"), sb_item("reltypes", "Tipos de relacionamento")),
        sb_grupo("casos", "Casos", "briefcase",
                 sb_item("cases", "Casos"), sb_item("classif", "Classificações de caso")),
        sb_grupo("notas", "Notas", "note-sticky",
                 sb_item("memos", "Memorandos"), sb_item("annots", "Anotações"), aberto = FALSE),
        sb_grupo("conj", "Conjuntos", "circle-half-stroke", sb_item("sets", "Conjuntos"), aberto = FALSE),
        div(class = "sb-sec", "ANÁLISE"),
        sb_grupo("cons", "Consultas", "magnifying-glass",
                 sb_item("queries", "Consultas"), sb_item("results", "Resultados da consulta"),
                 sb_item("matrices", "Matrizes de codificação"), aberto = FALSE),
        sb_grupo("vis", "Visualizações", "diagram-project", sb_item("maps", "Mapas e gráficos"), aberto = FALSE),
        sb_grupo("rel", "Relatórios", "file-lines", sb_item("reports", "Relatórios"), aberto = FALSE),
        div(class = "sb-sec", "COLABORAÇÃO"),
        sb_grupo("equipe", "Equipe", "users", sb_item("activity", "Atividade"),
                 if (admin) sb_item("users", "Usuários"), aberto = FALSE)
      ),
      div(class = "sb-user", uiOutput("online_users"),
          div(class = "sb-me", tags$span(class = "av", style = sprintf("background:%s", user$color %||% "#888"),
                                         toupper(substr(user$name, 1, 1))),
              tags$span(user$name, tags$small(user$role))))
    ),
    # ======================= ÁREA PRINCIPAL ================================
    div(id = "mainarea",
      div(id = "ribbon",
        div(class = "rb-tabs",
          tags$a(class = "rb-tab", `data-tab` = "arquivo", "Arquivo"),
          tags$a(class = "rb-tab active", `data-tab` = "inicio", "Início"),
          tags$a(class = "rb-tab", `data-tab` = "importar", "Importar"),
          tags$a(class = "rb-tab", `data-tab` = "criar", "Criar"),
          tags$a(class = "rb-tab", `data-tab` = "explorar", "Análise"),
          tags$a(class = "rb-tab", `data-tab` = "compartilhar", "Exportar"),
          tags$a(class = "rb-tab", `data-tab` = "colaborar", "Colaboração"),
          div(class = "rb-right",
              tags$a(class = "rb-collapse", title = "Recolher/expandir a faixa de opções", icon("angle-up"), "Recolher"),
              actionLink("btn_senha", tagList(icon("key"), "Senha"), class = "tb-link"),
              actionLink("btn_sair", tagList(icon("right-from-bracket"), "Sair"), class = "tb-link"))
        ),
        div(class = "rb-panel", `data-tab` = "arquivo",
          rb_grupo("Projetos", rb_btn("rb_novo_projeto", "Novo projeto", "folder-plus"),
                   rb_btn("rb_abrir_projeto", "Abrir projeto", "folder-open"),
                   rb_btn("rb_importar_projeto", "Importar projeto", "file-import")),
          rb_grupo("Este projeto", rb_btn("rb_projeto", "Propriedades do projeto", "pen-to-square"),
                   rb_btn("rb_atualizar2", "Atualizar", "rotate"),
                   if (admin) rb_dl("dl_backup", "Cópia de segurança", "floppy-disk")),
          rb_grupo("Conta", rb_btn("rb_senha", "Trocar senha", "key"), rb_btn("rb_sair", "Sair", "right-from-bracket"))
        ),
        div(class = "rb-panel active", `data-tab` = "inicio",
          rb_grupo("Item", rb_btn("rb_abrir", "Abrir", "folder-open"),
                   rb_btn("rb_propriedades", "Propriedades", "sliders"),
                   rb_btn("rb_excluir", "Excluir", "trash-can")),
          rb_grupo("Explorar", rb_btn("rb_consulta", "Consulta", "magnifying-glass"),
                   rb_btn("rb_visualizar", "Visualizar", "chart-simple")),
          rb_grupo("Codificação", rb_btn("rb_codificar", "Codificar", "circle-plus", "Codificar a seleção"),
                   rb_pilha(rb_mini("rb_autocod", "Autocodificar", "wand-magic-sparkles"),
                            rb_mini("rb_intervalo", "Codificar intervalo", "arrows-up-down"),
                            rb_mini("rb_descodificar", "Descodificar", "circle-minus"))),
          rb_grupo("Classificação", rb_btn("rb_classif_caso", "Classificação de caso", "table-list"),
                   rb_btn("rb_classif_arq", "Classificação de arquivo", "table-columns")),
          rb_grupo("Conjunto", rb_btn("rb_add_conjunto", "Adicionar ao conjunto", "circle-half-stroke")),
          rb_grupo("Projeto", rb_btn("rb_atualizar", "Atualizar", "rotate"))
        ),
        div(class = "rb-panel", `data-tab` = "importar",
          rb_grupo("Dados", rb_btn("rb_importar", "Arquivos", "file-import", "Importar .txt, .docx, .pdf, .vtt, .srt"),
                   rb_btn("rb_colar", "Colar transcrição", "paste")),
          rb_grupo("Codificação", rb_btn("rb_imp_codebook", "Livro de nós", "book")),
          div(class = "rb-hint", icon("circle-info"),
              "Transcrições no formato ", tags$code("Falante: fala"), " ou ", tags$code("[00:01:23] Falante: fala"),
              " são reconhecidas automaticamente; legendas do Teams/Zoom (.vtt) também.")
        ),
        div(class = "rb-panel", `data-tab` = "criar",
          rb_grupo("Codificação", rb_btn("rb_novo_codigo", "Novo nó", "circle"),
                   rb_btn("rb_novo_subcodigo", "Nó filho", "sitemap"),
                   rb_btn("rb_novo_rel", "Relacionamento", "arrows-left-right"),
                   rb_btn("rb_novo_reltipo", "Tipo de relacionamento", "tags")),
          rb_grupo("Casos", rb_btn("rb_novo_caso", "Caso", "briefcase"),
                   rb_btn("rb_novo_attr_caso", "Atributo de caso", "table-list"),
                   rb_btn("rb_novo_attr_arq", "Atributo de arquivo", "table-columns")),
          rb_grupo("Notas", rb_btn("rb_nova_nota", "Memorando", "note-sticky"),
                   rb_btn("rb_nova_anotacao", "Anotação", "comment-dots")),
          rb_grupo("Conjuntos", rb_btn("rb_novo_conjunto", "Conjunto", "circle-half-stroke"))
        ),
        div(class = "rb-panel", `data-tab` = "explorar",
          rb_grupo("Palavras", rb_btn("rb_q_freq", "Frequência de palavras", "chart-column"),
                   rb_btn("rb_q_busca", "Pesquisa de texto", "magnifying-glass"),
                   rb_btn("rb_q_arvore", "Árvore de palavras", "code-branch")),
          rb_grupo("Codificação", rb_btn("rb_q_codq", "Consulta de codificação", "filter"),
                   rb_btn("rb_q_matriz", "Matriz de codificação", "table-cells"),
                   rb_btn("rb_q_cooc", "Coocorrência", "share-nodes"),
                   rb_btn("rb_q_cluster", "Análise de agrupamento", "sitemap"),
                   rb_btn("rb_q_comp", "Comparação de codificação", "people-arrows")),
          rb_grupo("Visualizar", rb_btn("rb_v_hier", "Gráfico hierárquico", "table-cells-large"),
                   rb_btn("rb_v_mapa", "Mapa do projeto", "diagram-project"),
                   rb_btn("rb_v_rel", "Mapa de relacionamentos", "arrows-left-right")),
          rb_grupo("Relatórios", rb_btn("rb_relatorios", "Relatórios", "file-lines"))
        ),
        div(class = "rb-panel", `data-tab` = "compartilhar",
          rb_grupo("Exportar", rb_dl("dl_excel", "Codificação (Excel)", "file-excel"),
                   rb_dl("dl_codebook", "Livro de nós (Word)", "book"),
                   rb_dl("dl_codebook_xlsx", "Livro de nós (Excel)", "table"),
                   rb_dl("dl_doc_cod", "Arquivo aberto com codificação", "file-word")),
          if (admin) rb_grupo("Projeto", rb_dl("dl_backup2", "Cópia de segurança", "floppy-disk"))
        ),
        div(class = "rb-panel", `data-tab` = "colaborar",
          rb_grupo("Equipe", rb_btn("rb_atividade", "Atividade", "clock-rotate-left"),
                   if (admin) rb_btn("rb_usuarios", "Usuários", "users-gear"),
                   rb_btn("rb_q_comp2", "Comparação de codificação", "people-arrows")),
          div(class = "rb-hint", icon("circle-info"),
              "Todos os membros trabalham no mesmo projeto ao mesmo tempo; as alterações aparecem em cerca de 1 segundo.")
        )
      ),
      div(id = "workspace",
        div(id = "listpane",
          div(class = "pane-head", uiOutput("list_title", inline = TRUE),
              div(class = "list-search", icon("magnifying-glass"),
                  tags$input(id = "list-filter", type = "search", placeholder = "Filtrar lista\u2026"))),
          div(class = "pane-body", uiOutput("list_view"))
        ),
        div(id = "vsplitter", title = "Arraste para redimensionar"),
        div(id = "detailpane",
          uiOutput("detail_tabs"),
          uiOutput("detail_view", class = "detail-out"))
      ),
      div(id = "statusbar", uiOutput("status_bar", inline = TRUE))
    ),
    # menu de contexto da codificação
    div(id = "qr-ctx", class = "qr-ctx",
        tags$input(id = "qr-ctx-filter", type = "search", placeholder = "Codificar em… (digite para filtrar)"),
        div(class = "qr-ctx-list"),
        div(class = "qr-ctx-foot",
            tags$a(href = "#", `data-act` = "new", icon("plus"), " Codificar em novo nó…"),
            tags$a(href = "#", `data-act` = "annot", icon("comment-dots"), " Nova anotação…"),
            tags$a(href = "#", `data-act` = "uncode", icon("circle-minus"), " Descodificar seleção…")))
  )
}

app_ui <- function() {
  bslib::page(
    theme = bslib::bs_theme(version = 5, primary = "#1F5FA8",
                            base_font = bslib::font_collection("Segoe UI", "system-ui", "-apple-system", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
                            "font-size-base" = "0.86rem"),
    title = "QualiR",
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = "img/qualir-icone-simples-32.png"),
      tags$link(rel = "apple-touch-icon", href = "img/qualir-icone-192.png"),
      tags$link(rel = "stylesheet", href = "qualir.css"),
      tags$script(src = "qualir.js")
    ),
    shinyjs::useShinyjs(),
    uiOutput("page", class = "page-out")
  )
}
