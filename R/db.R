# ---------------------------------------------------------------------------
# QualiR - camada de banco de dados (SQLite compartilhado pela equipe)
# Posições de texto: pos_start (0-based, inclusiva) e pos_end (exclusiva)
# ---------------------------------------------------------------------------

qr_caminho_banco <- function() {
  Sys.getenv("QUALIR_DB", unset = file.path(getwd(), "projeto_qualir.sqlite"))
}

db_conectar <- function(caminho = qr_caminho_banco()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), caminho)
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL")
  DBI::dbExecute(con, "PRAGMA busy_timeout=10000")
  DBI::dbExecute(con, "PRAGMA foreign_keys=ON")
  db_inicializar(con)
  con
}

db_inicializar <- function(con, admin = NULL) {
  sql <- c(
    "CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT)",
    "CREATE TABLE IF NOT EXISTS users (
       id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT UNIQUE NOT NULL, name TEXT,
       pass TEXT NOT NULL, role TEXT NOT NULL DEFAULT 'pesquisador', color TEXT,
       active INTEGER NOT NULL DEFAULT 1, created TEXT)",
    "CREATE TABLE IF NOT EXISTS documents (
       id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, type TEXT,
       content TEXT NOT NULL, description TEXT, is_transcript INTEGER NOT NULL DEFAULT 0,
       created_by INTEGER, created TEXT, modified TEXT)",
    "CREATE TABLE IF NOT EXISTS nodes (
       id INTEGER PRIMARY KEY AUTOINCREMENT, kind TEXT NOT NULL DEFAULT 'code',
       parent_id INTEGER REFERENCES nodes(id) ON DELETE CASCADE, name TEXT NOT NULL,
       description TEXT, color TEXT, created_by INTEGER, created TEXT, modified TEXT)",
    "CREATE TABLE IF NOT EXISTS codings (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       doc_id INTEGER NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
       node_id INTEGER NOT NULL REFERENCES nodes(id) ON DELETE CASCADE,
       pos_start INTEGER NOT NULL, pos_end INTEGER NOT NULL, user_id INTEGER, created TEXT)",
    "CREATE INDEX IF NOT EXISTS idx_cod_doc ON codings(doc_id)",
    "CREATE INDEX IF NOT EXISTS idx_cod_node ON codings(node_id)",
    "CREATE TABLE IF NOT EXISTS attributes (
       node_id INTEGER NOT NULL REFERENCES nodes(id) ON DELETE CASCADE,
       attr TEXT NOT NULL, value TEXT, PRIMARY KEY (node_id, attr))",
    "CREATE TABLE IF NOT EXISTS memos (
       id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL, body TEXT,
       link_type TEXT, link_id INTEGER, user_id INTEGER, created TEXT, modified TEXT)",
    "CREATE TABLE IF NOT EXISTS doc_attributes (
       doc_id INTEGER NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
       attr TEXT NOT NULL, value TEXT, PRIMARY KEY (doc_id, attr))",
    "CREATE TABLE IF NOT EXISTS saved_queries (
       id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, qtype TEXT NOT NULL,
       params TEXT NOT NULL, user_id INTEGER, created TEXT)",
    "CREATE TABLE IF NOT EXISTS activity (
       id INTEGER PRIMARY KEY AUTOINCREMENT, user_id INTEGER, ts TEXT, action TEXT, detail TEXT)"
  )
  sql <- c(sql,
    "CREATE TABLE IF NOT EXISTS annotations (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       doc_id INTEGER NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
       pos_start INTEGER NOT NULL, pos_end INTEGER NOT NULL, body TEXT, user_id INTEGER, created TEXT)",
    "CREATE TABLE IF NOT EXISTS rel_types (
       id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, direction TEXT NOT NULL DEFAULT 'associativa',
       created_by INTEGER, created TEXT)",
    "CREATE TABLE IF NOT EXISTS relationships (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       from_id INTEGER NOT NULL REFERENCES nodes(id) ON DELETE CASCADE,
       to_id INTEGER NOT NULL REFERENCES nodes(id) ON DELETE CASCADE,
       type_id INTEGER REFERENCES rel_types(id) ON DELETE SET NULL,
       description TEXT, created_by INTEGER, created TEXT)",
    "CREATE TABLE IF NOT EXISTS sets (
       id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, description TEXT, created_by INTEGER, created TEXT)",
    "CREATE TABLE IF NOT EXISTS set_items (
       set_id INTEGER NOT NULL REFERENCES sets(id) ON DELETE CASCADE,
       item_type TEXT NOT NULL, item_id INTEGER NOT NULL, PRIMARY KEY (set_id, item_type, item_id))",
    "CREATE TABLE IF NOT EXISTS saved_matrices (
       id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, data TEXT NOT NULL, params TEXT,
       user_id INTEGER, created TEXT)",
    "CREATE TABLE IF NOT EXISTS recent (
       user_id INTEGER NOT NULL, item_type TEXT NOT NULL, item_id TEXT NOT NULL, ts TEXT,
       PRIMARY KEY (user_id, item_type, item_id))")
  for (s in sql) DBI::dbExecute(con, s)
  # migrações (colunas novas em projetos antigos)
  add_col <- function(tab, col, tipo) {
    if (!(col %in% DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", tab))$name))
      DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s", tab, col, tipo))
  }
  add_col("nodes", "modified_by", "INTEGER")
  add_col("documents", "modified_by", "INTEGER")
  if (DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM rel_types")$n == 0) {
    for (r in list(c("Associado a", "associativa"), c("Influencia", "unidirecional"),
                   c("Leva a", "unidirecional"), c("Contradiz", "simetrica")))
      DBI::dbExecute(con, "INSERT INTO rel_types(name, direction, created) VALUES (?,?,?)",
                     params = list(r[1], r[2], agora()))
  }

  if (nrow(DBI::dbGetQuery(con, "SELECT 1 FROM meta WHERE key='version'")) == 0) {
    DBI::dbExecute(con, "INSERT INTO meta VALUES ('version','0')")
    DBI::dbExecute(con, "INSERT INTO meta VALUES ('project_name','Novo projeto')")
  }
  if (DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM users")$n == 0) {
    if (is.null(admin)) {
      db_criar_usuario(con, "admin", "Administrador", "admin", "admin")
    } else if (!is.null(admin$hash)) {
      DBI::dbExecute(con, "INSERT INTO users(username, name, pass, role, color, created) VALUES (?,?,?,?,?,?)",
                     params = list(tolower(admin$user), admin$name, admin$hash, "admin", admin$color %||% "#2B6CB8", agora()))
    } else {
      db_criar_usuario(con, admin$user, admin$name, admin$pass, "admin", admin$color)
    }
  }
  invisible(TRUE)
}

# ---- versão (para atualização ao vivo entre membros da equipe) -------------
db_versao <- function(con) {
  as.integer(DBI::dbGetQuery(con, "SELECT value FROM meta WHERE key='version'")$value)
}
db_tocar <- function(con) {
  DBI::dbExecute(con, "UPDATE meta SET value = CAST(value AS INTEGER) + 1 WHERE key='version'")
}
db_meta <- function(con, key) {
  r <- DBI::dbGetQuery(con, "SELECT value FROM meta WHERE key=?", params = list(key))
  if (nrow(r)) r$value else NA_character_
}
db_set_meta <- function(con, key, value) {
  DBI::dbExecute(con, "INSERT OR REPLACE INTO meta(key,value) VALUES (?,?)", params = list(key, value))
  db_tocar(con)
}

db_log <- function(con, user_id, action, detail = "") {
  DBI::dbExecute(con, "INSERT INTO activity(user_id, ts, action, detail) VALUES (?,?,?,?)",
                 params = list(as.integer(user_id), agora(), action, detail))
}

ultimo_id <- function(con) DBI::dbGetQuery(con, "SELECT last_insert_rowid() id")$id

# ---- usuários --------------------------------------------------------------
db_criar_usuario <- function(con, username, name, senha, role = "pesquisador", color = NULL) {
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM users")$n
  DBI::dbExecute(con,
    "INSERT INTO users(username, name, pass, role, color, created) VALUES (?,?,?,?,?,?)",
    params = list(tolower(trimws(username)), name, sodium::password_store(senha), role,
                  color %||% cor_aleatoria(n + 6), agora()))
  db_tocar(con)
  ultimo_id(con)
}

db_verificar_login <- function(con, username, senha) {
  u <- DBI::dbGetQuery(con, "SELECT * FROM users WHERE username=? AND active=1",
                       params = list(tolower(trimws(username))))
  if (nrow(u) == 0) return(NULL)
  ok <- tryCatch(sodium::password_verify(u$pass[1], senha), error = function(e) FALSE)
  if (isTRUE(ok)) u[1, setdiff(names(u), "pass")] else NULL
}

db_trocar_senha <- function(con, user_id, senha) {
  DBI::dbExecute(con, "UPDATE users SET pass=? WHERE id=?",
                 params = list(sodium::password_store(senha), as.integer(user_id)))
}

db_usuarios <- function(con) {
  DBI::dbGetQuery(con, "SELECT id, username, name, role, color, active, created FROM users ORDER BY id")
}

# ---- documentos ------------------------------------------------------------
db_docs <- function(con) {
  DBI::dbGetQuery(con, "SELECT id, name, type, description, is_transcript, created_by, created,
                         modified, modified_by, length(content) AS n_chars FROM documents ORDER BY name")
}
db_doc_texto <- function(con, id) {
  DBI::dbGetQuery(con, "SELECT content FROM documents WHERE id=?", params = list(as.integer(id)))$content
}
db_inserir_doc <- function(con, name, type, content, user_id, is_transcript = FALSE, description = "") {
  DBI::dbExecute(con,
    "INSERT INTO documents(name, type, content, description, is_transcript, created_by, created, modified, modified_by)
     VALUES (?,?,?,?,?,?,?,?,?)",
    params = list(name, type, content, description, as.integer(is_transcript), as.integer(user_id),
                  agora(), agora(), as.integer(user_id)))
  id <- ultimo_id(con)
  db_tocar(con)
  id
}

# ---- nós (nós e casos) -------------------------------------------------
db_nodes <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM nodes ORDER BY kind, name COLLATE NOCASE")
}
db_criar_node <- function(con, name, kind = "code", parent_id = NA, color = NULL,
                          description = "", user_id = NA) {
  DBI::dbExecute(con,
    "INSERT INTO nodes(kind, parent_id, name, description, color, created_by, created, modified, modified_by)
     VALUES (?,?,?,?,?,?,?,?,?)",
    params = list(kind, if (is.null(parent_id) || is.na(parent_id)) NA_integer_ else as.integer(parent_id),
                  name, description, color %||% cor_aleatoria(), as.integer(user_id), agora(), agora(), as.integer(user_id)))
  id <- ultimo_id(con)
  db_tocar(con)
  id
}
db_atualizar_node <- function(con, id, name, parent_id, color, description, user_id = NA) {
  DBI::dbExecute(con,
    "UPDATE nodes SET name=?, parent_id=?, color=?, description=?, modified=?, modified_by=? WHERE id=?",
    params = list(name, if (is.null(parent_id) || is.na(parent_id)) NA_integer_ else as.integer(parent_id),
                  color, description, agora(), as.integer(user_id), as.integer(id)))
  db_tocar(con)
}
db_mover_node <- function(con, id, parent_id) {
  DBI::dbExecute(con, "UPDATE nodes SET parent_id=?, modified=? WHERE id=?",
                 params = list(if (is.null(parent_id) || is.na(parent_id)) NA_integer_ else as.integer(parent_id),
                               agora(), as.integer(id)))
  db_tocar(con)
}
db_excluir_node <- function(con, id) {
  DBI::dbExecute(con, "DELETE FROM nodes WHERE id=?", params = list(as.integer(id)))
  db_tocar(con)
}
# Mescla 'origem' em 'destino': move codificações e nós filhos, apaga origem
db_mesclar_nodes <- function(con, origem, destino) {
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "UPDATE codings SET node_id=? WHERE node_id=?",
                   params = list(as.integer(destino), as.integer(origem)))
    DBI::dbExecute(con, "UPDATE nodes SET parent_id=? WHERE parent_id=?",
                   params = list(as.integer(destino), as.integer(origem)))
    DBI::dbExecute(con, "DELETE FROM nodes WHERE id=?", params = list(as.integer(origem)))
  })
  db_tocar(con)
}

# ---- codificação -----------------------------------------------------------
db_codings <- function(con) {
  DBI::dbGetQuery(con, "SELECT id, doc_id, node_id, pos_start, pos_end, user_id, created FROM codings")
}
db_codificar <- function(con, doc_id, node_id, pos_start, pos_end, user_id, tocar = TRUE) {
  # evita duplicata exata do mesmo usuário
  dup <- DBI::dbGetQuery(con,
    "SELECT id FROM codings WHERE doc_id=? AND node_id=? AND pos_start=? AND pos_end=? AND user_id=?",
    params = list(as.integer(doc_id), as.integer(node_id), as.integer(pos_start), as.integer(pos_end),
                  as.integer(user_id)))
  if (nrow(dup)) return(invisible(dup$id[1]))
  DBI::dbExecute(con,
    "INSERT INTO codings(doc_id, node_id, pos_start, pos_end, user_id, created) VALUES (?,?,?,?,?,?)",
    params = list(as.integer(doc_id), as.integer(node_id), as.integer(pos_start), as.integer(pos_end),
                  as.integer(user_id), agora()))
  DBI::dbExecute(con, "UPDATE nodes SET modified=?, modified_by=? WHERE id=?",
                 params = list(agora(), as.integer(user_id), as.integer(node_id)))
  if (tocar) db_tocar(con)
  invisible(ultimo_id(con))
}
db_descodificar <- function(con, coding_id) {
  DBI::dbExecute(con, "DELETE FROM codings WHERE id=?", params = list(as.integer(coding_id)))
  db_tocar(con)
}

# ---- atributos de casos ----------------------------------------------------
db_atributos <- function(con) DBI::dbGetQuery(con, "SELECT * FROM attributes")
db_set_atributo <- function(con, node_id, attr, value) {
  DBI::dbExecute(con, "INSERT OR REPLACE INTO attributes(node_id, attr, value) VALUES (?,?,?)",
                 params = list(as.integer(node_id), attr, value))
  db_tocar(con)
}
db_renomear_atributo <- function(con, antigo, novo) {
  DBI::dbExecute(con, "UPDATE attributes SET attr=? WHERE attr=?", params = list(novo, antigo))
  db_tocar(con)
}

# ---- notas (memos) ---------------------------------------------------------
db_memos <- function(con) DBI::dbGetQuery(con, "SELECT * FROM memos ORDER BY modified DESC")
db_criar_memo <- function(con, title, body, link_type, link_id, user_id) {
  DBI::dbExecute(con,
    "INSERT INTO memos(title, body, link_type, link_id, user_id, created, modified) VALUES (?,?,?,?,?,?,?)",
    params = list(title, body, link_type, if (is.null(link_id)) NA_integer_ else as.integer(link_id),
                  as.integer(user_id), agora(), agora()))
  id <- ultimo_id(con)
  db_tocar(con)
  id
}
db_salvar_memo <- function(con, id, title, body) {
  DBI::dbExecute(con, "UPDATE memos SET title=?, body=?, modified=? WHERE id=?",
                 params = list(title, body, agora(), as.integer(id)))
  db_tocar(con)
}
db_excluir_memo <- function(con, id) {
  DBI::dbExecute(con, "DELETE FROM memos WHERE id=?", params = list(as.integer(id)))
  db_tocar(con)
}

db_atividade <- function(con, n = 300) {
  DBI::dbGetQuery(con, "SELECT a.*, u.name AS user_name FROM activity a LEFT JOIN users u ON u.id=a.user_id
                        ORDER BY a.id DESC LIMIT ?", params = list(as.integer(n)))
}

# Cópia de segurança consistente do banco
db_backup <- function(con, destino) {
  if (file.exists(destino)) file.remove(destino)
  DBI::dbExecute(con, paste0("VACUUM INTO '", gsub("'", "''", destino), "'"))
  destino
}

db_atualizar_doc <- function(con, id, name, description) {
  DBI::dbExecute(con, "UPDATE documents SET name=?, description=?, modified=? WHERE id=?",
                 params = list(name, description, agora(), as.integer(id)))
  db_tocar(con)
}
db_excluir_doc <- function(con, id) {
  DBI::dbExecute(con, "DELETE FROM documents WHERE id=?", params = list(as.integer(id)))
  db_tocar(con)
}
db_atualizar_usuario <- function(con, id, name, role, color, active) {
  DBI::dbExecute(con, "UPDATE users SET name=?, role=?, color=?, active=? WHERE id=?",
                 params = list(name, role, color, as.integer(active), as.integer(id)))
  db_tocar(con)
}

# Remove o intervalo [s, e) das codificações indicadas (pode dividir uma codificação em duas)
db_descodificar_intervalo <- function(con, linhas, s, e) {
  if (!nrow(linhas)) return(0L)
  DBI::dbWithTransaction(con, {
    for (i in seq_len(nrow(linhas))) {
      r <- linhas[i, ]
      if (r$pos_end <= s || r$pos_start >= e) next
      DBI::dbExecute(con, "DELETE FROM codings WHERE id=?", params = list(as.integer(r$id)))
      if (r$pos_start < s)
        DBI::dbExecute(con, "INSERT INTO codings(doc_id,node_id,pos_start,pos_end,user_id,created) VALUES (?,?,?,?,?,?)",
                       params = list(r$doc_id, r$node_id, r$pos_start, as.integer(s), r$user_id, r$created))
      if (r$pos_end > e)
        DBI::dbExecute(con, "INSERT INTO codings(doc_id,node_id,pos_start,pos_end,user_id,created) VALUES (?,?,?,?,?,?)",
                       params = list(r$doc_id, r$node_id, as.integer(e), r$pos_end, r$user_id, r$created))
    }
  })
  db_tocar(con)
  nrow(linhas)
}

nome_unico_doc <- function(con, nome) {
  existentes <- DBI::dbGetQuery(con, "SELECT name FROM documents")$name
  if (!(nome %in% existentes)) return(nome)
  k <- 2
  while (paste0(nome, " (", k, ")") %in% existentes) k <- k + 1
  paste0(nome, " (", k, ")")
}

# Importa um texto: cria documento e (opcionalmente) casos por falante
importar_texto <- function(con, nome, texto, tipo, user_id, detectar = TRUE, autocodificar = TRUE,
                           ignorar = character()) {
  transc <- detectar && eh_transcricao(texto)
  nome <- nome_unico_doc(con, nome)
  doc_id <- db_inserir_doc(con, nome, tipo, texto, user_id, is_transcript = transc)
  n_casos <- 0L
  if (transc && autocodificar) {
    t <- turnos_de_fala(texto)
    ign <- tolower(trimws(ignorar)); ign <- ign[nzchar(ign)]
    t <- t[!(tolower(t$falante) %in% ign), , drop = FALSE]
    if (nrow(t)) {
      casos <- DBI::dbGetQuery(con, "SELECT id, name FROM nodes WHERE kind='case'")
      DBI::dbWithTransaction(con, {
        for (f in unique(t$falante)) {
          cid <- casos$id[tolower(casos$name) == tolower(f)][1]
          if (is.na(cid)) {
            cid <- db_criar_node(con, f, kind = "case", user_id = user_id)
            casos <- rbind(casos, data.frame(id = cid, name = f))
            n_casos <- n_casos + 1L
          }
          tt <- t[t$falante == f, ]
          for (i in seq_len(nrow(tt)))
            db_codificar(con, doc_id, cid, tt$fala_start[i], tt$fala_end[i], user_id, tocar = FALSE)
        }
      })
      db_tocar(con)
    }
  }
  list(doc_id = doc_id, transcricao = transc, novos_casos = n_casos, nome = nome)
}


# ---- atributos de arquivos -------------------------------------------------
db_doc_atributos <- function(con) DBI::dbGetQuery(con, "SELECT * FROM doc_attributes")
db_set_doc_atributo <- function(con, doc_id, attr, value) {
  DBI::dbExecute(con, "INSERT OR REPLACE INTO doc_attributes(doc_id, attr, value) VALUES (?,?,?)",
                 params = list(as.integer(doc_id), attr, value))
  db_tocar(con)
}

# ---- consultas salvas ------------------------------------------------------
db_consultas_salvas <- function(con) DBI::dbGetQuery(con, "SELECT * FROM saved_queries ORDER BY name COLLATE NOCASE")
db_salvar_consulta <- function(con, name, qtype, params, user_id) {
  js <- jsonlite::toJSON(params, auto_unbox = TRUE, null = "null")
  ex <- DBI::dbGetQuery(con, "SELECT id FROM saved_queries WHERE name=? AND qtype=?", params = list(name, qtype))
  if (nrow(ex)) {
    DBI::dbExecute(con, "UPDATE saved_queries SET params=?, user_id=?, created=? WHERE id=?",
                   params = list(as.character(js), as.integer(user_id), agora(), ex$id[1]))
  } else {
    DBI::dbExecute(con, "INSERT INTO saved_queries(name, qtype, params, user_id, created) VALUES (?,?,?,?,?)",
                   params = list(name, qtype, as.character(js), as.integer(user_id), agora()))
  }
  db_tocar(con)
}
db_excluir_consulta <- function(con, id) {
  DBI::dbExecute(con, "DELETE FROM saved_queries WHERE id=?", params = list(as.integer(id)))
  db_tocar(con)
}


# ---- anotações ---------------------------------------------------------------
db_anotacoes <- function(con) DBI::dbGetQuery(con, "SELECT * FROM annotations ORDER BY doc_id, pos_start")
db_criar_anotacao <- function(con, doc_id, s, e, body, user_id) {
  DBI::dbExecute(con, "INSERT INTO annotations(doc_id,pos_start,pos_end,body,user_id,created) VALUES (?,?,?,?,?,?)",
                 params = list(as.integer(doc_id), as.integer(s), as.integer(e), body, as.integer(user_id), agora()))
  db_tocar(con)
}
db_editar_anotacao <- function(con, id, body) {
  DBI::dbExecute(con, "UPDATE annotations SET body=? WHERE id=?", params = list(body, as.integer(id))); db_tocar(con)
}
db_excluir_anotacao <- function(con, id) {
  DBI::dbExecute(con, "DELETE FROM annotations WHERE id=?", params = list(as.integer(id))); db_tocar(con)
}

# ---- relacionamentos ----------------------------------------------------------
db_rel_tipos <- function(con) DBI::dbGetQuery(con, "SELECT * FROM rel_types ORDER BY name COLLATE NOCASE")
db_relacionamentos <- function(con) DBI::dbGetQuery(con, "SELECT * FROM relationships ORDER BY id")
db_criar_rel_tipo <- function(con, name, direction, user_id) {
  DBI::dbExecute(con, "INSERT INTO rel_types(name,direction,created_by,created) VALUES (?,?,?,?)",
                 params = list(name, direction, as.integer(user_id), agora())); db_tocar(con)
}
db_editar_rel_tipo <- function(con, id, name, direction) {
  DBI::dbExecute(con, "UPDATE rel_types SET name=?, direction=? WHERE id=?", params = list(name, direction, as.integer(id))); db_tocar(con)
}
db_excluir_rel_tipo <- function(con, id) { DBI::dbExecute(con, "DELETE FROM rel_types WHERE id=?", params = list(as.integer(id))); db_tocar(con) }
db_criar_relacionamento <- function(con, from_id, to_id, type_id, description, user_id) {
  DBI::dbExecute(con, "INSERT INTO relationships(from_id,to_id,type_id,description,created_by,created) VALUES (?,?,?,?,?,?)",
                 params = list(as.integer(from_id), as.integer(to_id), as.integer(type_id), description, as.integer(user_id), agora()))
  db_tocar(con)
}
db_editar_relacionamento <- function(con, id, from_id, to_id, type_id, description) {
  DBI::dbExecute(con, "UPDATE relationships SET from_id=?, to_id=?, type_id=?, description=? WHERE id=?",
                 params = list(as.integer(from_id), as.integer(to_id), as.integer(type_id), description, as.integer(id))); db_tocar(con)
}
db_excluir_relacionamento <- function(con, id) { DBI::dbExecute(con, "DELETE FROM relationships WHERE id=?", params = list(as.integer(id))); db_tocar(con) }

# ---- conjuntos ------------------------------------------------------------------
db_conjuntos <- function(con) DBI::dbGetQuery(con, "SELECT * FROM sets ORDER BY name COLLATE NOCASE")
db_conjunto_itens <- function(con) DBI::dbGetQuery(con, "SELECT * FROM set_items")
db_criar_conjunto <- function(con, name, description, user_id) {
  DBI::dbExecute(con, "INSERT INTO sets(name,description,created_by,created) VALUES (?,?,?,?)",
                 params = list(name, description, as.integer(user_id), agora()))
  id <- ultimo_id(con); db_tocar(con); id
}
db_editar_conjunto <- function(con, id, name, description) {
  DBI::dbExecute(con, "UPDATE sets SET name=?, description=? WHERE id=?", params = list(name, description, as.integer(id))); db_tocar(con)
}
db_excluir_conjunto <- function(con, id) { DBI::dbExecute(con, "DELETE FROM sets WHERE id=?", params = list(as.integer(id))); db_tocar(con) }
db_add_conjunto <- function(con, set_id, item_type, item_id) {
  DBI::dbExecute(con, "INSERT OR IGNORE INTO set_items(set_id,item_type,item_id) VALUES (?,?,?)",
                 params = list(as.integer(set_id), item_type, as.integer(item_id))); db_tocar(con)
}
db_rm_conjunto <- function(con, set_id, item_type, item_id) {
  DBI::dbExecute(con, "DELETE FROM set_items WHERE set_id=? AND item_type=? AND item_id=?",
                 params = list(as.integer(set_id), item_type, as.integer(item_id))); db_tocar(con)
}

# ---- matrizes salvas -------------------------------------------------------------
db_matrizes <- function(con) DBI::dbGetQuery(con, "SELECT id, name, params, user_id, created FROM saved_matrices ORDER BY name COLLATE NOCASE")
db_matriz <- function(con, id) DBI::dbGetQuery(con, "SELECT * FROM saved_matrices WHERE id=?", params = list(as.integer(id)))
db_salvar_matriz <- function(con, name, df, params, user_id) {
  DBI::dbExecute(con, "INSERT INTO saved_matrices(name,data,params,user_id,created) VALUES (?,?,?,?,?)",
                 params = list(name, as.character(jsonlite::toJSON(df, dataframe = "columns")),
                               as.character(jsonlite::toJSON(params, auto_unbox = TRUE)), as.integer(user_id), agora()))
  db_tocar(con)
}
db_excluir_matriz <- function(con, id) { DBI::dbExecute(con, "DELETE FROM saved_matrices WHERE id=?", params = list(as.integer(id))); db_tocar(con) }

# ---- acesso rápido (itens recentes) ---------------------------------------------
db_registrar_recente <- function(con, user_id, tipo, id) {
  DBI::dbExecute(con, "INSERT OR REPLACE INTO recent(user_id,item_type,item_id,ts) VALUES (?,?,?,?)",
                 params = list(as.integer(user_id), tipo, as.character(id), agora()))
}
db_recentes <- function(con, user_id, n = 30) {
  DBI::dbGetQuery(con, "SELECT * FROM recent WHERE user_id=? ORDER BY ts DESC LIMIT ?", params = list(as.integer(user_id), as.integer(n)))
}
