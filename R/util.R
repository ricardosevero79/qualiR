# ---------------------------------------------------------------------------
# QualiR - utilidades gerais
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

agora <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

fmt_data <- function(x) {
  x <- as.character(x)
  out <- ifelse(is.na(x) | x == "", "", format(as.POSIXct(x, tz = ""), "%d/%m/%Y %H:%M"))
  out[is.na(out)] <- ""
  out
}

# Paleta de cores para nós e usuários 
QR_PALETA <- c("#E8505B", "#F39C12", "#F1C40F", "#2ECC71", "#1ABC9C", "#3498DB",
               "#2B579A", "#9B59B6", "#E91E63", "#795548", "#16A085", "#D35400",
               "#7F8C8D", "#8E44AD", "#27AE60", "#C0392B")

cor_aleatoria <- function(i = NULL) {
  if (is.null(i)) sample(QR_PALETA, 1) else QR_PALETA[((i - 1) %% length(QR_PALETA)) + 1]
}

hex_rgb <- function(hex) {
  hex <- sub("^#", "", hex %||% "999999")
  if (nchar(hex) != 6) hex <- "999999"
  strtoi(c(substr(hex, 1, 2), substr(hex, 3, 4), substr(hex, 5, 6)), 16L)
}

# Mistura várias cores (média RGB) e devolve rgba() com transparência
mistura_rgba <- function(hexes, alpha = 0.35) {
  m <- vapply(hexes, hex_rgb, numeric(3))
  if (is.null(dim(m))) m <- matrix(m, nrow = 3)
  rgb <- round(rowMeans(m))
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

esc <- function(x) htmltools::htmlEscape(x)

# Conta palavras
n_palavras <- function(x) {
  vapply(strsplit(x, "[^\\p{L}\\p{N}]+", perl = TRUE), function(w) sum(nzchar(w)), integer(1))
}

trunca <- function(x, n = 80) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "…"), x)

# Lista de palavras vazias (stopwords) em português
STOPWORDS_PT <- c(
  "a","à","ao","aos","aquela","aquelas","aquele","aqueles","aquilo","as","às","até","com","como",
  "da","das","de","dela","delas","dele","deles","depois","do","dos","e","é","ela","elas","ele",
  "eles","em","entre","era","eram","éramos","essa","essas","esse","esses","esta","está","estamos",
  "estão","estar","estas","estava","estavam","estávamos","este","esteja","estejam","estejamos",
  "estes","esteve","estive","estivemos","estiver","estivera","estiveram","estiverem","estivermos",
  "estivesse","estivessem","estivéssemos","estou","eu","foi","fomos","for","fora","foram","forem",
  "formos","fosse","fossem","fôssemos","fui","há","haja","hajam","hajamos","hão","havemos","haver",
  "hei","houve","houvemos","houver","houvera","houverá","houveram","houverão","houverei","houverem",
  "houveremos","houveria","houveriam","houveríamos","houvermos","houvesse","houvessem","isso",
  "isto","já","lhe","lhes","mais","mas","me","mesmo","meu","meus","minha","minhas","muito","na",
  "não","nas","nem","no","nos","nós","nossa","nossas","nosso","nossos","num","numa","o","os","ou",
  "para","pela","pelas","pelo","pelos","por","qual","quando","que","quem","são","se","seja","sejam",
  "sejamos","sem","ser","será","serão","serei","seremos","seria","seriam","seríamos","seu","seus",
  "só","somos","sou","sua","suas","também","te","tem","tém","têm","temos","tenha","tenham",
  "tenhamos","tenho","terá","terão","terei","teremos","teria","teriam","teríamos","teu","teus",
  "teve","tinha","tinham","tínhamos","tive","tivemos","tiver","tivera","tiveram","tiverem",
  "tivermos","tivesse","tivessem","tivéssemos","tu","tua","tuas","um","uma","umas","uns","você",
  "vocês","vos","vai","vou","vão","ter","então","aí","aqui","lá","ali","assim","porque","pra","pro",
  "tá","tô","né","ah","eh","hum","uhum","hã","ok","tipo","coisa","gente","sim","bem","bom","agora",
  "ainda","sempre","cada","outro","outra","outros","outras","todo","toda","todos","todas","alguma",
  "algum","alguns","algumas","onde","sobre","ser","fazer","faz","fez","dizer","disse","acho",
  "acha","ia","iam","daí","dessa","desse","disso","nessa","nesse","nisso","neste","nesta","deste",
  "desta","pois","porém","contudo","embora","enquanto","seus","umas","lo","la","los","las",
  "nao","voce","entao","ja","so","tambem","esta","estao","porque","pq","q","vc","vcs"
)

# opções do selectize com texto em português para criar novo nó
SELECTIZE_CRIAR <- function(placeholder) list(
  create = TRUE, placeholder = placeholder,
  # permite digitar mesmo com um item já escolhido (restaura se nada for escolhido)
  onFocus = I("function() { this._prev = this.getValue(); this.clear(true); }"),
  onBlur = I("function() { if (!this.getValue() && this._prev) this.setValue(this._prev, true); }"),
  render = I("{option_create: function(d, esc) { return '<div class=\"create\">+ Criar nó <strong>' + esc(d.input) + '</strong>\u2026</div>'; }}"))

# decodifica entidades numéricas (&#243;) que algumas versões do openxlsx devolvem
decodificar_entidades <- function(x) {
  if (!is.character(x)) return(x)
  vapply(x, function(s) {
    if (is.na(s) || !grepl("&#", s, fixed = TRUE)) return(s)
    m <- gregexpr("&#(x?[0-9A-Fa-f]+);", s, perl = TRUE)
    regmatches(s, m) <- list(vapply(regmatches(s, m)[[1]], function(e) {
      n <- sub("^&#(x?)([0-9A-Fa-f]+);$", "\\2", e); hex <- grepl("^&#x", e)
      intToUtf8(strtoi(n, if (hex) 16L else 10L))
    }, ""))
    s
  }, "", USE.NAMES = FALSE)
}
