#' @title Pesquisa estatísticas municipais no sistema Comex Stat
#' @description Consulta dados de exportação/importação por município via API oficial do Comex Stat.
#' @param ano_inicial [int] Ano inicial da consulta (ex.: 2025).
#' @param ano_final [int] Ano final da consulta (ex.: 2025).
#' @param mes_inicial [int] Mês inicial da consulta (1–12).
#' @param mes_final [int] Mês final da consulta (1–12).
#' @param detalha_mes [logical] TRUE para detalhar dados por mês.
#' @param tipo_op [char] "exp" (exportação) ou "imp" (importação).
#' @param tipo_ord [char] "val" (por valor) ou "det" (por detalhe).
#' @param filtros [char[]] Vetor com nomes de filtros (ex.: c("pais", "mun")).
#' @param filtros_esp [list] Lista de vetores com valores para cada filtro.
#' @param detalhamentos [char[]] Vetor com campos de detalhamento (ex.: c("pais", "mun")).
#' @param faixa [logical] TRUE para filtro por faixa (só para sh4, sh2, secao).
#' @param valor_FOB [logical] TRUE para incluir valor FOB.
#' @param valor_kg [logical] TRUE para incluir valor em kg.
#' @return Data frame com resultados ou mensagem de erro.
#' @export
pesquisar_comex_stat_mun <- function(
    ano_inicial   = as.integer(format(Sys.Date(), "%Y")),
    ano_final     = as.integer(format(Sys.Date(), "%Y")),
    mes_inicial   = 1,
    mes_final     = 12,
    detalha_mes   = FALSE,
    tipo_op       = "exp",
    tipo_ord      = "val",
    filtros       = character(),
    filtros_esp   = list(),
    detalhamentos = character(),
    faixa         = FALSE,
    valor_FOB     = TRUE,
    valor_kg      = TRUE
) {
  # Validações básicas
  if (!is.numeric(ano_inicial) || ano_inicial %% 1 != 0) stop("`ano_inicial` deve ser inteiro.")
  if (!is.numeric(ano_final)   || ano_final %% 1   != 0) stop("`ano_final` deve ser inteiro.")
  if (!is.numeric(mes_inicial) || mes_inicial %% 1 != 0 || mes_inicial < 1 || mes_inicial > 12) stop("`mes_inicial` deve estar entre 1 e 12.")
  if (!is.numeric(mes_final)   || mes_final %% 1   != 0 || mes_final   < 1 || mes_final   > 12) stop("`mes_final` deve estar entre 1 e 12.")
  if (mes_inicial > mes_final) stop("`mes_inicial` deve ser ≤ `mes_final`.")
  if (!tipo_op %in% c("exp","imp")) stop("`tipo_op` deve ser 'exp' ou 'imp'.")
  if (!tipo_ord %in% c("val","det")) stop("`tipo_ord` deve ser 'val' ou 'det'.")
  if (length(filtros) != length(filtros_esp)) stop("`filtros` e `filtros_esp` devem ter mesmo comprimento.")

  # Mapeamento filtros ⇆ nomes de API
  mapeamento <- list(
    pais   = "noPaispt", blocos = "noBlocopt", uf  = "noUf",
    mun    = "noMunMin",  sh4    = "noSh4pt",   sh2 = "noSh2pt",
    secao  = "noSecpt"
  )

  # Formatação de meses
  mes_inicial <- sprintf("%02d", mes_inicial)
  mes_final   <- sprintf("%02d", mes_final)

  # Conversão de parâmetros para API
  tipo_op_num  <- switch(tipo_op, exp = 1, imp = 2)
  tipo_ord_num <- switch(tipo_ord, val = 1, det = 2)

  # Construção de filtros e detalhamentos
  filterList  <- lapply(filtros, function(it) list(id = mapeamento[[it]]))
  filterArray <- list()
  rangeFilter <- list()
  for (i in seq_along(filtros)) {
    it  <- filtros[i]
    api <- mapeamento[[it]]
    vals <- as.character(filtros_esp[[i]])
    if (faixa && it %in% c("sh4","sh2","secao")) {
      filterArray[[i]] <- list(item = list(), idInput = api)
      rangeFilter[[length(rangeFilter)+1]] <- list(
        id    = api,
        value = list(rangeOne = vals[1], rangeTwo = vals[2])
      )
    } else {
      filterArray[[i]] <- list(item = vals, idInput = api)
    }
  }
  detailDatabase <- lapply(detalhamentos, function(it) list(id = mapeamento[[it]], text = ""))

  # Montagem do corpo da requisição
  body_list <- list(
    yearStart      = ano_inicial,
    yearEnd        = ano_final,
    typeForm       = tipo_op_num,
    typeOrder      = tipo_ord_num,
    filterList     = filterList,
    filterArray    = filterArray,
    detailDatabase = detailDatabase,
    rangeFilter    = rangeFilter,
    monthDetail    = detalha_mes,
    metricFOB      = valor_FOB,
    metricKG       = valor_kg,
    monthStart     = mes_inicial,
    monthEnd       = mes_final,
    formQueue      = "city",
    langDefault    = "pt"
  )

  filtro_json <- jsonlite::toJSON(body_list, auto_unbox = TRUE)
  url_final   <- paste0(
    "https://api-comexstat.mdic.gov.br/cities?filter=",
    utils::URLencode(iconv(filtro_json, to = "UTF-8", sub = "byte"))
  )

  # Requisição e tratamento de erros
  resposta <- httr::RETRY("GET", url_final, times = 3, pause_base = 1)
  switch(as.character(resposta$status_code),
         "413" = stop("A consulta retornou dados demais. Refine os filtros."),
         "429" = stop("Muitas requisições. Aguarde e tente novamente."),
         "500" = stop("Erro interno da API."),
         "504" = stop("Tempo de resposta excedido. Reduza o escopo da consulta."),
         httr::stop_for_status(resposta, task = "consultar Comex Stat municípios")
  )

  # Processamento da resposta
  dados_txt <- httr::content(resposta, "text", encoding = "UTF-8")
  dados     <- jsonlite::fromJSON(dados_txt, flatten = TRUE)[[1]][[1]]
  df        <- as.data.frame(dados)

  if (nrow(df) == 0) return("Nenhum resultado retornado. Verifique os parâmetros.")

  class(df) <- c("comexstat_df", class(df))
  attr(df, "params") <- list(
    data_consulta = Sys.time(), url_consulta = url_final,
    ano_inicial, ano_final, mes_inicial, mes_final,
    detalha_mes, tipo_op, tipo_ord,
    filtros, filtros_esp, detalhamentos, faixa,
    incluir_fob = valor_FOB, incluir_kg = valor_kg
  )

  return(df)
}
