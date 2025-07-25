#' @title Tabelas auxiliares do Comex Stat
#' @name Tabelas_Auxiliares
#' @rdname Tabelas_Auxiliares
#' @encoding UTF-8
#' @description
#' Pesquisa padrões de códigos e descrições das tabelas auxiliares do Comex Stat.
#' Use os wrappers `pesquisar_ncm()`, `pesquisar_sh6()` etc. para cada tabela.
#' @param pattern Trecho de texto ou código para buscar (opcional).
#' @return Data frame com colunas específicas de cada tabela ou mensagem de erro.
#' @export
consultar_tabela_auxiliar <- function(
    pattern     = "",
    endpoint    = stop("endpoint obrigatório"),
    filter_id   = stop("filter_id obrigatório"),
    col_names   = c("ID", "Descrição"),
    trim_chars  = NULL,
    distinct_id = FALSE
) {
  params <- list(
    term   = pattern,
    filter = jsonlite::toJSON(list(id = filter_id), auto_unbox = TRUE)
  )
  url <- paste0("https://api-comexstat.mdic.gov.br/pt/", endpoint)
  resp <- httr::GET(url = url, query = params)
  httr::stop_for_status(resp, task = paste("consultar tabela", filter_id))
  data_txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  df <- jsonlite::fromJSON(data_txt, flatten = TRUE)
  if (length(df) == 0 || nrow(df) == 0) return("Sem resultados")
  df <- as.data.frame(df)
  if (!is.null(trim_chars) && "text" %in% names(df)) {
    df$text <- substring(df$text, trim_chars)
  }
  if (distinct_id) df <- dplyr::distinct(df, id, .keep_all = TRUE)
  # Seleciona e renomeia
  keep <- intersect(names(df), c("id", "text"))
  df <- df[, keep, drop = FALSE]
  colnames(df) <- col_names
  return(df)
}

# Wrappers gerados automaticamente:
#' @rdname Tabelas_Auxiliares
pesquisar_ncm <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product/ncm", "noNcmpt", c("NCM", "Descrição"), trim_chars = 12)

#' @rdname Tabelas_Auxiliares
pesquisar_sh6 <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "harmonized-system/subposition", "noSh6pt", c("SH6", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_sh4 <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "harmonized-system/position", "noSh4pt", c("SH4", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_sh2 <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "harmonized-system/chapter", "noSh2pt", c("SH2", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_ncm_sec <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "harmonized-system/section", "noSecpt", c("NCM_SEC", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_pais <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "location/countries", "noPaispt", c("CO_PAIS", "País"), distinct_id = TRUE)

#' @rdname Tabelas_Auxiliares
pesquisar_blocos <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "location/blocks", "noBlocopt", c("CO_BLOCO", "Bloco"), distinct_id = TRUE)

#' @rdname Tabelas_Auxiliares
pesquisar_uf <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "location/states", "noUf", c("CO_UF", "UF"))

#' @rdname Tabelas_Auxiliares
pesquisar_mun <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "location/cities", "noMunMin", c("CO_MUN", "Município"))

#' @rdname Tabelas_Auxiliares
pesquisar_via <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "location/via", "noVia", c("CO_VIA", "Via"))

#' @rdname Tabelas_Auxiliares
pesquisar_urf <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "location/urf", "noUrf", c("CO_URF", "URF"), trim_chars = 11)

#' @rdname Tabelas_Auxiliares
pesquisar_cgce3 <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product-description/cgce-n3", "noCgceN3pt", c("CO_CGCE_N3", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_cgce2 <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product-description/cgce-n2", "noCgceN2pt", c("CO_CGCE_N2", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_cgce1 <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product-description/cgce-n1", "noCgceN1pt", c("CO_CGCE_N1", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_cuci_item <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product-category/item", "noCuciItempt", c("CO_CUCI_ITEM", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_cuci_sub <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product-category/subposition", "noCuciSubpt", c("CO_CUCI_SUBGRUPO", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_cuci_pos <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product-category/position", "noCuciPospt", c("CO_CUCI_POSICAO", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_cuci_cap <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product-category/chapter", "noCuciCappt", c("CO_CUCI_CAPITULO", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_cuci_sec <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product-category/section", "noCuciSecpt", c("CO_CUCI_SECAO", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_isic_classe <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product-classification/class", "noIsicClasspt", c("CO_ISIC_CLASSE", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_isic_grupo <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product-classification/group", "noIsicGrouppt", c("CO_ISIC_GRUPO", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_isic_div <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product-classification/division", "noIsicDivisionpt", c("CO_ISIC_DIVISAO", "Descrição"))

#' @rdname Tabelas_Auxiliares
pesquisar_isic_sec <- function(pattern = "")
  consultar_tabela_auxiliar(pattern, "product-classification/section", "noIsicSectionpt", c("CO_ISIC_SECAO", "Descrição"))
