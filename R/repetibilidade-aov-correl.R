###############################################################################
#####                                                                    ######
#####                           Script                                   ######
#####              FUNCAO REPETIBILIDADE: ANOVA - CORRELACAO             ######
#####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++######
#####  Author: Ismael Neu                                                ######
#####  Date: 11/05/2020                                                  ######
#####  Version: 1.0.1                                                    ######
#####  E-mail: ismaelmmneu@hotmail.com                                   ######
###############################################################################
#'
#' @title Analise de Repetibilidade - ANOVA unifatorial
#'
#' @name rpt_aov_fat1
#'
#' @author Ismael Neu (ismaelmmneu@@hotmail.com)
#'
#' @description Estima o coeficiente de repetibilidade, a analise de variancia.
#'     Considera modelo com efeito de genotipo.
#'
#' @param dados Data frame. Matriz de dados.
#' @param A String. Nome da variavel correspondendo a coluna da categoria Ambiente.
#' @param G String. Nome da variavel correspondendo a coluna da categoria Genotipo.
#' @param Y String. Nome da variavel correspondendo a coluna da categoria a
#'    Variavel Resposta.
#' @param R2 Numeric. Coeficiente de determinacao.
#' @details Modelo unifatorial utilizado: \eqn{Y_{ij} = m + G_i + e_{ij}}
#'
#' @references Cruz, Cosme Damiao & Regazzi, Adair Jose (1997). Modelos biometricos
#'    aplicados ao melhoramento genetico, Vicosa - UFV, 390p.
#'
#'
#' @export

rpt_aov_fat1 <- function(dados, A, G, Y, R2 = .95){
  # x: matriz de dados
  # A = "Ambiente"
  # G = "Geno"
  # Y = "Resp"

  x <- dados %>% dplyr::select(A, G, Y)
  colnames(x) <- c('A', 'G', 'Y')
  resmod <- summary(stats::aov(Y~G, data = x))
  ni <- x %>% stats::na.omit() %>% dplyr::group_by(A) %>% dplyr::summarise(length(Y))
  N <- x %>% stats::na.omit() %>% dplyr::summarise(length(Y))
  p <- length(unique(x$A))
  k <- (N-(sum(ni[ , 2]^2)/N[[1]]))/(p-1)
  varE <- resmod[[1]][2, 3]
  varG <- (resmod[[1]][1, 3] - varE)/k
  reptb <- as.numeric(varG/(varE + varG))
  # num medicoes
  n0 <- as.numeric((R2*(1-reptb))/((1-R2)*reptb))
  # salva lista com resultados
  saida <- list(
    aovtab <- resmod[[1]],
    r = reptb,
    R2 = R2,
    n0 = n0
  )
  return(saida)
}
