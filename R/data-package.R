###############################################################################
###                                                                         ###
###                           Script                                        ###
###               DOCUMENTACAO - DADOS - PACKAGE                            ###
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
###  Author: Ismael Neu                                                     ###
###  Date: 04/06/2021                                                       ###
###  Version: 1.0.1                                                         ###
###  E-mail: ismaelmmneu@hotmail.com                                        ###
###############################################################################

###############################################################################
# O exemplo a seguir está presente no livro de Cruz e Regazzi, 1997 - Cap 8
# -----------------------------------------------------------------------------
#' @title Médias de rendimento de dez clones de cana de açúcar avaliados em três épocas.
#'
#' @name clones_cana
#'
#' @description O `data.frame` \code{clones_cana}, apresenta a médias de rendimento de
#'     dez clones de cana de açúcar avaliados em três épocas.
#'
#' @format Um \code{data.frame} com 30 observações:
#'    \describe{
#'      \item{\code{Clone}}{Fator de 10 níveis qualitativos, usado para
#'     identificar os clones de cana de açúcar.}
#'     \item{\code{Epoca}}{Fator de 3 níveis qualitativos, usado para
#'     identificar a época  de avaliação.}
#'     \item{\code{Taxa}}{Médias de rendimento}
#'    }
#'
#' @keywords datasets Repetibilidade
#'
#' @references Cruz, Cosme Damião & Regazzi, Adair José (1997). Modelos biométricos
#'    aplicados ao melhoramento genético, Viçosa - UFV, 390p. (Tabela 8.6, p. 376)
#'
#' @docType data
#' @usage data(clones_cana)
#'
NULL
# clones_cana <- data.frame(matrix(data = c(18.33, 18.55, 19.34,
#                                           18.34, 18.99, 18.6,
#                                           19.89, 19.84, 20.82,
#                                           15.86, 17.45, 16.97,
#                                           19.78, 19.81, 20.86,
#                                           19.40, 19.36, 20.74,
#                                           20.57, 20.04, 21.44,
#                                           18.99, 19.10, 19.98,
#                                           18.45, 18.31, 18.81,
#                                           17.96, 18.61, 19.44), nrow = 10, ncol = 3, byrow = TRUE,
#                                  dimnames = list(1:10, paste0('E', 1:3))))
