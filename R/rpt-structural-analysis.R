###############################################################################
###                                                                         ###
###                           Script                                        ###
###                        ANALISE ESTRUTURAL                               ###
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
###  Author: Ismael Neu                                                     ###
###  Date: 04/06/2021                                                       ###
###  Version: 1.0.1                                                         ###
###  E-mail: ismaelmmneu@hotmail.com                                        ###
###############################################################################
#' @encoding UTF-8
#' @title Análise de Reptibilidade - Análise Estrutural
#' @name rpt_sctructural
#' @description Análise de repetibilidade usando a metodologia de análise
#'     estrutural.
#' @param x Data frame ou matriz, contendo as observações dos ensaios ou
#'     ambientes em cada coluna, ou a matriz de correlação se \code{correl = TRUE}
#'     ou a matriz variância e covariância se \code{correl = FALSE}.
#' @param correl Lógico, definindo de a determinação é realizada por meio da
#'     matriz de correlação - `default` se \code{correl = TRUE} ou matriz de
#'     variância e covariâncias se \code{correl = FALSE}.
#' @details O método proposto por Mansour, Nordhein e Rutledge (1981) apresenta
#'     apenas diferenças conceituais em relação ao método dos componentes
#'     principais.
#'     Neste método, considera-se R a matriz paramétrica de corrreções entre
#'     os genótipos em cada par  de avaliações. Um estimador do coeficiente de
#'     repetibilidade baseado nos componentes principais é:
#'     \deqn{r = \frac{\hat{\lambda}_{1}  - 1}{\eta - 1} = \frac{\hat{\alpha}' \hat{R} \hat{\alpha}}{\eta - 1}}
#'     e para matriz de covariância:
#'     \deqn{r = \frac{\alpha' \Gamma \alpha - \hat{\sigma_{Y}^{2}}}{\hat{\sigma_{Y}^{2}} (\eta - 1)}}
#'
#' @examples
#' data("clones_cana")
#' # Obtido usando a amtriz de correlação
#' rpt_sctructural(x = clones_cana) # 0.9537291
#'
#' # Obtido usando a matriz de covariância
#' rpt_sctructural(x = clones_cana, correl = FALSE) # 0.8907801
#' @export
rpt_sctructural <- function(x, correl = TRUE){

  #########################################################
  # 1. PCA - Matriz de Correlacao
  # -------------------------------------------------------
  if(correl){
    # verifica se x eh um data frame contendo as observacoes ou a matriz de correlacoes ou covariancia
    if(nrow(x) != ncol(x)){
      mat_cor <- stats::cor(x)
    }else{
      mat_cor <- x
    }

    # 1.1. versao 1
    # k <- (nrow(mat_cor) * (nrow(mat_cor) - 1)) / 2
    # sum_r <- sum(cor[upper.tri(cor)])
    # result <- sum_r / k

    # 1.2. versao 2
    vetor <- matrix(rep(as.numeric(1 / sqrt(nrow(mat_cor))), nrow(mat_cor)), nrow = nrow(mat_cor))
    result <- (as.vector(t(vetor) %*% mat_cor %*% vetor) - 1) / (nrow(mat_cor) - 1)
  }
  #########################################################
  # 2. PCA - Matriz de Variancia Covariancia
  # -------------------------------------------------------
  if(correl == FALSE){
    # verifica se x eh um data frame contendo as observacoes ou a matriz de correlacoes ou covariancia
    if(nrow(x) != ncol(x)){
      mat_cov <- stats::cov(x)
    }else{
      mat_cov <- x
    }
    # auto valores da matriz
    vetor <- matrix(rep(as.numeric(1 / sqrt(nrow(mat_cov))), nrow(mat_cov)), nrow = nrow(mat_cov))
    varY <- sum(diag(mat_cov)) / nrow(mat_cov)
    result <- ((as.vector(t(vetor) %*% mat_cov %*% vetor)) - varY) / (varY * (nrow(mat_cov) - 1))
  }

  #########################################################
  # 3. Resultado a ser retornado
  # --------------------------
  return(result)
}
