###############################################################################
###                                                                         ###
###                           Script                                        ###
###                     COMPONENTES PRINCIPAIS                              ###
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
###  Author: Ismael Neu                                                     ###
###  Date: 04/06/2021                                                       ###
###  Version: 1.0.1                                                         ###
###  E-mail: ismaelmmneu@hotmail.com                                        ###
###############################################################################
#' @encoding UTF-8
#' @title Análise de Reptibilidade - Componentes Principais
#' @name rpt_pca
#' @description Análise de repetibilidade usando a metodologia dos componentes
#'     principais.
#' @param x Data frame ou matriz, contendo as observações dos ensaios ou
#'     ambientes em cada coluna, ou a matriz de correlação se \code{correl = TRUE}
#'     ou a matriz variância e covariância se \code{correl = FALSE}.
#' @param correl Lógico, definindo de a determinação é realizada por meio da
#'     matriz de correlação - `default` se \code{correl = TRUE} ou matriz de
#'     variância e covariâncias se \code{correl = FALSE}.
#' @param tolerance Numérico, diferença tolerada para que a condição de que as
#'     observações de cada autovetor tenham o mesmo sinal e  magnitude semelhante.
#'     Por `default`, tem-se que \code{tolerance = 0.5}.
#' @details O método proposto por Abeywardena (1972) consiste em se obter a
#'     matriz de correlações e desta, os autovalores e os autovetores normalizados.
#'     O autovetor cujos elementos possuem o mesmo sinal e magnitudes próximas é
#'     aquele que expressa a tendência em manter as suas posições relativas nos
#'     vários ensaios ou pesquisas no tempo.
#'     O estimador de Abeywardena é obtido baseando-se na pressuposição de que
#'     o coeficiente de repetibilidade é dado pela correlação entre cada par de
#'     medições avaliadas nos diferentes genótipos. A estimativas do coeficiente de
#'     repetibilidade (r), são as seguintes ao serem estimadas a partir da matriz
#'     de correlações e covariância, respectivamente:
#'     \deqn{r = \frac{\hat{\lambda_{1}} -1 }{\eta - 1}}
#'     \deqn{r = \frac{\hat{\sigma}_{1} - \hat{\sigma_{Y}^{2}}}{- \hat{\sigma_{Y}^{2}} (\eta - 1)}}
#' @examples
#' data("clones_cana")
#' # Obtido usando a matriz de correlação
#' rpt_pca(x = clones_cana) # 0.9537408
#'
#' # Obtido usando a matriz de covariância
#' rpt_pca(x = clones_cana, correl = FALSE) # 0.9590025
#' @export
rpt_pca <- function(x, correl = TRUE, tolerance = 0.5){
  #########################################################
  # 1. Funcao auxiliar - obtenção do indice
  # verifica se elementos do autovetor tem o mesmo sinal e  magnitude semelhante
  # -------------------------------------------------------
  fc_indice <- function(matriz, tolerance){
    # indice para selecao do autovetor
    indice <- NA
    # autovetores da matriz
    eigvector <- eigen(matriz)$vectors

    for(i in 1:ncol(eigvector)){
      # seleciona cada autovetor
      autovet <- eigvector[, i]
      if(abs(sum(sign(autovet))) == nrow(eigvector) &
         abs(abs(sum(autovet)) - abs(autovet[1] * nrow(eigvector))) < tolerance
      ){
        indice <-  i
        break
      }

    }
    return(indice)
  }

  #########################################################
  # 2. PCA - Matriz de Correlacao
  # -------------------------------------------------------
  if(correl){
    # verifica se x eh um data frame contendo as observacoes ou a matriz de correlacoes ou covariancia
    if(dim(x)[1] != dim(x)[2]){
      mat_cor <- stats::cor(x)
    }else{
      mat_cor <- x
    }
    # auto valores da matriz
    eigvalue_R <- eigen(mat_cor)$values
    # verifica se elementos do autovetor tem o mesmo sinal e  magnitude semelhante
    indice <- fc_indice (mat_cor, tolerance)

    # determina o coef repetibilidade
    if(is.na(indice)){
      result <- NA
    }else{
      result <- (eigvalue_R[indice] - 1)/(nrow(mat_cor) - 1)
    }

  }
  #########################################################
  # 3. PCA - Matriz de Variancia Covariancia
  # -------------------------------------------------------
  if(correl == FALSE){
    # verifica se x eh um data frame contendo as observacoes ou a matriz de correlacoes ou covariancia
    if(nrow(x) != ncol(x)){
      mat_cov <- stats::cov(x)
    }else{
      mat_cov <- x
    }
    # auto valores da matriz
    eigvalue_S <- eigen(mat_cov)$values
    # erro experimental da variavel resposta
    varY <- sum(diag(mat_cov)) / nrow(mat_cov)
    # verifica se elementos do autovetor tem o mesmo sinal e  magnitude semelhante
    indice <- fc_indice (mat_cov, tolerance)

    # determina o coef repetibilidade
    if(is.na(indice)){
      result <- NA
    }else{
      result <- (eigvalue_S[indice] - varY) / (varY * (nrow(mat_cov) - 1))
    }


  }

  #########################################################
  # 4. Resultado a ser retornado
  # --------------------------
  return(result)
}


