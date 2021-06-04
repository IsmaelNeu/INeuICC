###############################################################################
#####                                                                    ######
#####                           Script                                   ######
#####                     COMPONENTES PRINCIPAIS                         ######
#####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++######
#####  Author: Ismael Neu                                                ######
#####  Date: 18/05/2020                                                  ######
#####  Version: 1.0.1                                                    ######
#####  E-mail: ismaelmmneu@hotmail.com                                   ######
###############################################################################
#'
#' @title Analise de Repetibilidade - Componentes Principais - Correlacao
#'
#' @name rpt_cp_correl
#'
#' @author Ismael Neu (ismaelmmneu@@hotmail.com)
#'
#' @description A funcao estima o coeficiente de repetibilidade, utilizando o metodo
#'     dos componentes principais e a matriz de correlacoes.
#' @param dados Data frame. Matriz de dados.
#' @param A String. Nome da variavel correspondendo a coluna da categoria Ambiente.
#' @param G String. Nome da variavel correspondendo a coluna da categoria Genotipo.
#' @param Y String. Nome da variavel correspondendo a coluna da categoria a
#'    Variavel Resposta.
#' @param R2 Numeric. Coeficiente de determinacao.
#'
#' @references Cruz, Cosme Damiao & Regazzi, Adair Jose (1997). Modelos biometricos
#'    aplicados ao melhoramento genetico, Vicosa - UFV, 390p.
#'
#'
#' @export

rpt_cp_correl <- function(dados, A, G, Y, R2 = .95){
  # x: matriz de dados
  # A = "Ambiente"
  # G = "Geno"
  # Y = "Resp"

  x <- dados %>% dplyr::select(A, G, Y)
  colnames(x) <- c('A', 'G', 'Y')

  matdados <- x %>% reshape2::dcast(G ~ A) %>% dplyr::select(-G)
  R <- stats::cor(matdados, use="complete.obs", method="pearson")


  eigval <- eigen(R)$values
  tbeigvect <- eigen(R)$vectors

  # tabeigen <-  data.frame(cbind(eigval, eigval/sum(eigval), t(tbeigvect)))
  dimtb <- dim(tbeigvect)[1]
  # sinal entre valores autovetor
  tftb1 <- tbeigvect < 0
  for (j in 1:dimtb){

    linha <- 1
    for (i in 1:dimtb){
      if(length(unique(tftb1[, i])) == 1){
        igual <- 'igual'
        break
      }else{
        igual <- 'diferente'
      }
      linha <- linha + 1
    }
  }
  # testa condicoes para autovetor:
  # 1. mesmo sinal
  # 2. mesma magnitude
  mineigvect <- apply(X = tbeigvect, MARGIN = 2, FUN = min)
  maxeigvect <- apply(X = tbeigvect, MARGIN = 2, FUN = max)
  releig <- abs(mineigvect/maxeigvect)
  if(linha <= dimtb && igual == 'igual' && releig >= .95){
    reptb <-  as.numeric((eigval[linha]-1)/(dimtb-1))

    n0 <- as.numeric((R2*(1-reptb))/((1-R2)*reptb))
  }else{
    reptb <- NA
    n0 <- NA
  }
  # salva lista de resultados
  saida <- list(
    Eigen <- list(eigen(R)),
    r = reptb,
    R2 = R2,
    n0 = n0
  )
  return(saida)
}
