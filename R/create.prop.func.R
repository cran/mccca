#' Creates a list length J of category proportion for each cluster.
#'
#' @description Creates a list length J of category proportion for each cluster.
#' @param J The number of active variable.!!!
#' @param q.vec A vector of length J giving the number of categories for each active variable.
#' @param Ktrue The number of clusters in J active variables.
#' @param strongprop A numeric value giving the strongest proportion of categories (common for all J active variables).
#' @param which.noise A vector of length (<= J) giving the index of noise variables in J active variables. NULL indicating all variable is non-noise.
#' @return Returns a list length J, each of which is a (Ktrue x qj) matrix giving the proportion for each qj category in each Ktrue cluster.
#' @importFrom stats runif
#' @export

create.prop <- function(J=J, q.vec=q.vec, Ktrue=Ktrue,
                             strongprop=0.8,which.noise=NULL){

  ####logical matrix to see if each cluster & category variable is noise vari or not
  noiseTF.mat <- matrix(FALSE,Ktrue,J)
  if(is.null(dim(which.noise))){#if noiseTF is vector.
    which.noise <- matrix(which.noise,1,length(which.noise))
  }

  if(!is.null(which.noise)){#if which.noise is not NULL, make variables partly noise
    if(nrow(which.noise)==1){#if noiseTF is vector.
      noiseTF.mat[,which.noise[1,]] <- TRUE
    }else{
      for(kk in 1:Ktrue) noiseTF.mat[kk,which.noise[kk,]] <- TRUE
    }
  }

  prop.J.list <- rep(list(NA),J)
  names(prop.J.list) <- paste("vari",c(1:J),sep="")

  for(jj in 1:J){

    qj <- q.vec[jj]
    prop.J.list[[jj]] <- matrix(NA,Ktrue,qj)
    rownames(prop.J.list[[jj]]) <- paste("cls",c(1:Ktrue),sep="")
    colnames(prop.J.list[[jj]]) <- paste("cate",c(1:qj),sep="")

    ########only propvec is used.(4/16)########
    prn <- min(Ktrue,4)
    propmat <- matrix(NA, 4, qj)
    where.highcate <- sample(qj,Ktrue,replace=TRUE)

    for(kk in 1:Ktrue){#specify probability for qj categories for each cluster
      cateord <- sample((qj-1),(qj-1),replace=FALSE)
      propmat[kk,where.highcate[kk]] <- strongprop

      propvec.moto <- runif((qj-1),0,(1-strongprop))
      propvec <- (propvec.moto/sum(propvec.moto))*(1-strongprop)
      propmat[kk,-where.highcate[kk]] <- propvec
    }

    ###end set prop

    ###for non-noise variable, use prob. in propmat, if noise, use uniform probability.
    for(kk in 1:Ktrue){
      noisetf <- noiseTF.mat[kk,jj]
      if(!noisetf){
        prop.J.list[[jj]][kk,] <- propmat[kk,]
      }else{
        prop.J.list[[jj]][kk,] <- rep(1/qj,qj)
      }
    }
  }#end vari

    prop.J.list

}
