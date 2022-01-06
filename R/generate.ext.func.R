#' generates an artificial (NxH) external variable matrix.
#'
#' @description Generates an artificial (NxH) external variable matrix.
#' @usage generate.ext(N,extcate.vec=extcate.vec,unbala.cate=FALSE)
#' @param N The number of observation.
#' @param extcate.vec A vector of length H, each element indicates the number of category for each H external variables.
#' @param unbala.cate logical value. If TRUE, the proportion of categories in the external variable is unbalanced. The default is FALSE.
#' @return An (NxH) external variable matrix.
#' @seealso \code{\link{generate.catecls}}
#' @export
#' @examples
#' ###data setting
#' N <- 30 ; extcate.vec=c(2,3)
#' ext.mat=generate.ext(N,extcate.vec=extcate.vec)


generate.ext <- function(N,extcate.vec=extcate.vec,unbala.cate=FALSE){

  maxcate=10
  if(any(extcate.vec>maxcate)){
    stop("external variable having only less than 10 categories can be generated")
  }

  H <- length(extcate.vec)

  ####generate proportion for external variables (for the case 1:maxcate categories)#######
  prop.ext.list <- rep(list(NA),maxcate) #10 is maximum # of category in extvari
  for(ssup in 1:maxcate){
    if(unbala.cate){
      prop.ext.list[[ssup]] <- c(1:ssup)/sum(1:ssup)
    }else{
      prop.ext.list[[ssup]] <- rep(1/ssup,ssup)
    }}

  #######generate class by each extvari#########
  ext.mat <- matrix(NA,N,H)

  for(hh in 1:H){
    extcate <- extcate.vec[hh]
    probsup <- prop.ext.list[[extcate]]
    ext.mat[,hh] <- sample(extcate,N,replace=TRUE,prob=probsup)
    #ndatavec.sup <- as.numeric(table(supmat[,hh]))
  }

  colnames(ext.mat) <- paste("ext",seq(1,H),sep="")
  rownames(ext.mat) <- paste0("obj",c(1:N))

  ext.mat

}
