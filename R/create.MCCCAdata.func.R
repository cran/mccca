#' this function creates a list (class: mcccadata) to be applied to MCCCA.
#'
#' @description Creates a list (named \code{mcccadata.list}) applied to MCCCA.
#' @usage create.MCCCAdata(dat,ext.mat=ext.mat,clstr0.vec=NULL)
#' @param dat An (NxJ) matrix of categorical data (N:the number of observations, J:the number of variables). If \code{rownames(dat)} is \code{NULL}, \code{c(obj1,..,objN)} are defined as \code{rownames(dat)}.
#' @param ext.mat An (NxH) external variable matrix (H:the number of external variable).
#' @param clstr0.vec An integer vector of length N giving each observation's true cluster.
#' @return Returns a list with the following elements.
#' \item{\code{data.mat}}{data matrix same as \code{dat}.}
#' \item{\code{data.list}}{A list of C (NxJ) categorical data matrices for each class (C:the number of classes).}
#' \item{\code{clstr0.list}}{A list of C vectors where each vector indicates the true cluster (given in \code{clstr0.vec}) to which each class of observations belongs (NULL if \code{clstr0.vec} is NULL).}
#' \item{\code{N.vec}}{A vector of length C giving the number of observations in each class.}
#' \item{\code{Ktrue.vec}}{A vector of length C giving the true number of clusters in each class  (NULL if \code{clstr0.vec} is NULL).}
#' \item{\code{q.vec}}{A vector of length J giving the number of categories in each of J categorical variables.}
#' \item{\code{class.n.vec}}{An integer (from 1:C) vector of length N giving the class index of each observation. \code{names(class.n.vec)=rownames(dat)}.}
#' \item{\code{classname.n.vec}}{A characteristic vector of length N giving the class label each observation belongs to. \code{names(classname.n.vec)=rownames(dat)}.}
#' \item{\code{classlabel}}{A characteristic vector of length C giving the classlabel for each class.}
#' \item{\code{classlab.mat}}{(Cx(H+1)) table, showing which combinations of categories of external variables each class index and class name corresponds to. The first H columns indicate the categories for each of the H external variables, and the last H+1th column indicates the corresponding class label (same as \code{classlabel}).}
#' \item{\code{oriindex.list}}{A list of length C, where each list element corresponds to a row (observation) in data.list, indicating which row of observations (in \code{data.mat}) each observation (in \code{oriindex.list}) corresponds to.}
#' @export
#' @references Takagishi & Michel van de Velden (2022): Visualizing Class Specific
#' Heterogeneous Tendencies in Categorical Data, Journal of Computational and Graphical Statistics,
#' DOI: 10.1080/10618600.2022.2035737
#' @examples
#' #setting
#' N <- 100 ; J <- 5 ; Ktrue <- 2 ; q.vec <- rep(5,J) ; noise.prop <- 0.2
#' extcate.vec=c(2,3)#the number of categories for each external variable
#'
#' #generate categorical variable data
#' catedata.list <- generate.onedata(N=N,J=J,Ktrue=Ktrue,q.vec=q.vec,noise.prop = noise.prop)
#' data.cate=catedata.list$data.mat
#' clstr0.vec=catedata.list$clstr0.vec
#'
#' #generate external variable data
#' data.ext=generate.ext(N,extcate.vec=extcate.vec)
#'
#' #create mccca.list to be applied to MCCCA function
#' mccca.data=create.MCCCAdata(data.cate,ext.mat=data.ext,clstr0.vec =clstr0.vec)
#'
#' #check which class each observation belongs to. (given by class name)
#' mccca.data$classname.n.vec
#'
#' #A table showing that which combinations of categories of external variables
#' # each class index and class name corresponds to.
#' mccca.data$classlab.mat


## @references Takagishi, M. & Velden, M. van de (2022). Visualizing class specific heterogeneous tendencies in categorical data, to appear in Journal of Computational and Graphical Statistics.


create.MCCCAdata <- function(dat,ext.mat=ext.mat,clstr0.vec=NULL){

  ###data seibi before splitting data
  if(is.null(rownames(dat))) rownames(dat)=paste0("obj",c(1:nrow(dat)))
  if(!is.data.frame(dat))dat=as.data.frame(dat)

  ######first split data by the combinations of categories of external variables.
  split.list=split.data.byext.comb(dat,ext.mat=ext.mat,clstr0.vec =clstr0.vec )
  data.list=split.list$data.list
  clstr.list=split.list$clstr.list
  clstr0.list=split.list$clstr0.list
  clstr.vec=split.list$clstr.vec
  class.n.vec=split.list$class.n.vec
  classname.n.vec=split.list$classname.n.vec
  oriindex.list=split.list$oriindex.list
  N.vec=split.list$N.vec ; Ktrue.vec=split.list$Ktrue.vec

  H=ncol(ext.mat)
  classlab.mat=split.list$classlab.mat
  classlabel=classlab.mat[,c(H+1)]
  rm(split.list)

  C <- length(data.list) ; J <- ncol(data.list[[1]])
  q.vec=sapply(c(1:J),function(x){length(unique(dat[,x]))})

  ###output
  res <- list(data.mat=dat,data.list=data.list,#clstr.list=clstr.list,clstr.vec=clstr.vec,
              clstr0.list=clstr0.list,N.vec=N.vec,q.vec=q.vec,Ktrue.vec=Ktrue.vec,#q.vec.J=q.vec.J,
       class.n.vec=class.n.vec,classlabel=classlabel,classlab.mat=classlab.mat#,,classlabel.short=classlabel.short
       ,classname.n.vec=classname.n.vec,oriindex.list=oriindex.list)#extcate.list=extcate.list)#,dummy.all=dummy.all)
  class(res) <- "mcccadata"

  res

}

