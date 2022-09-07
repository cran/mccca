updateQcate.MCCCA <- function(dummy.mat=dummy.mat,Ddiag.sq=Ddiag.sq#dummy.diag=dummy.diag,
                             ,Ugrp=Ugrp,m=m,lowdim=lowdim,ncatevec=NULL
                             ,grpbase=F,printcheck=1){#,objcoord=objcoord

  ###D
  n.all<-nrow(Ugrp)
  #m<-ncol(Ggrp)

  #q.all<-ncol(dummy.mat)
  Jn <- diag(n.all)-(1/n.all)*(rep(1,n.all)%*%t(rep(1,n.all)))
  #Ddiag<-matrix(0,q.all,q.all)

  #browser()

  #browser()
  Yn <- t(Ugrp)%*%Jn%*%dummy.mat

  DZZD<-(1/m)*((Ddiag.sq)%*%t(Yn)%*%solve(t(Ugrp) %*% Ugrp)%*%(Yn%*%Ddiag.sq))
  #grpals
  #DZZD<-(1/(m)^2)*((Ddiag.sq)%*%t(Yn)%*%solve(t(Ugrp)%*%Ugrp)%*%Yn%*%Ddiag.sq)

  #cluster CA
  #DZZD<-(1/m)*((Ddiag.sq)%*%t(Yn)%*%solve(t(Ugrp)%*%Ugrp)%*%Yn%*%Ddiag.sq)

  eig.res<-eigen(DZZD)
  #all(DZZD==t(DZZD))
  #if(printcheck>2) cat("    eig value of DZZD",eig.res$values[c(1:5)],"\n")

  #eig.res
  B.star<-eig.res$vectors[,c(1:lowdim)]#[,c(q.all:(q.all-lowdim+1))]
  round(B.star,3)
  B.mat<-(sqrt(n.all*m))*(Ddiag.sq%*%B.star)#[,c(1:lowdim)]

  #grpals
  #B.mat<-Ddiag.sq%*%B.star
  #cluster CA
  #B.mat<-(sqrt(n*m))*(Ddiag.sq%*%B.star)#[,c(1:lowdim)]

  list(Qcate.mat=B.mat,lambda.vec=eig.res$values)#Qcate.list=W.list,

}


