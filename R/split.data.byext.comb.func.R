split.data.byext.comb <- function(dat,ext.mat=ext.mat,#extcate.label=NULL,
                              clstr0.vec=NULL){#,shortlab.ps=3){

  verbose <- TRUE
  #J <- ncol(dat)
  H <- ncol(ext.mat) # the number of external variable
  if(is.null(H)){
    ext.mat=as.matrix(ext.mat,ncol=1)
    H=1 #if H is NULL,it means ext.mat is not matrix but a vector with a length n,
    #so set the number of external vari as 1.
  }
  #browser()
  J=ncol(dat) ; N=nrow(dat)

  ######ext.matをfactorのdata.frameにする####


  ##########split data##########
  #ext.mat<-matrix(as.numeric(data.all[,which.ext]),nrow(data.all),next)
  #dat<-matrix(as.numeric(data.all[,-which.ext]),ncol = (ncol(data.all)-next),byrow=F)

  ##the number of categories for each external variables
  extcate.vec<-sapply(seq(1,H),function(x){length(unique(ext.mat[,x]))})
  extcate.list=rep(list(NA),H)
  for(hh in 1:H){
    #if(is.data.frame(dat))
    #browser()
    extcate.list[[hh]]=unique(ext.mat[,hh])#levels(data.ext[,hh])#factor想定
    #if all is numeric, sort categories
    if(all(is.numeric(extcate.list[[hh]])))extcate.list[[hh]]=sort(extcate.list[[hh]])
    extcate.vec[hh]=length(extcate.list[[hh]])
  }
  #when just speparating data
  #ndata<-sum(datavec.sup)
  #combining data
  C<-prod(extcate.vec)
  #browser()

  extcomb <- expand.grid(lapply(c(1:H),function(x){extcate.list[[x]]}))
  #extcate.list
  #ncomb <- nrow(extcomb)
  if(nrow(extcomb)!=C){
    # the number of combinations(=nrow(extcomb)) should be equal to C.
    stop("something is wrong.")
  }


  #####define classlabel#####
  classlab.mat=matrix(NA,C,H+1)
  rownames(classlab.mat)=paste0(c(1:C),"th class")
  colnames(classlab.mat)=c(paste0("extvari",c(1:H)),"classlabel")#,"classlabel(short)")
  #1-Hth col:combinations of external categories,
  #H+1th col:label name
  ##(文字のclass名使うときは省略verの列追加)
  classlab.mat[,c(1:H)]=as.matrix(extcomb)#as.matrixしないとなんかlistになる
  classlab.mat[,(H+1)]=apply(extcomb,1,function(x){paste(x,collapse="&")})
  #short
  #classlab.mat[,(H+2)]=apply(extcomb,1,function(x){paste(stringr::str_sub(x,start=1,end=shortlab.ps),collapse="&")})

  #motodata.vec may not be needed... (19/6)
  #how many 1given data is repeated and how each is divided.
  #when dividing data by gender(M/W) and age(Y/M/O),
  #motodata.vec=c(1,1,2,2,2)
  #motodata.vec <- rep(seq(1,length(datavec.sup)),times=datavec.sup)

  #######save data################
  X.list<-clstr0.list <- clstr.list <- oriindex.list <-rep(list(NA),C)
  names(X.list)<-names(clstr0.list) <- names(clstr.list) <- names(oriindex.list) <-classlab.mat[,(H+1)]
  class.n.vec <- clstr.vec<-rep(NA,N)
  names(class.n.vec)=rownames(dat)
  Ktrue.vec<-N.vec <- rep(NA,C)
  names(Ktrue.vec)<-names(N.vec) <-classlab.mat[,(H+1)]
  #data.vec <- seq(1,nrow(dat))#oriid.vec <-

  ####for check
  #datadis <- rep(NA,H)
  #for(tt in 1:length(datavesc.sup)){
  #for(tt in 1:C){
  #  datadis[tt] <- sum(datavec.sup[c(1:tt)])
  #}

  ###which is empty class
  emptyclass=rep(FALSE,C)

  #browser()
  cc <- emp<-1
  for(cc in 1:C){

    comb <- as.vector(extcomb[cc,])
    as.character(extcomb[cc,])

    ###pick up sub having the "comb" combinations of ext vari.
    pickupsub <- apply(ext.mat,1,function(x){all(x==comb)})
    N.vec[cc] <- Nc <-sum(pickupsub)

    #if(printcheck) cat(cc,"th data, n=",sum(pickupsub),"\n")#print(paste("data",datalabel[ss]))
    if(verbose) cat(cc,"th class:","(",paste(as.matrix(comb),collapse=","),") data, n=",Nc,"\n")
    #as.matrix enables "comb" to be treated as character.
    if(Nc==0){
      message("the # of observation for (",paste(comb,collapse=","),") data is 0. So skip this class.\n")
      emptyclass[cc]=TRUE
      #clstr0.list[[cc]] <- clstr.list[[cc]]<-"(empty class)"
      Ktrue.vec[cc]=0
      #oriindex.list[[cc]]=NULL
      #browser()
      #emp=emp+1
      #next

    }else{#when not Nc==0
      #X.list[[cc]]<-matrix(dat[pickupsub,],Nc,J)
      #by using "matrix", this is always matrix even though Nc=1.
      #=>しかしmatrix使うと，dataframeの時に変なことになるのでやめた．
      X.list[[cc]]<-dat[pickupsub,]
      class.n.vec[pickupsub]=cc
      oriindex.list[[cc]]=which(pickupsub)
      #これでもNc=1のときもいけるはず...(簡単に確認した結果いけたが)

      if(!is.null(clstr0.vec)){
        clstr0.list[[cc]] <- clstr0.vec[pickupsub]
        Ktrue.vec[cc]=length(unique(clstr0.list[[cc]]))
        #clstr.list[[cc]]=clstr0.list[[cc]]+ifelse(cc==1,0,sum(Ktrue.vec[c(1:(cc-1))]))#sum(K.vec[c(1:cc)])
        #clstr.vec[oriindex.list[[cc]]]=clstr.list[[cc]]
      }

    }
    #if(printcheck)print(paste("data",ss))
    #ss.de <- min(which(ss<=datadis))
    #ss.d <- ss - ifelse(ss.de==1,0,sum(extcate.vec[c(1:(ss.de-1))]))
    #pickupsub <- ext.mat[,ss.de]==ss.d

    #print(table(ext.mat[pickupsub,ss.de]))

    #cat("ss:",ss,"\n")
    #cat("ss.de:",ss.de,",ss.d=",ss.d,"\n")
    #,",",ss.de2
    #cat("insert data\n")
    #browser()

    #cls.tr.vec[data.vec==ss]<-
   # browser()
    #is true cluster is given, cluster lists for each classes are also made.
    #oriid.list[[cc]] <- oriid.vec[pickupsub]
    #data.vec[pickupsub] <- cc

    ####label
    # if(!is.null(extcate.label)){
    #
    # }

  }###end for class
  #browser()

  ###remove class skipped because N is 0.
  ded=1 ; class.n.vec.old=class.n.vec
  for(emp in which(emptyclass)){
    whichlist=(which(names(X.list)==classlab.mat[emp,H+1]))
    X.list[[whichlist]]<-clstr0.list[[whichlist]] <- oriindex.list[[whichlist]]<-NULL
    #X.list[[emp]]<-clstr0.list[[emp]] <- clstr.list[[emp]]<-oriindex.list[[emp]]<-NULL
    class.n.vec[emp<class.n.vec.old]=class.n.vec.old[emp<class.n.vec.old]-ded#mae ni tsumeru
    ded=ded+1
  }

  if(any(emptyclass)){
    classlab.mat=classlab.mat[-which(emptyclass),]
    N.vec<-N.vec[-which(emptyclass)] ; Ktrue.vec<-Ktrue.vec[-which(emptyclass)]
  }

  ###after removing empty class, redefine the class index.
  C2=nrow(classlab.mat)
  rownames(classlab.mat)<-paste0(c(1:C2),"th class")
  names(N.vec)<-names(Ktrue.vec)<-mapply(function(x,y){paste(x,y,sep=":")},c(1:C2),classlab.mat[,(H+1)])
  names(oriindex.list)=mapply(function(x,y){paste(x,y,sep=":")},c(1:C2),names(oriindex.list))
  names(X.list)=mapply(function(x,y){paste(x,y,sep=":")},c(1:C2),names(X.list))
  #browser()

  ##create character ver of class.n.vec
  classname.n.vec=classlab.mat[class.n.vec,(H+1)]
  classname.n.vec=mapply(function(x,y){paste(x,y,sep=":")},class.n.vec,classname.n.vec)

  if(is.null(clstr0.vec)){
    clstr0.list <- Ktrue.vec<-NULL#clstr.list <- clstr.vec <- NULL
  }

  list(data.list=X.list,clstr0.list=clstr0.list,Ktrue.vec=Ktrue.vec#clstr.list=clstr.list,clstr.vec=clstr.vec,#,
       ,N.vec=N.vec,classlab.mat=classlab.mat,class.n.vec=class.n.vec,classname.n.vec=classname.n.vec,
       oriindex.list=oriindex.list)#,extcate.list=extcate.list)
       #data.vec=data.vec)#,motodata.vec=motodata.vec)


}
