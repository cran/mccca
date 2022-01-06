#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices rainbow

objcheck.func <- function(para.list=para.list,ite=ite,OB.vec=OB.vec
                        ,paraname.p=paraname.p,ite.ob=ite.ob,printcheck=T
                        ,obcheck.start=1,e.cri=e.cri){

  if(is.logical(printcheck))printcheck<-ifelse(printcheck,2,0)

  ob.list<-OBJfunc(para.list)
  OB.vec[ite.ob]<-ob.list$obval
  saved.list<-ob.list$saved.list

  obval <- OB.vec[ite.ob]

  if(ite.ob>obcheck.start){

    if( OB.vec[ite.ob]-OB.vec[ite.ob-1] > 0){#if(printcheck)
      # browser()
      if(printcheck>1) cat("  obvalue is increased by",OB.vec[ite.ob]-OB.vec[ite.ob-1],"at",ite,"th ite,",paraname.p,"update.\n")
      down.para.save<-paraname.p
    }else{
      down.para.save<-FALSE
    }

    if(abs(OB.vec[ite.ob]-OB.vec[ite.ob-1])<e.cri){
      if(printcheck>2) cat(paste("  ",ite,"th ite converge at",paraname.p,"update.\n"))
      OB.vec[(ite.ob+1):length(OB.vec)]<-OB.vec[ite.ob]
      convergence<-TRUE
      #break
    }else{
      convergence<-FALSE
    }


  }else{
    if(printcheck>2) cat(("  skip checking\n"))
    down.para.save<-F
    convergence<-F
  }

  ite.ob2<-ite.ob+1

  list(OB.vec=OB.vec,ite.ob=ite.ob2,saved.list=saved.list
       ,down.para.save=down.para.save,convergence=convergence)
}



list2mat.func<-function(data=data,inputform=c("list")#,ndata=ndata
                        ,outputform="matrix",whichsame="col"){
  ##check all dim
  ndata <- length(data)
  rowvec<-rep(0,ndata)
  colvec<-rep(0,ndata)

  for(dd in 1:ndata){
    data.d<-data[[dd]]
    rowvec[dd]<-nrow(data.d)
    colvec[dd]<-ncol(data.d)
  }

  ###para
  row.all<-sum(rowvec)
  col.all<-sum(colvec)

  data.diag<-matrix(0,row.all,col.all)

  if("matrix" %in% outputform){
    if(whichsame=="row"){
      data.mat<-matrix(0,rowvec[1],col.all)
      if(any(rowvec[1]!=rowvec))print("To combine matrix, all row needs to be the same.")

    }else if(whichsame=="col"){
      data.mat<-matrix(0,row.all,colvec[1])
      if(any(colvec[1]!=colvec)) print("To combine matrix, all column needs to be the same.")
    }}


  dd<-1
  for(dd in 1:ndata){
    ##row
    drow<-ifelse(dd!=1,sum(rowvec[c(1:(dd-1))]),0)
    dd.rowvec<-c((drow+1):(drow+rowvec[dd]))

    dcol<-ifelse(dd!=1,sum(colvec[c(1:(dd-1))]),0)
    dd.colvec<-c((dcol+1):(dcol+colvec[dd]))

    if("matrix" %in% outputform){

      if(whichsame=="row"){
        data.mat[,dd.colvec]<-data[[dd]]
      }else if(whichsame=="col"){
        data.mat[dd.rowvec,]<-data[[dd]]
      }

    }
    if("diag" %in% outputform){
      data.diag[dd.rowvec,dd.colvec]<-data[[dd]]
    }
  }##ite data

  #####delete unecessary one (to save memory)
  if(("matrix" %in% outputform)==FALSE){
    data.mat<-NULL}
  if(("diag" %in% outputform)==FALSE){
    data.diag<-NULL}

  list(data.mat=data.mat,data.diag=data.diag
       ,rowvec=rowvec,colvec=colvec)


}

is_integer <- function(vari) {
  all(vari %% 1 == 0)
}


create.color.func<-function(ncolor=ncolor,transpare=FALSE,printcheck=FALSE
                            ,degreeTra=80,palet1="Dark2",palet2="Paired",use.package=TRUE){

  if(use.package){
    if(ncolor<=8){#8 is max of dark2
      palet<-palet1
      cols<-brewer.pal(ncolor,palet)
    }else if(ncolor<=12){#12 is max of paired
      palet<-palet2
      #cols<-brewer.pal(12,palet)[1:ncolor]
      cols<-brewer.pal(ncolor,palet)[1:ncolor]
      #cols<-rep(brewer.pal(12,palet), ceiling(ncolor/12))[1:ncolor]
    }else if(ncolor>12){
      cols1<-brewer.pal(12,palet2)
      cols2<-brewer.pal(8,palet1)[c(4,8)] #pink,gray
      #browser()
      #cols2<-rainbow(ncolor-12)
      cols<-rep(c(cols1,cols2), ceiling(ncolor/20))[1:ncolor]
      if(ncolor>20)print("ncolor is >20. so same color is repeated.")
    }
  }else{
    #cols<-heat.colors(ncolor)
    #rainbow except red
    cols<-rainbow(ncolor+1)[-1]
    palet<-"defalut"
  }

  if(printcheck) print(paste("use palette",palet))

  colvec<-cols#[seq(1,ncolor)]#c(seq((Gtrue+1),(Gtrue+sum(Kdatavec[c(1:ndata.self)]))),rep(seq(1,Gtrue),ncate.ori))
  if(transpare) colvec<-sapply(cols,function(x){paste(x,degreeTra,sep="")})

  colvec


}


