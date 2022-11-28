#' plot \code{mccca} object.
#'
#' @description plot \code{mccca} object.
#' @param x An object of class \code{mccca}, a list of \code{MCCCA} outputs.
#' @param main A character giving the title of biplot.
#' @param catelabel A characteristic vector of length Q giving labels for all categories to be displayed on the biplot (Q=\code{sum(q.vec)}). If \code{NULL}, \code{rownames(B)} are used.
#' @param classlabel A characteristic vector of length C (C:the number of class) giving labels for all classes to be displayed on the biplot. If \code{NULL}, labels specified in \code{create.MCCCAdata} are used.
#' @param classlabel.legend A characteristic vector of length C giving labels for all classes to be used on the legend (this can be longer). If \code{NULL}, \code{classlabel} is used.
#' @param xlim A numeric vector of length 2 giving the range of plot on the x (horizontal) axis. If NULL, the range is automatically determined.
#' @param ylim A numeric vector of length 2 for the y (vertical) axis (same role as \code{xlim}).
#' @param scatter.level A numeric value that adjusts the scatter of points in the biplot. The higher the value, the more scattered the points are. The default is 2.
#' @param sort.clssize If \code{TRUE}, the class-specific cluster numbers are sorted in the order of cluster size. The default is \code{TRUE}.
#' @param connect.cord If \code{TRUE}, lines are drawn between original (estimated by MCCCA) coordinates and coordinates moved to avoid overlap.
#' @param include.variname If \code{TRUE}, variable name is included in category labels in the biplot (ex.a point of category "male" in "v1"(the name of 1st variable) is displayed as "v1:male" on the biplot).
#' @param scale.gamma If \code{TRUE}, quantifications are scaled such that the average squared deviation from the origin of the row and column points is the same (See section 2.3 in the paper).
#' @param break.size An integer vector that adjusts the size of bubble displayed on the legend.
#' @param output.coord If \code{TRUE}, the output will be \code{Cocls.mat} and \code{Cocate.mat}. See value.
#' @param plot.setting A list of biplot settings. See details.
#' @param \dots Additional arguments passed to \code{\link{print}}.
#' @return If \code{output.coord} is \code{TRUE}, returns a list with the following elements.
#' \item{\code{Cocls.mat}}{A (Kx4) coordinate matrix of clusters, where the last two columns are the coordinates estimated by MCCCA, and the first two columns are the coordinates moved from the estimated coordinates to prevent overlap.}
#' \item{\code{Cocate.mat}}{A (Kx4) coordinate matrix of categories (each column plays the same role as \code{Cocls.mat}) }
#' @seealso \code{\link{MCCCA}}
#' @importFrom ggplot2 waiver ggplot aes geom_text geom_point geom_segment scale_color_manual scale_size labs coord_equal xlim ylim guides guide_legend theme element_text theme_bw geom_vline geom_hline
#' @importFrom stringr str_sub str_detect
#' @importFrom grDevices palette dev.cur dev.off rainbow
#' @importFrom graphics par
#' @method plot mccca
#' @details Parameters in \code{plot.setting} are as follows:
#'
#' -\code{alp.point}:A numeric value from 0 to 1 which adjusts the transparency of the bubble point. The default is 0.3.
#'
#' -\code{alp.seg}:A numeric value from 0 to 1 which adjusts the transparency of the segments between texts and points. The default is 0.8.
#'
#' -\code{txtsize}:A numeric value which adjusts the textsize on the biplot. The default is 3.
#'
#' -\code{txtsize.legend}:A numeric value which adjusts the textsize of the legend on the biplot. The default is 10.
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
#' #generate external variable data
#' data.ext=generate.ext(N,extcate.vec=extcate.vec)
#'
#' #create mccca.list to be applied to MCCCA function
#' mccca.data=create.MCCCAdata(data.cate,ext.mat=data.ext,clstr0.vec =clstr0.vec)
#'
#' #specify the number of cluster for each of C classes
#' C=length(mccca.data$data.list)
#' K.vec=rep(2,C)
#' #apply MCCCA
#' mccca.res=MCCCA(mccca.data,K.vec=K.vec)
#'
#' #plot MCCCA result
#' plot(mccca.res)


### @references Takagishi, M. & Velden, M. van de (2022). Visualizing class specific heterogeneous tendencies in categorical data, to appear in Journal of Computational and Graphical Statistics.

plot.mccca=function(x,main="MCCCA result",catelabel=NULL,classlabel=NULL,classlabel.legend=NULL#,
                       ,xlim=NULL,ylim=NULL,#xlimval=NULL,#,add.numlabel=TRUE
                       #dis.color=FALSE,output.plot=FALSE
                       #notmv.c=NULL,notmv2.c=NULL,##
                       #CCsp=NULL,output.coord=FALSE,#
                       #legendname="class-specific clusters",#legend name
                       sort.clssize=TRUE,break.size=NULL,output.coord=FALSE,
                       connect.cord=TRUE,include.variname=TRUE,scale.gamma = TRUE,#which.drawseg=NULL,#
                       #plot.seg.biord=FALSE,ordcomb=NULL,#bi,ordに線をひく用
                       #seglabel=NULL,#seglabel=rep(c("binary (speed)","ordinal (light)"),times=c(1,3)),
                       #(ただし20/11/19時点traffic dataのみ想定)
                       scatter.level=2,#plot.bubble=TRUE,
                       plot.setting=list(alp.point=0.3,alp.seg=0.8,txtsize=3,txtsize.legend=10),...){#,
                    #){#,display.legend.size=FALSE,display.legend.color=FALSE#,display.legend.seg=FALSE
  #sort.clssize=TRUE ;
  fontfamily="Helvetica" ; bigger=0.2
  legendname="class-specific clusters" ; add.numlabel=TRUE

  ####setting
  #plot.setting$xlimm
  alp.point=plot.setting$alp.point ; alp.seg=plot.setting$alp.seg
  txtsize=plot.setting$txtsize ; txtsize.legend=plot.setting$txtsize.legend

  #----------------------------------------------------#
  #####1,data preparation####
  #----------------------------------------------------#

  #####data preparation####
  if(!is.null(x)){
    #print("not null")
    if(scale.gamma){
      Cate.cord<-x$Bg
      Obj.cord<-x$Qg
      Cls.cord<-x$Gg
    }else{
      Cate.cord<-x$B
      Obj.cord<-x$Q
      Cls.cord<-x$G
    }

    #rownames(Cls.cord) <- seq(1,nrow(Cls.cord))
    clses.vec<-x$clses.vec
    #Kdatavec.es<-x$Kdatavec.es
    q.vec<-x$q.vec

    if(!include.variname){
      rownames(Cate.cord)=x$catename.vec
    }

    ###(20/11/19)new data
    K.vec=x$K.vec
    #if(is.null(q.vec)) q.vec<-x$q.vec

    if(is.null(classlabel.legend))classlabel.legend <- x$classlabel#datalabel
    #without setting NULL, names(classlabel.legend) will be plotted in legend
    names(classlabel.legend)=NULL
    #legends.mca<-x$datalabel
    #data.vec<-x$data.vec
    cluster.vec <- x$cluster.vec
    if(is.null(classlabel))classlabel=x$classlabel#.short
  }

  #####para define
  K<-sum(K.vec) ; C<-length(K.vec) ; J <- length(q.vec)
  #rownames(Cls.cord) <- seq(1,nrow(Cls.cord))

  if(sort.clssize){ #copy from onenote
    #browser()
    #levels(clses.f)[c(1,2)]<-c(2,1)
    clses.vec.save <- clses.vec
    Cls.cord.save <- Cls.cord

    dd<-1
    for(dd in 1:C){
      #cls.d <- cluster.vec[cluster.vec==dd]
      which.cls <- which(cluster.vec==dd)

      if(length(which.cls)!=1){
        #  browser()
        Cls.cord.d <- Cls.cord.save[which.cls,]
        #clses.vec[which(which.cls %in% clses.vec)]
        clses.d <- clses.vec.save[which(clses.vec.save %in% which.cls)]

        tabcls <- as.numeric(table(clses.d))
        srt <- sort(tabcls,index.return=TRUE,decreasing = TRUE)
        sortind <- srt$ix
        table(clses.d)[sortind]
        clses.f <- factor(clses.d)
        levels(clses.f)[sortind] <- levels(clses.f)
        table(clses.f) #biggest size cluster has index 1.

        ######sort cluster mean
        Cls.cord[which.cls,] <- Cls.cord.d[sortind,]
        clses.vec[which(clses.vec.save %in% which.cls)] <- as.numeric(as.character(clses.f))

      }
    }##end for data

  }##end sort clssize


  ###prepare matrix####
  Cate.d<-as.data.frame(Cate.cord)
  Cls.d <- as.data.frame(Cls.cord)
  Obj.d <- as.data.frame(Obj.cord)
  clsind<-rep(seq(1,C),K.vec)
  clsind2<-as.factor(clsind)
  Cls.d2<-cbind(clsind2,Cls.d)
  colnames(Cls.d2)[1]<-"ind"

  tabcls<-as.numeric(table(clses.vec))
  Cls.d2<-cbind(clsind2,tabcls,Cls.d)
  colnames(Cls.d2)<-c("ind","tabcls","dim1","dim2")

  itemlab<-as.factor(c(rep(1,nrow(Cls.d)),rep(seq(1,J),times=q.vec)))
  CC.d <- rbind(Cls.d,Cate.d)
  CC.d <- cbind(itemlab,CC.d)
  colnames(CC.d)[1]<-"item"
  colnames(CC.d)[-1]=c("dim1","dim2")

  ######category, class label#####
  ###cate label
  if(is.null(catelabel)) catelabel <- rownames(Cate.d)

  ###cluster label####
  #if(is.null(clslabel)){###21ed
  #if(add.numlabel){###21ed
    classlabel.num <- str_sub(rownames(Cls.d),start=-1)
    #for slide and paper
    clsname <- rep(classlabel,K.vec)
    #browser()
    #check if the last character of classname is the number
    lastchara=str_sub(classlabel,start=-1)
    if(any(str_detect(lastchara,"(1|2|3|4|5|6|7|8|9|0)"))){
    #if(any(!is.na(as.numeric(lastchara)))){
      #if there is a number in the last characters of the classlabel, include "-" between classname and cluster index in cluster labels.
      classlabel.all <- mapply(function(x,y){paste(x,y,sep="-")},clsname,classlabel.num)
    }else{#if there is no number, not include "-"
      classlabel.all <- mapply(function(x,y){paste(x,y,sep="")},clsname,classlabel.num)
    }

  labelvec<-c(classlabel.all,catelabel)

  #######adjust coordinate matrix#####
  #cls.d.ed
  clsind<-rep(seq(1,C),K.vec)
  Cate.na<-matrix(NA,nrow(Cate.d),ncol(Cls.d2))
  #Cate.na[,1]<-rep(c(1:J),times=q.vec)+C
  colnames(Cate.na)<-colnames(Cls.d2)
  CC.d2 <- rbind(Cls.d2,Cate.na)
  #Cls.d3[c((nrow(Cls.d2)+1):nrow(Cls.d3)),1]<-rep(c(1:J),times=q.vec)+C
  CC.d2[,1] <- as.factor(c(Cls.d2[,1],rep(c(1:J),times=q.vec)+C))
  CC.d[,1]<-CC.d2[,1]
  CC.d <- cbind(CC.d2$tabcls,CC.d)
  colnames(CC.d)[1] <- "tabcls"

  #########color######
  ###cluster is default, cate is package
  #if(is.null(col.cls))
  #col.cls<-palette()[c(1:C)]
  col.cls=rainbow(C)#22/9/5
  #browser()
  ##use.defaultcol is FALSE
  #if(is.null(col.pal)) {
    col.pal<-create.color.func(J)[c(1:J)]
    if(J==1){
      col.pal<-create.color.func(2)[2]
    }
    #col.pal <- rep(1,length(col.pal))
  #}
    #browser()

  colvec <- c(col.cls,col.pal)
  CC.d$colvec <- colvec[CC.d$item]
  CC.d$labelvec=labelvec
  #browser()
  ######break.size#####
  if(is.null(break.size)){
    size.label <- break.size <- waiver()
  }else{
    size.label <- break.size
  }

  #----------------------------------------------------#
  #####2,use textplot(wordcloud) to avoid overlap configuration####
  #----------------------------------------------------#
  #browser()
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mfrow=c(1,1))
  txtplot.list=textplot.cord(CC.d$dim1,CC.d$dim2,labelvec,cex=scatter.level)
  if( dev.cur() > 1 ) dev.off()

  Xcor=txtplot.list$cord
  lineend=txtplot.list$lineend
  Xcor.ori=txtplot.list$cord.ori

  #----------------------------------------------------#
  #####5,prepare for main plot####
  #----------------------------------------------------#
  #####data for cluster#####
  CC.d.cls=data.frame(dim1=Xcor[,1],dim2=Xcor[,2],dim1ori=CC.d$dim1,dim2ori=CC.d$dim2
                      ,tabcls=CC.d$tabcls,item=CC.d$item, col=CC.d$colvec)#CC.d$item[c(1:length(tabcls))])

  ###data for original place of category####
  CC.both=CC.d
  CC.both$dim1.es=Xcor[,1]#CC.d.es$dim1
  CC.both$dim2.es=Xcor[,2]#CC.d.es$dim2

  #----------------------------------------------------#
  #####6,main plot####
  #----------------------------------------------------#
  ###start plot
  #g2=ggplot(NULL)#,colour = "green"
  g2=ggplot(CC.d.cls,aes(x=dim1,y=dim2))#,shape=item,colour=item))

  ##
  if(connect.cord){
    which.drawseg=which(!is.na(lineend[,1]))
    CC.both=CC.both[which.drawseg,]
    g2=g2+geom_segment(data=CC.both,aes(x=dim1, y=dim2, xend=dim1.es, yend=dim2.es)#,alpha=0.6)
                       ,color=CC.both$col,alpha=alp.seg)
  }

  ###cluster bubble#data=CC.d.cls,
  g2=g2+geom_point(aes(x = dim1, y = dim2, size = tabcls,colour=item)#,shape=item)
                   ,alpha=alp.point)#, show.legend = TRUE)#,shape=clusterlabel)#[CC.d.es$item])
  #data=CC.d.cls,

  g2=g2+geom_text(data=CC.d.cls,aes(x = dim1, y = dim2,label = labelvec)
                  ,color=CC.d$colvec,size=txtsize #aes()の中から外すとtxtsizeの数値変更が反映されるように(21/2/6)
                  , show.legend = FALSE,fontface="bold",family=fontfamily)#,

  #browser()
  g2 <- g2 + scale_color_manual(values=colvec,#[c(1:C)]
                                name=legendname,breaks=seq(1,C),
                                #labels=c(classlabel.legend,"a","b"))
                                labels=classlabel.legend)

  #geom_point(aes(x=0,y=0, color=factor(1)), shape='X', size=5) +
  #range:actual bubble size (not the range of # of cluster size)
  g2 <- g2 + scale_size(range = c(5, 15), name="cluster size",
                        breaks=break.size, labels=size.label)

  if(is.null(xlim)){
    cate.max1<-max(CC.d.cls$dim1)#ifelse(plot.cate,max(abs(Cate.cord)),0)
    cate.min1<-min(CC.d.cls$dim1)
    xlim=c(cate.min1-bigger,cate.max1+bigger)
  }
  if(is.null(ylim)){
    cate.max2<-max(CC.d.cls$dim2)#ifelse(plot.cate,max(abs(Cate.cord)),0)
    cate.min2<-min(CC.d.cls$dim2)
    ylim=c(cate.min2-bigger,cate.max2+bigger)
  }

  g2 <- g2 + labs(x="dim 1",y="dim 2",title=main)
  g2 <- g2 + coord_equal() #
  g2 <- g2 + xlim(xlim) + ylim(ylim)
  #g2 <- g2 + theme(legend.key=element_blank(), legend.title=element_blank())
  g2 <- g2+ guides(colour = guide_legend(override.aes = list(alpha = 0.7,size=5),order=1),
                   size=guide_legend(override.aes = list(alpha = 0.5),order=1),
                   linetype=guide_legend(order=3))#,reverse=TRUE))
  ##legend
  #g2 <- g2+ theme(legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical")

  g2 <- g2 + theme(text=element_text(face="bold") ,plot.title = element_text(hjust = 0.5))#+theme(plot.title = element_text(hjust = 0.5))
  g2 <- g2 + theme_bw(base_family = fontfamily) + geom_vline(xintercept=0) + geom_hline(yintercept=0)

  #legend
  g2 <- g2+ theme(legend.title = element_text(face="bold",size=txtsize.legend),#
                  legend.text =element_text(face="bold",size=txtsize.legend) )

  print(g2,...)


  if(output.coord){
    rownames(CC.d.cls)=labelvec
    K=length(classlabel.all)
    Cocls.mat=CC.d.cls[c(1:K),c(1:4)]
    #Cocls.ori.mat=CC.d.cls[c(1:K),c(3:4)]
    Cocate.mat=CC.d.cls[-c(1:K),c(1:4)]
    #Cocate.ori.mat=CC.d.cls[-c(1:K),c(3:4)]
    return(list(Cocls.mat=Cocls.mat,Cocate.mat=Cocate.mat))
    #return(list(Cocls.mat=Cocls.mat,Cocls.ori.mat=Cocls.ori.mat,
     #           Cocate.mat=Cocate.mat,Cocate.ori.mat=Cocate.ori.mat))
  }

}
