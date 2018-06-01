plotM <- function(dummy=1){
  
  par( mar=c(3,3,0.05,0.01), cex.main = 1.5, cex.lab=1.35 )
  M_nams<-unlist(M_list)#c("M_60", "M_40_60","M_20_40","M_10_20","M_05_10","M_05")
  
  cond<-M_nams%in%input$M
  
  if(sum(cond)>0){
    
    M_max<-max(M_maxes[cond])
    M_min<-min(M_mins[cond])
    
    maxa<- -log(0.02)/M_min
    mina<- -log(0.02)/M_max
    maxage<-floor(maxa*1.1)
    UB<-exp(-M_min*((1:maxage)-1))
    LB<-exp(-M_max*((1:maxage)-1))
    
    plot(c(1,maxage),c(0,1),col="white",xlab="",ylab="")
    mtext("Age",1,line=2)
    mtext("Survival",2,line=2)
    polygon(c(1:maxage,maxage:1),c(LB,UB[maxage:1]),border=NA,col=fcol)
    abline(v=c(mina,maxa),lty=2,col=icol)
    
    text(mina+(maxa-mina)/2,0.95," Range max age ",col=icol)
    text(mina+(maxa-mina)/2,0.88,paste(round(mina,1), "-",round(maxa,1),"years"),col=icol)
    text(mina+(maxa-mina)/2,0.81,paste(M_max, "> M >",M_min),col=icol)
    
    
  }else{
    
    plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(10,0.5,"< unspecified >",col="grey")
    
  }
  
}



plotD <- function(dummy=1){
  
  D_nams<-unlist(D_list)#c("D_10", "D_10_20","D_20_30","D_30_60","D_60_80","D_80")
  
  cond<-D_nams%in%input$D
  
  if(sum(cond)>0){
    par(mfrow=c(1,2),mai=c(0.3,0.5,0.01,0.01), omi=c(0.4,0.18,0.55,0.1),cex.main = 1.5, cex.lab=1.35 )
    D_max<-max(D_maxes[cond])
    D_min<-min(D_mins[cond])
    
    set.seed(1)
    
    ts1<-(2+(cos((-0:60)/10.18))/3)*exp(rnorm(61,0,0.2))
    ts1<-ts1/mean(ts1[1:5])
    
    ts2<-(2+(cos((10:40)/4.18)/4))*exp(rnorm(31,0,0.1))
    ts2<-ts2/mean(ts2[1:5])
    
    # plot TS1
    yrs<-2017-(length(ts1):1)-1
    ny<-length(yrs)
    
    Dmaxs<-seq(1,D_max,length.out=ny)
    Dmins<-seq(1,D_min,length.out=ny)
    
    UB<-Dmaxs*ts1
    LB<-Dmins*ts1
    
    plot(yrs[c(1,ny)],c(0,1.2),col="white",xlab="",ylab="")
    abline(h=c(D_max,D_min),lty=2,col=icol)
    abline(h=1)
    polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=fcol)
    
    mtext("Example 1",3,line=0.8)
    
    # plot TS2
    yrs<-2017-(length(ts2):1)-1
    ny<-length(yrs)
    
    Dmaxs<-seq(1,D_max,length.out=ny)
    Dmins<-seq(1,D_min,length.out=ny)
    
    UB<-Dmaxs*ts2
    LB<-Dmins*ts2
    
    plot(yrs[c(1,ny)],c(0,1.2),col="white",xlab="",ylab="")
    abline(h=c(D_max,D_min),lty=2,col=icol)
    abline(h=1)
    polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=fcol)
    
    mtext("Example 2",3,line=0.8)
    #text(mina+(maxa-mina)/2,0.95," Range max age ",col='orange')
    #text(mina+(maxa-mina)/2,0.88,paste(round(mina,1), "-",round(maxa,1)),col='orange')
    #text(mina+(maxa-mina)/2,0.81,paste(M_max, "> M >",M_min),col='orange')
    mtext("Year",1,line=1,outer=T)
    mtext("Spawn. bio. relative to unfished (D)",2,line=0,outer=T)
    
  }else{
    par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
    plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(10,0.5,"< unspecified >", col="grey")
    
  }
  
}



ploth <- function(dummy=1){
  
  par(mfrow=c(1,1), mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
  h_nams<-unlist(h_list)#c("h_30", "h_30_50","h_50_70","h_70_90","h_90")
  
  cond<-h_nams%in%input$h
  
  if(sum(cond)>0){
    
    h_max<-max(h_maxes[cond])
    h_min<-min(h_mins[cond])
    
    np<-100
    D<-seq(0,1,length.out=np)
    UB<-(0.8*h_max*D)/(0.2*(1-h_max)+(h_max-0.2)*D)
    LB<-(0.8*h_min*D)/(0.2*(1-h_min)+(h_min-0.2)*D)
    
    plot(c(0,1),c(0,1),col="white",xlab="",ylab="")
    mtext("Stock depletion (spawning biomass relative to unfished)",1,line=2)
    mtext("Fraction of unfished recruitment",2,line=2)
    polygon(c(D,D[np:1]),c(LB,UB[np:1]),border=NA,col=fcol)
    abline(v=0.2,col=icol)
    abline(h=c(h_min,h_max),col=icol,lty=2)
    
  }else{
    
    plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(10,0.5,"< unspecified >",col="grey")
    
  }
  
}



Ftrendfunc<-function(M1=0.2,M2=1.2,sd1=0.1,sd2=0.3,h2=2,ny=68,plot=F){
  
  E<-rep(NA,ny)
  ind1<-1:floor(M1*ny)
  d1<-dnorm(ind1,M1*ny,sd1*ny)
  E[ind1]<-d1/max(d1)
  ind12<-(floor(M1*ny)+1):ceiling(M2*ny)
  ind12<-ind12[ind12<=ny]
  E[ind12]<-1
  d2<-dnorm(ind12,M2*ny,sd2*ny)
  E[ind12]<-E[ind12]+(d2/max(d2))*h2
  
  ind2<-(ceiling(M2*ny)+1):ny
  if(ind2[1]<ind2[2]){
    E[ind2]<-dnorm(ind2,M2*ny,sd2*ny)
    E[ind2]<-E[ind2]/max(E[ind2])*max(E[ind12])
  }
  
  E<-E/mean(E)
  if(plot)plot(E,ylim=c(0,max(E)*1.05),type="l")
  E
  
}

plotFP <-function(dummy=1){
  
  par(mfrow=c(1,1), mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
  FP_nams<-unlist(FP_list)#c("FP_s", "FP_gr","FP_bb","FP_gi","FP_ri","FP_rd")
  ny=60
  
  trends<-array(NA,c(6,ny))
  # par(mfrow=c(3,2),mar=rep(0.1,4))
  for(i in 1:6)trends[i,]<-Ftrendfunc(M1=M1s[i],M2=M2s[i],sd1=sd1s[i],sd2=sd2s[i],h2=h2s[i],ny=ny)
  cols<-rep(c(fcol,'black','dark grey'),2)
  ltys<-rep(c(1,2),each=3)
  
  cond<-FP_nams%in%input$FP
  trends[!cond,]<-NA
  
  if(sum(cond)>0){
    
    plot(c(0,ny),c(0,max(trends,na.rm=T)),col="white",xlab="",ylab="",axes=F)
    axis(2)
    axis(1,c(-10e6,10e6),c(-10e6,10e6))
    mtext("Historical year",1,line=2)
    mtext("Relative exploitation rate (Effort)",2,line=2)
    for(i in (1:6)[cond])lines(trends[i,],col=cols[i],lty=ltys[i])
    legend('topleft',legend=names(FP_list)[cond],text.col=cols[cond],lty=ltys[cond],col=cols[cond],bty='n',cex=0.8)
    
  }else{
    
    plot(c(1,ny),c(0,2),col="white",axes=FALSE,xlab="",ylab="")
    text(ny/2,1,"< unspecified >",col="grey")
    
  }
  
}



plotF <- function(dummy=1){
  
  FP_nams<-unlist(FP_list)#c("FP_s", "FP_gr","FP_bb","FP_gi","FP_ri","FP_rd")
  
  ny=60
  
  trends<-array(NA,c(6,ny))
  # par(mfrow=c(3,2),mar=rep(0.1,4))
  for(i in 1:6)trends[i,]<-Ftrendfunc(M1=M1s[i],M2=M2s[i],sd1=sd1s[i],sd2=sd2s[i],h2=h2s[i],ny=ny)
  cond<-FP_nams%in%input$FP
  
  F_nams<-unlist(F_list)#c("F_10", "F_10_25","F_25_50")
  
  cond2<-F_nams%in%input$F
  
  if(sum(cond)>0&sum(cond2)>0){
    par(mfrow=c(1,2),mai=c(0.3,0.5,0.01,0.01), omi=c(0.4,0.4,0.55,0.1),cex.main = 1.5, cex.lab=1.35 )
    
    #trends<-trends[cond,]
    simbyt<-100
    nsim<-simbyt*6
    
    Esd_max<-max(F_maxes[cond2])
    Esd_min<-min(F_mins[cond2])
    Esdrand<-runif(nsim,Esd_min,Esd_max)
    Emu<-(-0.5*Esdrand^2)
    Esdarray<-array(exp(rnorm(nsim*ny,Emu,Esdrand)),c(nsim,ny))
    Eind<-as.matrix(expand.grid(1:nsim,1:ny))
    Tind<-cbind(rep(1:6,each=simbyt),Eind[,2])
    stochtrends<-array(NA,c(nsim,ny))
    stochtrends[Eind]<-Esdarray[Eind]*trends[Tind]
    stochtrends<-stochtrends/apply(stochtrends,1,mean)
    
    plot(c(1,ny),c(0,quantile(stochtrends,0.98)),col="white",xlab="",ylab="")
    B90s<-apply(stochtrends[rep(cond,each=simbyt),],2,quantile,p=c(0.05,0.95))
    B50s<-apply(stochtrends[rep(cond,each=simbyt),],2,quantile,p=c(0.25,0.75))
    
    #med<-apply(stochtrends,2,quantile,p=0.5)
    #matplot(t(stochtrends),col="#99999920",type="l")
    polygon(c(1:ny,ny:1),c(B90s[1,],B90s[2,ny:1]),border=NA,col=fcol)
    polygon(c(1:ny,ny:1),c(B50s[1,],B50s[2,ny:1]),border=NA,col=icol)
    #lines(1:ny,med,col='white',lwd=2)
    legend('topleft',legend=c('90% PI',"50% PI"),fill=c(fcol,icol),bty='n',border='white')
    mtext("Historical year",1,line=0.45,outer=T)
    mtext("Relative exploitation (Effort)",2,line=2)
    mtext("Simulated range",3,line=0.8)
    
    # Example plots
    maxind<-(((0:5)*100)+aggregate(Esdrand,by=list(rep(1:6,each=simbyt)),which.max)$x)[cond]
    minind<-(((0:5)*100)+aggregate(Esdrand,by=list(rep(1:6,each=simbyt)),which.min)$x)[cond]
    
    cols<-rep(c(fcol,'black','dark grey'),2)
    ltys<-rep(c(1,2),each=3)
    
    plot(c(1,ny),c(0,quantile(stochtrends,0.98)),col="white",xlab="",ylab="")
    if(sum(cond)==1){
      lines(1:ny,stochtrends[maxind,],col=cols[cond],lty=ltys[cond])
      lines(1:ny,stochtrends[minind,],col=cols[cond],lty=ltys[cond])
    }else{
      matplot(1:ny,t(stochtrends[maxind,]),add=T,col=cols[cond],lty=ltys[cond],type='l')
      matplot(1:ny,t(stochtrends[minind,]),add=T,col=cols[cond],lty=ltys[cond],type='l')
    }
    
    mtext("",1,line=2)
    mtext("",2,line=2)
    
    #legend('topleft',legend=names(FP_list)[cond],text.col=cols[cond],lty=ltys[cond],col=cols[cond],bty='n',cex=0.8)
    mtext("Examples",3,line=0.8)
    
  }else{
    plot(c(1,ny),c(0,2),col="white",axes=FALSE,xlab="",ylab="")
    if(sum(cond)==0){
      text(ny/2,1,"< Answer question 4 >",col="grey")
    }else{
      text(ny/2,1,"< Unspecified >",col="grey")
    }
    
  }
  
}



plotsel <- function(dummy=1){
  
  par( mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
  sel_nams<-unlist(sel_list)#c("sel_50", "sel_50_75","sel_75_125","sel_125_150","sel_150_200")
  cond<-sel_nams%in%input$sel
  
  if(sum(cond)>0){
    
    sel_max<-max(sel_maxes[cond])
    sel_min<-min(sel_mins[cond])
    lengths<-seq(0.05,3.5,length.out=100)
    
    mat<-1/(1+exp(-((lengths-1)/0.1)))
    
    sel1<-1/(1+exp(-((lengths-sel_min)/(sel_min*0.1))))
    sel2<-1/(1+exp(-((lengths-sel_max)/(sel_max*0.1))))
    
    par( mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
    
    plot(c(0,3.5),c(0,1),col="white",xlab="",ylab="")
    polygon(c(lengths,lengths[100:1]),c(sel1,sel2[100:1]),border=NA,col=fcol)
    abline(h=0.5)
    
    lines(lengths,mat,col=icol)
    mtext("Length relative to length at 50% maturity (S)",1,line=2)
    mtext("Selectivity",2,line=2)
    legend('bottomright',legend=c("Selectivity","Maturity"),fill=c(fcol,icol),bty='n',cex=0.8,border='white')
    abline(v=c(sel_min,sel_max),col=fcol,lty=2)
    abline(v=1,col=icol,lty=2)
    
  }else{
    plot(c(0,3.5),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(1.75,0.5,"< Unspecified >",col="grey")
  }
  
}



dnormal<-function(lens,lfs,sl,sr){
  cond<-lens<=lfs
  sel<-rep(NA,length(lens))
  sel[cond]<-2.0^-((lens[cond]-lfs)/sl*(lens[cond]-lfs)/sl)
  sel[!cond]<-2.0^-((lens[!cond]-lfs)/sr*(lens[!cond]-lfs)/sr)
  sel
}

getsel<-function(lens,lenmax,sl,Vmaxlen){
  sr<-(max(lens)-lenmax)/((-log(Vmaxlen,2))^0.5) # upper standard deviation of double log normal
  dnormal(lens,lenmax,sl,sr)
}


plotdome <- function(dummy=1){
  
  par(mfrow=c(1,1), mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
  dome_nams<-unlist(dome_list)#c("dome_100", "dome_75_100","dome_25_75","dome_25")
  cond<-dome_nams%in%input$dome
  
  if(sum(cond)>0){
    
    dome_max<-max(dome_maxes[cond])
    dome_min<-min(dome_mins[cond])
    lens<-seq(0.05,3.5,length.out=100)
    
    sel1<-getsel(lens,1,0.1,dome_min)
    sel2<-getsel(lens,1,0.1,dome_max)
    
    par( mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
    
    plot(c(0,3.5),c(0,1),col="white",xlab="",ylab="",axes=F)
    axis(2)
    axis(1,c(-10,10),c(-10,10))
    polygon(c(lens,lens[100:1]),c(sel1,sel2[100:1]),border=NA,col=fcol)
    lines(lens,sel1)
    lines(lens,sel2)
    
    mtext("Length",1,line=2)
    mtext("Selectivity of oldest length (SL)",2,line=2)
    abline(h=c(dome_min,dome_max),col=icol,lty=2)
    
    
  }else{
    plot(c(0,3.5),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(1.75,0.5,"< Unspecified >",col="grey")
  }
  
}



fishy<-function(x=0,y=0,scale=1,res=20,border="black",col='white',lwd=1,dead=F,reflect=F){
  
  x1<-seq(0,0.4,length.out=res)
  x2<-seq(0.41,0.8,length.out=res)
  x3t<-seq(-0.5,0,length.out=res)
  x3<-(x3t)*0.4+1
  x4<-((x3/3)+0.6666)[res:1]
  tf<-((1/(1+10*(0.4-x1)^4))*(3/2)-0.692)
  bf<-1-tf
  tr<-((1/(1+10*(x2-0.4)^2)))*(0.48)+0.326
  br<-1-tr
  tt<-(-(x3t)^2)*1.3+0.835
  bt<-1-tt
  xs=c(x1,x2,x3,x4,x4[res:1],x3[res:1],x2[res:1],x1[res:1])
  ys=c(tf,tr,tt,tt[res:1],bt,bt[res:1],br[res:1],bf[res:1])
  if(reflect)xs=1-xs
  xs<-(xs-0.5)*scale+x
  ys<-(ys-0.5)*scale+y
  
  polygon(xs,ys,col=col,border=border,lwd=lwd)
  
}

boaty<-function(x,y,scale=1,res=20,border="black",col="white",lwd=1){
  
  x1<-seq(0,0.3,length.out=res)
  bf<-1+0.53-(1/(1+10*(0.4-x1)^4))*1.3
  
  x2<-c(0,   0.2,0.25,0.5,0.5,1,  1,0.3)
  rest<-c(0.5,0.5,0.7, 0.7,0.5,0.5,0.23,0.23)
  
  xs<-c(x2,x1[res:1])
  ys<-c(rest,bf[res:1])
  
  xs<-(xs-0.5)*scale+x
  ys<-(ys-0.5)*scale+y
  
  polygon(xs,ys,col=col,border=border,lwd=lwd)
  
}

fishgrid<-function(xlim,ylim,nfish,col="green",border="green",lwd=2){
  
  xl<-seq(xlim[1],xlim[2],length.out=7)[2:6]
  yl<-seq(ylim[1],ylim[2],length.out=4)[2:3]
  ind<-expand.grid(1:5,1:2)
  for(i in 1:nfish)fishy(xl[ind[i,1]],yl[ind[i,2]],scale=0.075,col=col,border=border)
  
}

DRplot<-function(DR){
  
  dfish<-floor(DR*10)
  cfish<-10-dfish#ceiling((1-DM)*10)
  
  plot(c(-0.5,0.5),c(-0.5,0.5),col='white',axes=F,xlab="",ylab="")
  abline(h=0.25)
  boaty(-0.1,0.38,0.8,col="white",border="black")
  fishgrid(c(-0.35,0.35),c(0.12,0.42),cfish,col=icol,border=icol)
  
  if(dfish>0){ #fishgrid(c(-0.6,0.05),c(-0.3,0.0),dfish,col=icol,border=icol)
    
    xs<-c(0.3, 0.45, 0.2, 0.25, 0.18, 0.16, 0.21,  0.29, 0.35, 0.41)+0.03
    ys<-c(-0.3,-0.45,-0.25,-0.1,-0.45, -0.28,-0.35, -0.2, -0.1, -0.4)
    ref<-c(F,F,F,T,T,F,T,F,T,F)
    plotf<-sample(1:10,dfish)
    for(i in plotf){
      fishy(xs[i],ys[i],scale=0.075,col=fcol,border=fcol,reflect=ref[i])
    }
    
  }
  
  arrows(x0=0.1,x1=0.2,y0=0.12,y1=-0.03,col=fcol,lwd=2,length=0.1)
  text(0.31,0.08,"DR",font=2,col=fcol)
  
  #arrows(x0=0.13,x1=0.01,y0=-0.28,y1=-0.25,col='black',lwd=2,length=0.1)
  #text(0.0,-0.4,"PRM",font=2)
  
}


PRMplot<-function(PRM){
  
  dfish<-floor(PRM*10)
  cfish<-10-dfish#ceiling((1-DM)*10)
  
  plot(c(-0.5,0.5),c(-0.5,0.5),col='white',axes=F,xlab="",ylab="")
  abline(h=0.25)
  boaty(-0.1,0.38,0.8,col="white",border="black")
  #fishgrid(c(-0.35,0.35),c(0.12,0.42),cfish,col=icol,border=icol)
  fishgrid(c(-0.6,0.05),c(-0.3,0.0),dfish,col=icol,border=icol)
  if(cfish>0){
    
    xs<-c(0.3, 0.45, 0.2, 0.25, 0.18, 0.16, 0.21,  0.29, 0.35, 0.41)+0.03
    ys<-c(-0.3,-0.45,-0.25,-0.1,-0.45, -0.28,-0.35, -0.2, -0.1, -0.4)
    ref<-c(F,F,F,T,T,F,T,F,T,F)
    plotf<-sample(1:10,cfish)
    for(i in plotf){
      fishy(xs[i],ys[i],scale=0.075,col=fcol,border=fcol,reflect=ref[i])
    }
    
  }
  
  arrows(x0=0.1,x1=0.2,y0=0.12,y1=-0.03,col=fcol,lwd=2,length=0.1)
  text(0.31,0.08,"DR",font=2,col=fcol)
  
  arrows(x0=0.13,x1=0.01,y0=-0.28,y1=-0.25,col='black',lwd=2,length=0.1)
  text(0.0,-0.4,"PRM",font=2,col=icol)
  
}



plotDR <- function(dummy=1){
  
  DR_nams<-unlist(DR_list)#c("DM_1", "DM_1_10","DM_10_30","DM_30_50","DM_50_70")
  
  cond<-DR_nams%in%input$DR
  
  if(sum(cond)>0){
    
    par(mfrow=c(1,2),mai=c(0.01,0.2,0.01,0.01), omi=c(0.4,0.01,0.55,0.01), cex.main = 1.5, cex.lab=1.35 )
    
    DR_max<-max(DR_maxes[cond])
    DR_min<-min(DR_mins[cond])
    
    DRplot(DR_min)
    mtext(paste0("Lowest rate = ",round(DR_min*100),"%"),3,line=2)
    DRplot(DR_max)
    mtext(paste0("Highest rate = ",round(DR_max*100),"%"),3,line=2)
    
  }else{
    plot(c(0,3.5),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(1.75,0.5,"< Unspecified >",col="grey")
    #text(1.75,0.25,input$DR,col="grey")
    #text(1.75,0.75,DR_nams,col="grey")
  }
  
}



plotPRM <- function(dummy=1){
  
  PRM_nams<-unlist(PRM_list)#c("PRM_1", "PRM_1_10","PRM_10_30","PRM_30_50","PRM_50_70")
  
  cond<-PRM_nams%in%input$PRM
  
  if(sum(cond)>0){
    
    par(mfrow=c(1,2),mai=c(0.01,0.2,0.01,0.01), omi=c(0.4,0.01,0.55,0.01), cex.main = 1.5, cex.lab=1.35 )
    
    PRM_max<-max(PRM_maxes[cond])
    PRM_min<-min(PRM_mins[cond])
    
    PRMplot(PRM_min)
    mtext(paste0("Lowest rate = ",round(PRM_min*100),"%"),3,line=2)
    PRMplot(PRM_max)
    mtext(paste0("Highest rate = ",round(PRM_max*100),"%"),3,line=2)
    
  }else{
    plot(c(0,3.5),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(1.75,0.5,"< Unspecified >",col="grey")
    #text(1.75,0.25,input$DM,col="grey")
    #text(1.75,0.75,DM_nams,col="grey")
  }
  
}



plotsigR <- function(dummy=1){
  
  sigR_nams<- unlist(sigR_list)#c("sigR_10", "sigR_10_30","sigR_30_60","sigR_60_90","sigR_90")
  
  cond<-sigR_nams%in%input$sigR
  
  if(sum(cond)>0){
    
    layout(matrix(c(1,2,3),nrow=1),widths=c(2,2,1))
    par(mai=c(0.3,0.5,0.01,0.01), omi=c(0.4,0.01,0.55,0.01), cex.main = 1.5, cex.lab=1.35 )
    maxcol<-fcol
    mincol<-icol
    mucol<-"black"
    pch=19
    ny<-40
    yord<-order(runif(ny))
    sigR_max<-max(sigR_maxes[cond])
    sigR_min<-min(sigR_mins[cond])
    hh<-0.45
    D<-seq(0,1,length.out=ny)
    murec<-(0.8*hh*D)/(0.2*(1-hh)+(hh-0.2)*D)
    muR_max<--0.5*sigR_max^2
    muR_min<--0.5*sigR_min^2
    ld_max<-rnorm(ny,muR_max,sigR_max)
    ld_min<-rnorm(ny,muR_min,sigR_min)
    rd_max<-exp(ld_max)
    rd_min<-exp(ld_min)
    recs_max<-rd_max*murec
    recs_min<-rd_min*murec
    
    plot(D,murec,ylim=c(0,quantile(c(rd_max,rd_min),0.95)),type="l",lwd=2,xlab="",ylab="",col=mucol,axes=F)
    axis(1)
    axis(2)
    mtext("SSB relative to unfished",1,line=2.5)
    mtext("Recruitment relative to unfished",2,line=2.5)
    points(D,recs_max,col=maxcol,pch=pch)
    points(D,recs_min,col=mincol,pch=pch)
    legend('topleft',legend=c("Highest","Lowest","Mean"),text.col=c(fcol,icol,"black"),text.font=2,bty="n")
    
    ylim<-c(-1,1)*max(abs(range(c(ld_max,ld_min))))
    plot(c(0.5,ny),ylim,col='white',axes=F,xlab="",ylab="")
    abline(h=0,lwd=2,col=mucol)
    points(ld_max[yord]-muR_max,col=maxcol,pch=pch)
    points(ld_min[yord]-muR_min,col=mincol,pch=pch)
    axis(2)
    axis(1,c(-100,100),c(-100,100))
    mtext("Year",1,line=2.1)
    mtext("Recruitment deviation",2,line=2)
    
    d_max<-density(rnorm(10000,muR_max,sigR_max))
    d_min<-density(rnorm(10000,muR_min,sigR_min))
    
    scale<-max(d_min$y)
    
    plot(c(0,1),ylim,axes=F,xlab="",ylab="",col="white")
    
    polygon(x=d_max$y/scale,y=d_max$x-muR_max,col=maxcol,border=maxcol)
    polygon(x=d_min$y/scale,y=d_min$x-muR_min,col=mincol,border=mincol)
    legend('topright',legend=c(sigR_max,sigR_min),text.font=2,text.col=c(fcol,icol),bty='n')
    
  }else{
    plot(c(0,3.5),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(1.75,0.5,"< Unspecified >",col="grey")
    #text(1.75,0.25,input$DM,col="grey")
    #text(1.75,0.75,DM_nams,col="grey")
  }
  
}



plotq <- function(dummy=1){
  
  q_nams<-unlist(q_list)#c("q_d3_d2","q_d2_d1","q_d1_1","q_1_2","q_2_3")
  cond<-q_nams%in%input$q
  
  if(sum(cond)>0){
    
    ny<-75
    maxcol<-fcol2
    mincol<-icol
    q_max<-max(q_maxes[cond])
    q_min<-min(q_mins[cond])
    qy_max<-(1+q_max/100)^(1:ny)
    qy_min<-(1+q_min/100)^(1:ny)
    
    par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
    
    plot(c(0.5,ny)+2018,c(0,2.5),col="white",xlab="",ylab="")
    polygon(2018+c(1:ny,ny:1),c(qy_max,qy_min[ny:1]),border=NA,col=fcol)
    lines((1:ny)+2018,qy_max,col=maxcol)
    lines((1:ny)+2018,qy_min,col=mincol)
    hmin<-hmax<-0.5
    if(q_max>0)hmax<-2
    if(q_min>0)hmin<-2
    
    abline(h=1)
    abline(h=c(hmax,hmin),col=c(maxcol,mincol),lty=2)
    vmax<-log(hmax,1+q_max/100)
    vmin<-log(hmin,1+q_min/100)
    abline(v=2018+c(vmax,vmin),col=c(maxcol,mincol),lty=2)
    
    mtext("Year",1,line=2)
    mtext("Catchability relative to today (q)",2,line=2)
    text(vmax-10+2018,0.03,paste(round(vmax),"years"),col=maxcol)
    text(vmin-10+2018,0.24,paste(round(vmin),"years"),col=mincol)
    legend('topleft',legend=c(paste("Highest = ",q_max,"%"),paste("Lowest = ",q_min,"%")),text.col=c(maxcol,mincol),bty='n',text.font=1)
    
    
  }else{
    plot(c(0,3.5),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(1.75,0.5,"< Unspecified >",col="grey")
  }
  
}



fishgrid2<-function(nfish,fcol="red",mpacol="green"){
  
  nfish2<-floor(nfish/5)
  xlim<-c(0,1)
  ylim<-c(0,1)
  xl<-seq(xlim[1],xlim[2],length.out=6)[2:5]
  yl<-seq(ylim[1],ylim[2],length.out=7)[2:6]
  ind<-expand.grid(1:4,1:5)
  cols<-rep(fcol,20)
  cols[0:nfish2]<-mpacol
  
  for(i in 1:20)fishy(xl[ind[i,1]],yl[ind[i,2]],scale=0.09,col=cols[i],border=cols[i])
  
}

plotA <- function(dummy=1){
  
  A_nams<-unlist(A_list)#c("A_1", "A_1_5", "A_5_10", "A_10_20", "A_20_30", "A_30_40", "A_40_50")
  cond<-A_nams%in%input$A
  
  if(sum(cond)>0){
    
    Amax<-max(A_maxes[cond])*100
    Amin<-min(A_mins[cond])*100
    
    par(mfrow=c(1,2),mai=c(0.01,0.2,0.01,0.01), omi=c(0.4,0.01,0.55,0.01), cex.main = 1.5, cex.lab=1.35 )
    plot(c(0,1),c(0,1),col="white",axes=F)
    fishgrid2(Amin,fcol=icol,mpacol=fcol)
    mtext("Smallest",3,line=0.4)
    plot(c(0,1),c(0,1),col="white",axes=F)
    fishgrid2(Amax,fcol=icol,mpacol=fcol)
    mtext("Largest",3,line=0.4)
    
    
  }else{
    
    plot(c(0,3.5),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(1.75,0.5,"< Unspecified >",col="grey")
    
  }
  
}




fishgrid3<-function(Prob,fcol="red",mpacol="green"){
  
  nfish<-floor(Prob*100)
  xlim<-c(0,1)
  ylim<-c(0,1)
  xl<-seq(xlim[1],xlim[2],length.out=22)[2:21]
  yl<-seq(ylim[1],ylim[2],length.out=22)[2:21]
  ind<-expand.grid(1:20,1:20)
  cols<-array(fcol,c(20,20))
  cols[1:10,1:10]<-mpacol
  
  indmpa<-as.matrix(expand.grid(1:10,1:10))
  inds1<-indmpa[sample(1:100,nfish),]
  cols[inds1]<-fcol
  
  indf<-as.matrix(rbind(expand.grid(11:20,1:20),expand.grid(1:10,11:20)))
  inds2<-indf[sample(1:300,nfish),]
  cols[inds2]<-mpacol
  
  indall<-as.matrix(expand.grid(1:20,1:20))
  
  for(i in 1:400)fishy(xl[indall[i,1]],yl[indall[i,2]],scale=0.035,col=cols[indall[i,1],indall[i,2]],border=cols[indall[i,1],indall[i,2]])
  
}


plotV <- function(dummy=1){
  
  V_nams<-unlist(V_list)#c("P_1", "P_1_5", "P_5_10", "P_10_20", "P_20")
  cond<-V_nams%in%input$V
  
  if(sum(cond)>0){
    
    Vmax<-max(V_maxes[cond])
    Vmin<-min(V_mins[cond])
    
    par(mfrow=c(1,2),mai=c(0.01,0.01,0.01,0.01), omi=c(0.01,0.01,0.5,0.01), cex.main = 1.5, cex.lab=1.35 )
    plot(c(0,1),c(0,1),axes=F,col="white")
    text(0.25,0.25,"MPA",cex=2.7,font=2,col="grey78")
    fishgrid3(Vmin,fcol=icol,mpacol=fcol)
    polygon(c(0.02,0.5,0.5,0.02),c(0.02,0.02,0.5,0.5),col=NA,border='black')
    mtext("Lowest mixing",3,line=0.4)
    plot(c(0,1),c(0,1),axes=F,col="white")
    text(0.25,0.25,"MPA",cex=2.7,font=2,col="grey78")
    fishgrid3(Vmax,fcol=icol,mpacol=fcol)
    mtext("Highest mixing",3,line=0.4)
    polygon(c(0.02,0.5,0.5,0.02),c(0.02,0.02,0.5,0.5),col=NA, border='black')
    
    
    
  }else{
    
    plot(c(0,3.5),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(1.75,0.5,"< Unspecified >",col="grey")
    
  }
  
}


