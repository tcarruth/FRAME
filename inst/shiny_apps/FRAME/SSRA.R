
# Stochastic SRA wrapper

SSRA_wrap<-function(OM,Data,simno=1){


  CAA<-Data@CAA[simno,,]
  Chist<-Data@Cat[simno,]
  Ind<-Data@Ind[simno,]
  CAL=NA
  ML<-Data@ML[simno,]
  mulen<-NA

  StochasticSRA_MSC(OM,CAA,Chist,Ind,ML,CAL,mulen,wts=c(1,1,0.5,0.1,1),
                     Jump_fac=1,nits=200, burnin=100,thin=10,ESS=300,MLsd=0.1,
                     ploty=F,nplot=6,SRAdir=NA)

}

