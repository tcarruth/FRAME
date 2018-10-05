
# Redo MSE functions

redoEval<-function(fease=F){

  withProgress(message = "Calculating Evaluation results", value = 0, {
    incrate<-1/5
    incProgress(incrate)
    burnin<<-input$burnin
    Ptab1<<-Ptab(MSEobj,MSEobj_reb,burnin=burnin,rnd=0)
    incProgress(incrate)
    thresh<<-c(input$P111a,input$P111b,input$P112,input$P121a,input$P121b)
    temp<-Ptab_ord(Ptab1,burnin=burnin,ntop=input$ntop,fease=fease,thresh=thresh)
    incProgress(incrate)
    Ptab2<<-temp[[1]]
    MPcols<<-temp[[2]]
    MSEobj_top<<-Sub(MSEobj,MPs=Ptab2$MP)
    MSEobj_reb_top<<-Sub(MSEobj_reb,MPs=Ptab2$MP)
    #save(MSEobj_top,file="MSEobj_top")
    #save(MSEobj_reb_top,file="MSEobj_reb_top")
    nMPs<-length(MSEobj_top@MPs)
    updateTextAreaInput(session,"Debug1",value=Ptab2$MP)
    output$Ptable <- function()Ptab_formatted(Ptab2,burnin=burnin,cols=MPcols,thresh=thresh)
    incProgress(incrate)
    output$threshtable<-function()Thresh_tab(thresh)
    output$P1_LTY<-renderPlot(P1_LTY_plot(MSEobj_top,burnin,MPcols=MPcols),height=400,width=400)
    output$P2_LTY<-renderPlot(P2_LTY_plot(MSEobj_top,MPcols=MPcols),height=400,width=400)
    output$P3_LTY<-renderPlot(P3_LTY_plot(MSEobj_top,MSEobj_reb_top,MPcols=MPcols),height=400,width=400)
    output$wormplot<-renderPlot(Pplot3(MSEobj_top,MPcols=MPcols), height =ceiling(nMPs/6)*320 , width = 1300)
    output$wormplot2<-renderPlot(Rplot(MSEobj_reb_top,MPcols=MPcols), height =ceiling(nMPs/6)*320 , width = 1300)
    output$PI111_uncertain<-renderPlot(MSC_uncertain(MSEobj_top,MPcols=MPcols,maxMPs=MSEobj_top@nMPs, LTL=F,inc_thresh = F,burnin=burnin),height =ceiling(nMPs/6)*400 , width = 1100)

    incProgress(incrate)
    VOIout<<-getVOI(MSEobj_top)
    output$CCU<-renderPlot(CCU_plot(VOIout,MSEobj_top,MPcols=MPcols),height=ceiling(nMPs/3)*290,width=1300)
    output$Eval_Converge<-renderPlot(Converge(MSEobj_top,PMs = list(Yield, P10),ncol=2,nrow=1),height =400 , width = 1300)



  })
}

redoApp<-function(fease=F){
  withProgress(message = "Calculating Application results", value = 0, {
    incrate<-1/5
    incProgress(incrate)


    burnin<<-input$burnin
    Ptab1_app<<-Ptab(MSEobj_app,MSEobj_reb_app,burnin=burnin,rnd=0,App=T)
    thresh<<-c(input$P111a,input$P111b,input$P112,input$P121a,input$P121b)
    temp<-Ptab_ord(Ptab1_app,burnin=burnin,ntop=input$ntop, Eval=F,fease=fease,thresh=thresh)
    incProgress(incrate)

    Ptab2_app<<-temp[[1]]
    MPcols_app<<-temp[[2]]
    output$App_Ptable <- function()Ptab_formatted(Ptab2_app,burnin=burnin,cols=MPcols_app,thresh=thresh)
    output$App_threshtable<-function()Thresh_tab(thresh)
    incProgress(incrate)
    MSEobj_app<-Sub(MSEobj_app,MSEobj_app@MPs[2])
    MSEobj_reb_app<-Sub(MSEobj_reb_app,MSEobj_reb_app@MPs[2])

    output$MSC_PMs<-renderPlot(MSC_PMs(MSEobj_app,MSEobj_reb_app,MPcols=MPcols_app),height=1000,width=900)
    output$App_wormplot<-renderPlot(Pplot3(MSEobj_app,MPcols=MPcols_app,maxcol=1,maxrow=2), height =450 , width =550)
    output$App_wormplot2<-renderPlot(Pplot4(MSEobj_app,MPcols=MPcols_app,maxcol=1,maxrow=2), height =450 , width =550)
    output$App_wormplot3<-renderPlot(Rplot(MSEobj_reb_app,MPcols=MPcols_app,maxcol=1,maxrow=2), height =450 , width =550)
    output$App_PI111_uncertain<-renderPlot(MSC_uncertain(MSEobj_app,MPcols=MPcols_app,maxMPs=MSEobj_app@nMPs, LTL=F,inc_thresh = F,burnin=burnin),height =450 , width =550)

    VOIout_app<<-getVOI(MSEobj_app)
    incProgress(incrate)

    output$App_CCU<-renderPlot(CCU_plot(VOIout_app,MSEobj_app,MPcols=MPcols_app,maxrow=1,maxcol=1),height =550 , width =550)
    output$App_VOI<-renderPlot(VOI_MSC(MSEobj_app,MPcols=MPcols_app),height =550 , width =550)

    incProgress(incrate)

  })
}

redoInd<-function(){

  styr=MSEobj_app@nyears
  PPD<-MSEobj_app@Misc[[2]]

  # Standardization
  PPD@Cat<-PPD@Cat/PPD@Cat[,styr]
  PPD@Ind<-PPD@Ind/PPD@Ind[,styr]
  PPD@ML<-PPD@ML/PPD@ML[,styr]

  tsd= c("Cat","Cat","Cat","Ind","Ind","ML", "ML")
  stat=c("slp","AAV","mu","slp","mu", "slp","mu")
  res<-input$Ind_Res
  datayears<-dim(dat_ind@Cat)[2]

  if(datayears-MSEobj_app@nyears < res){
    res=datayears-MSEobj_app@nyears
  }else if(datayears-MSEobj_app@nyears < 3){
    return(0)
  }

  indPPD<-getinds(PPD,styr=styr,res=res,tsd=tsd,stat=stat)

  # Standardization
  dat_ind@Cat<-dat_ind@Cat/dat_ind@Cat[,styr]
  dat_ind@Ind<-dat_ind@Ind/dat_ind@Ind[,styr]
  dat_ind@ML<-dat_ind@ML/dat_ind@ML[,styr]

  indData<-getinds(dat_ind,styr=styr,res=res,tsd=tsd,stat=stat)

  output$CC<-renderPlot( CC(indPPD,indData,pp=1,res=res),height =900 ,width=900)
  output$mdist<-renderPlot(plot_mdist(indPPD,indData,alpha=input$Ind_Alpha),height =550 ,width=550)

}
