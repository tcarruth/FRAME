
library(shiny)
library(DLMtool)
library(kableExtra)
library(formattable)
library(knitr)
library(dplyr)
library(httpuv)

source("./global.R")

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {

  # -------------------------------------------------------------
  # Explanatory figures
  source("./Fishery_figs.R",local=TRUE)
  source("./Management_figs.R",local=TRUE)
  source("./Data_figs.R",local=TRUE)

  # Presentation of results
  source("./Analysis_results.R",local=TRUE)
  source("./AI_results.R",local=TRUE)
  source("./Performance_table.R",local=TRUE)
  source("./Trade_off_plots.R",local=TRUE)
  source("./VOI.R",local=TRUE)
  source("./MSC_source.R",local=TRUE)

  # OM construction / translation
  source("./makeOM.R",local=TRUE)

  # Reporting
  source("./OM_report.R",local=TRUE)

  # Miscellaneous
  source("./Misc.R",local=TRUE)
  #source('./modSampCpars.R',local=TRUE)
  source('./Fease_Functions.R',local=TRUE )
  source('./SSRA.R',local=TRUE ) # Stochastic SRA wrapper
  source('./StochasticSRA_MSC.R',local=TRUE ) # Stochastic SRA until progress bar update comes to DLMtool
  source('./Backwards.R',local=TRUE ) # Stochastic SRA until progress bar update comes to DLMtool
  #assignInNamespace("SampleCpars",SampleCpars_mod, ns="DLMtool")

  # --------------------------------------------------------------

  Fpanel<-reactiveVal(0)
  Mpanel<-reactiveVal(0)
  Dpanel<-reactiveVal(0)
  Calc<-reactiveVal(0)
  Started<-reactiveVal(0)
  Assess<-reactiveVal(0)

  output$Fpanel <- reactive({ Fpanel()})
  output$Mpanel <- reactive({ Mpanel()})
  output$Dpanel <- reactive({ Dpanel()})
  output$Calc   <- reactive({ Calc()})
  output$Started   <- reactive({ Started()})
  output$Assess  <- reactive({ Assess()})

  outputOptions(output,"Fpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Mpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Dpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Calc",suspendWhenHidden=FALSE)
  outputOptions(output,"Started",suspendWhenHidden=FALSE)
  outputOptions(output,"Assess",suspendWhenHidden=FALSE)

  output$Fpanelout <- renderText({ paste("Fishery",Fpanel(),"/ 14")})
  output$Mpanelout <- renderText({ paste("Management",Mpanel(),"/ 3")})
  output$Dpanelout <- renderText({ paste("Data",Dpanel(),"/ 4")})

  # Some useful things
  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

  CurrentYr<-as.integer(substr(as.character(Sys.time()),1,4))
  Just<-list(c("No introduction / general comments were provided",rep("No justification was provided",13)),rep("No justification was provided",3),rep("No justification was provided",4))
  FRAMEversion<<-"1.0"

  # Default simulation ttributes --------------------------------------------------------------------------------
  nyears<-68 # 1950-2018
  nsim<-48

  makeState<-function(x)rep(T,length(get(x)))

  Fpanel_names<-c("M_list","D_list","h_list","FP_list","F_list","sel_list","dome_list","DR_list","PRM_list","sigR_list","q_list","A_list","V_list")
  Mpanel_names<-c("M1_list","IB_list","IV_list")
  Dpanel_names<-c("D1_list","CB_list","Beta_list","Err_list")

  MasterList<-list(Fpanel_names,Mpanel_names,Dpanel_names)

  PanelState<-list(Fpanel=lapply(Fpanel_names, makeState),
                   Mpanel=lapply(Mpanel_names, makeState),
                   Dpanel=lapply(Dpanel_names, makeState))

  PanelState[[3]][[4]]<-c(F,F,F,T) # Exception is the final selection of the data menu - quality is a radio button default to data-poor

  getinputnames<-function(x)strsplit(x,"_")[[1]][1]

  inputnames<-list(Fpanel=lapply(Fpanel_names,getinputnames),
                   Mpanel=lapply(Mpanel_names,getinputnames),
                   Dpanel=lapply(Dpanel_names,getinputnames))

  inputtabs<-as.vector(unlist(inputnames))

  # Record all changes to tabs
  observeEvent(sapply(inputtabs, function(x) input[[x]]),{

    for(i in 1:length(PanelState)){
      for(j in 1:length(PanelState[[i]])) {
        value<-sapply(inputnames[[i]][j],function(x) input[[x]])
        PanelState[[i]][[j]] <<- get(MasterList[[i]][j])%in%value
      }
    }

  })


  # == File I/O =================

  output$Save<- downloadHandler(

    filename = paste0(namconv(input$Name),".frame"), #paste0(getwd(),"/",namconv(input$Name),".msc"),

    content=function(file){

      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)
      doprogress("Saving")
      saveRDS(MSClog,file)

    }

  )

  observeEvent(input$Analysis_type,{

    if(Started()==1){
      updateTabsetPanel(session=session, inputId="tabs1", selected = "5")
    }else if(Started()==0){
      updateTabsetPanel(session=session, inputId="tabs1", selected = "4")

    }else{
      updateTabsetPanel(session=session, inputId="tabs1", selected = "5")
    }

    if(input$Analysis_type=='Demo'){
      updateNumericInput(session,'nsim',value="24")
      updateNumericInput(session,'interval',value="8")

    }else if(input$Analysis_type=='Eval'){
      updateNumericInput(session,'nsim',value="96")
      updateNumericInput(session,'interval',value="4")

    }else{
      updateNumericInput(session,'nsim',value="196")
      updateNumericInput(session,'interval',value="4")
      #if(input$Analysis_type=='Ind')shinyjs::disable("Parallel")
      #if(input$Analysis_type!='Ind'){
      #  if(input$nsim<48) shinyjs::disable("Parallel")
       # if(input$nsim>47) shinyjs::enable("Parallel")
      #}
    }

    MPs<<-getMPs()

    if(input$Ex_Ref_MPs)MPs<<-MPs[!MPs%in%c("FMSYref","FMSYref75","FMSYref50","NFref")]

    if(input$Analysis_type%in%c("Demo","Eval")){
      shinyjs::disable("sel_MP")
      updateSelectInput(session=session,inputId="sel_MP",choices=MPs,selected=character(0))
      shinyjs::enable("ntop")

    }else{
      shinyjs::enable("sel_MP")
      updateSelectInput(session=session,inputId="sel_MP",choices=MPs,selected=selectedMP)
      shinyjs::disable("ntop")
    }
    shinyjs::disable("LTL")
    shinyjs::disable("LoadInd")
    shinyjs::disable("Power")
    Started(1)

  })

  observeEvent(input$nsim, {
    if(input$nsim<48) shinyjs::disable("Parallel")
    if(input$nsim>47) shinyjs::enable("Parallel")
  })

  observeEvent(input$Fcont,{

    if(Fpanel()==0 & Mpanel()==0 & Dpanel()==0){
      MPs<<-getMPs()
      updateSelectInput(session=session,inputId="sel_MP",choices=MPs,selected=character(0))
      selectedMP<<-MPs[2]
      shinyjs::disable("LTL")
    }

  })

  observeEvent(input$Ex_Ref_MPs, {
    MPs<<-getMPs()
    if(input$Ex_Ref_MPs){
      MPs<<-MPs[!MPs%in%c("FMSYref","FMSYref75","FMSYref50","NFref")]
      selectedMP<<-MPs[1]
    }
    if(input$Analysis_type%in%c("Demo","Eval")){
      shinyjs::disable("sel_MP")
      updateSelectInput(session=session,inputId="sel_MP",choices=MPs,selected=character(0))

    }else{
      shinyjs::enable("sel_MP")
      updateSelectInput(session=session,inputId="sel_MP",choices=MPs,selected=selectedMP)
    }

  })

  observeEvent(input$sel_MP,{
    if(input$Analysis_type%in%c("App","Ind"))  selectedMP<<-input$sel_MP
  })


  observeEvent(input$Load,{

    filey<-input$Load
    MSClog<-readRDS(file=filey$datapath)

    PanelState<<-MSClog[[1]]
    Just<<-MSClog[[2]]

    for(i in 1:length(PanelState)){
      for(j in 1:length(PanelState[[i]])) {
        if(!(i==3&j==4)){ # not the radio button
          state<-as.vector(unlist(PanelState[[i]][j]))
          choices<-as.vector(unlist(get(MasterList[[i]][j])))
          selected<-as.list(choices[state])
          choices<-as.list(choices)
          updateCheckboxGroupInput(session, as.character(inputnames[[i]][j]), selected = selected)
        }
      }
    }

    i<-3
    j<-4
    state<-as.vector(unlist(PanelState[[i]][j]))
    choices<-as.vector(unlist(get(MasterList[[i]][j])))
    selected<-as.list(choices[state])
    choices<-as.list(choices)
    updateRadioButtons(session, as.character(inputnames[[i]][j]), selected = selected)

    updateTextInput(session, "Name",     value= MSClog[[3]]$Name)
    updateTextInput(session, "Species",  value= MSClog[[3]]$Species)
    updateTextInput(session, "Region",   value= MSClog[[3]]$Region)
    updateTextInput(session, "Agency",   value= MSClog[[3]]$Agency)
    updateTextInput(session, "nyears",   value= MSClog[[3]]$nyears)
    updateTextInput(session, "Author",   value= MSClog[[3]]$Author)
    updateTextInput(session, "Justification",value=Just[[1]][1])
    updateTabsetPanel(session,"tabs1",selected="1")
    #=== DEBUGGING WINDOW =====================================================
    #updateTextAreaInput(session,"Debug",value=choices)
    #updateTextAreaInput(session,"Debug2",value=selected)
    #updateTextAreaInput(session,"Debug3",value=inputId)
    # ==========================================================================

    Fpanel(1)
    Mpanel(1)
    Dpanel(1)
    Calc(0)

  })

  observeEvent(input$LoadOBJ,{

    filey<-input$LoadOBJ
    obj<-readRDS(file=filey$datapath)

    if(class(obj)=="Data"){

      OM<<-makeOM(PanelState,nsim=nsim)
      updateTextAreaInput(session,"Debug1",value=paste(OM@nyears,ncol(obj@Cat)))
      withProgress(message = "Running SRA", value = 0, {
        OM<<-SSRA_wrap(OM,obj)
      })
      GoBackwards_SRA(OM)
      UpdateQuest()
      Just[[1]][1+c(2,4,5,6,7,10)]<<-"Estimated by Stochastic SRA"
      Assess(1)

    }else if(class(obj)=="OM"){

      GoBackwards(obj)
      UpdateQuest()

    }

    Fpanel(1)
    Mpanel(1)
    Dpanel(1)
    Calc(0)

  })

  observeEvent(input$ntop,{
    if(input$Analysis_type%in%c("Demo","Eval")&input$Calculate>0){
      redoEval()
    }
  })

  observeEvent(input$burnin,{
    if(input$Analysis_type%in%c("Demo","Eval")&input$Calculate>0){
      redoEval()
    }else if(input$Analysis_type=="App"){
      redoApp()
    }

  })


#############################################################################################################################################################################

#############################################################################################################################################################################


  observeEvent(input$Calculate,{

    Fpanel(1)
    updateTabsetPanel(session=session,inputId="tabs1", selected = "5")

    nsim<<-input$nsim
    burnin<<-input$burnin
    parallel=F
    if(input$Parallel&input$Analysis_type!="Ind"){

      if(nsim>47){
        parallel=T
        setup()
      }
    }
    OM<<-makeOM(PanelState,nsim=nsim)

    if(input$Analysis_type%in%c("Demo","Eval")){
       MPs<<-getMPs()
    }else{
       MPs<<-input$sel_MP
    }

    #tags$audio(src = "RunMSE.mp3", type = "audio/mp3", autoplay = NA, controls = NA)

    withProgress(message = "Running MSE", value = 0, {
      MSEobj<<-runMSE(OM,MPs=MPs,silent=T,control=list(progress=T),PPD=T,parallel=parallel)
    })

    MGT2<-ceiling(MSEobj@OM$MGT*2)
    MGT2[MGT2<5]<-5
    MGT2[MGT2>20]<-20

    OM_reb<-OM
    OM@proyears<-max(MGT2)+2 # only have to compute to this year
    OM_reb@cpars$D<-MSEobj@OM$SSBMSY_SSB0/2#apply(MSEobj@SSB_hist[,,MSEobj@nyears,],1, sum)/(MSEobj@OM$SSB0*2) # start from half BMSY

    #temp<-new('OM',Albacore,Generic_Fleet,Perfect_Info,Perfect_Imp)
    #OM_reb<-Replace(OM_reb,temp,Sub="Obs")
    #OM_reb<-Replace(OM_reb,temp,Sub="Imp")

    if(input$Analysis_type!="Ind"){
      withProgress(message = "Rebuilding evaluation", value = 0, {
        MSEobj_reb<<-runMSE(OM_reb,MPs=MPs,silent=T,control=list(progress=T),parallel=parallel)
      })
      save(MSEobj_reb,file="MSEobj_reb")
    }

    save(MSEobj,file="MSEobj")


    # ==== Types of reporting ==========================================================

    if(input$Analysis_type%in%c("Demo","Eval")){ # Demonstration or evaluation
     redoEval()
      Calc(1)
    } else if(input$Analysis_type=="App"){ # FIP presentations
     redoApp()
      Calc(2)
    } else { # leaving just generic risk assessment
     redoInd()
      Calc(3)

      PPD<-MSEobj@Misc[[1]]
      tsd= c("Cat","Cat","Cat","Ind","Ind","ML", "ML")
      stat=c("slp","AAV","mu","slp","mu", "slp","mu")
      res<-burnin

      indPPD<-getinds(PPD,styr=MSEobj@nyears,res=res,tsd=tsd,stat=stat)
      indData<-matrix(indPPD[,1,1],ncol=1)

      output$CC<-renderPlot(CC(indPPD,indData,pp=1,res=res),height=700,width=700)
      output$MahD<-renderPlot(plot_mdist(indPPD,indData),height=400,width=400)

    }

  })

  redoEval<-function(){
    burnin=input$burnin
    Ptab1<<-Ptab(MSEobj,MSEobj_reb,burnin=burnin,rnd=0)
    Ptab2<<-Ptab_ord(Ptab1,burnin=burnin,ntop=input$ntop)
    MSEobj_top<<-Sub(MSEobj,MPs=Ptab2$MP)
    MSEobj_reb_top<<-Sub(MSEobj_reb,MPs=Ptab2$MP)
    save(MSEobj_top,file="MSEobj_top")
    save(MSEobj_reb_top,file="MSEobj_reb_top")
    nMPs<-length(MSEobj_top@MPs)
    updateTextAreaInput(session,"Debug1",value=Ptab2$MP)
    output$Ptable <- function()Ptab_formatted(Ptab2,burnin=burnin)
    output$threshtable<-function()Thresh_tab()
    output$P1_LTY<-renderPlot(P1_LTY_plot(MSEobj_top,burnin,MPcols=MPcols),height=400,width=400)
    output$P2_LTY<-renderPlot(P2_LTY_plot(MSEobj_top,MPcols=MPcols),height=400,width=400)
    output$P3_LTY<-renderPlot(P3_LTY_plot(MSEobj_top,MSEobj_reb_top,MPcols=MPcols),height=400,width=400)
    output$wormplot<-renderPlot(Pplot3(MSEobj_top,MPcols=MPcols), height =ceiling(nMPs/6)*320 , width = 1300)
    output$wormplot2<-renderPlot(Rplot(MSEobj_reb_top,MPcols=MPcols), height =ceiling(nMPs/6)*320 , width = 1300)
    output$PI111_uncertain<-renderPlot(MSC_uncertain(MSEobj_top,MPcols=MPcols,maxMPs=MSEobj_top@nMPs, LTL=F,inc_thresh = F,burnin=burnin),height =ceiling(nMPs/6)*250 , width = 1100)
    VOIout<<-getVOI(MSEobj_top)
    output$CCU<-renderPlot(CCU_plot(VOIout,MSEobj_top,MPcols=MPcols),height=ceiling(nMPs/3)*290,width=1300)
  }

  redoApp<-function(){
    burnin=input$burnin
    Ptab1<<-Ptab(MSEobj,MSEobj_reb,burnin=burnin,rnd=0)
    Ptab2<<-Ptab_ord(Ptab1,burnin=burnin,ntop=input$ntop)
    output$App_Ptable <- function()Ptab_formatted(Ptab2,burnin=burnin)
    output$App_threshtable<-function()Thresh_tab()
    output$MSC_PMs<-renderPlot(MSC_PMs(MSEobj,MSEobj_reb,MPcols=MPcols),height=800,width=900)
    output$App_wormplot<-renderPlot(Pplot3(MSEobj,MPcols=MPcols,maxcol=1,maxrow=2), height =450 , width =550)
    output$App_wormplot2<-renderPlot(Pplot4(MSEobj,MPcols=MPcols,maxcol=1,maxrow=2), height =450 , width =550)
    output$App_wormplot3<-renderPlot(Rplot(MSEobj_reb,MPcols=MPcols,maxcol=1,maxrow=2), height =450 , width =550)
    output$App_PI111_uncertain<-renderPlot(MSC_uncertain(MSEobj,MPcols=MPcols,maxMPs=MSEobj@nMPs, LTL=F,inc_thresh = F,burnin=burnin),height =450 , width =550)
    VOIout<<-getVOI(MSEobj)
    output$App_CCU<-renderPlot(CCU_plot(VOIout,MSEobj,MPcols=MPcols,maxrow=1,maxcol=1),height =550 , width =550)
    output$App_VOI<-renderPlot(VOI_MSC(MSEobj,MPcols=MPcols),height =550 , width =550)
  }

  redoInd<-function(){
    PPD<-MSEobj@Misc[[1]]
    tsd= c("Cat","Cat","Cat","Ind","Ind","ML", "ML")
    stat=c("slp","AAV","mu","slp","mu", "slp","mu")
    res<-6
    indPPD<-getinds(PPD,styr=27,res=res,tsd=tsd,stat=stat)
    indData<-matrix(indPPD[,1,1],ncol=1)

    output$CC<-renderPlot( CC(indPPD,indData,pp=1,res=res),height =1300 ,width=1300)
    output$mdist<-renderPlot(plot_mdist(indPPD,indData),height =550 ,width=550)

  }

  observeEvent(input$D1,{
    if(Calc()!=0){
      if(input$Analysis_type%in%c("Demo","Eval")){ # Certification or demo of certification
        redoEval()
      } else if(input$Analysis_type=="App"){ # FIP presentations
        redoApp()
      } else { # leaving just generic risk assessment
        redoInd()
     }
    }
  })

  observeEvent(input$M1,{
    if(Calc()!=0){
    if(input$Analysis_type%in%c("Demo","Eval")){ # Certification or demo of certification
      redoEval()
    } else if(input$Analysis_type=="App"){ # One MP application
      redoApp()
    } else { # leaving just ancilliary indicators
      redoInd()
    }
    }
  })

  # OM report
  output$Build_OM <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0(namconv(input$Name),"_OM.html"), #"report.html",

    content = function(file) {
      doprogress("Building OM report",1)
      OM<<-makeOM(PanelState,nsim=nsim)
      src <- normalizePath('OMRep.Rmd')

      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'OMRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Operating Model Specification Report for ",input$Name),
                     set_type=paste0(switch(input$Analysis_type,
                                     "Demo"="Demonstration evaluation analysis",
                                     "Eval" = "Evaluation of MPs analysis",
                                     "App" = "Application of an MP",
                                     "Ind" = "Ancillary indicators report")," (FRAME version ",FRAMEversion,")"),

                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     ntop=input$ntop,
                     inputnames=inputnames,
                     SessionID=SessionID,
                     copyright="copyright (c) NRDC 2018"
      )

      output<-render(input="OMRep.Rmd",output_format="html_document", params = params)
      file.copy(output, file)

    }
  )


  # MSE report
  output$Build_Eval <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0(namconv(input$Name),"_Eval.html"), #"report.html",

    content = function(file) {

      src <- normalizePath('EvalRep.Rmd')
      doprogress("Building evaluation report",1)
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'EvalRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Evaluation Report for ",input$Name),
                     set_type=paste0(switch(input$Analysis_type,
                                            "Demo"="Demonstration evaluation analysis",
                                            "Eval" = "Evaluation of MPs analysis",
                                            "App" = "Application of an MP",
                                            "Ind" = "Ancillary indicators report")," (FRAME version ",FRAMEversion,")"),

                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     MSEobj=MSEobj,
                     MSEobj_reb=MSEobj_reb,
                     MSEobj_top=MSEobj_top,
                     MSEobj_reb_top=MSEobj_reb_top,
                     MPcols=MPcols,
                     ntop=input$ntop,
                     burnin=burnin,
                     SessionID=SessionID,
                     copyright="copyright (c) NRDC 2018"
                     )

      out<-render("EvalRep.Rmd", params = params)
      file.rename(out, file)

    }

  )
  # Application report
  output$Build_App <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0(namconv(input$Name),"_App.html"), #"report.html",

    content = function(file) {

      src <- normalizePath('AppRep.Rmd')
      doprogress("Building evaluation report",1)
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'AppRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Application Report for ",input$Name),
                     set_type=paste0(switch(input$Analysis_type,
                                            "Demo"="Demonstration evaluation analysis",
                                            "Eval" = "Evaluation of MPs analysis",
                                            "App" = "Application of an MP",
                                            "Ind" = "Ancillary indicators report")," (FRAME version ",FRAMEversion,")"),

                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     MSEobj=MSEobj,
                     MSEobj_reb=MSEobj_reb,
                     MPcols=MPcols,
                     ntop=input$ntop,
                     burnin=burnin,
                     SessionID=SessionID,
                     copyright="copyright (c) NRDC 2018"
      )

      out<-render("AppRep.Rmd", params = params)
      file.rename(out, file)

    }

  )

  output$Build_AI <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0(namconv(input$Name),"_AI.html"), #"report.html",
    content = function(file) {
      doprogress("Building AI report",1)
      src <- normalizePath('IndRep.Rmd')

      test<-match(input$sel_MP,MPs)
      if(is.na(test))mm<-1
      if(!is.na(test))mm<-test

      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'IndRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Ancillary Indicator Analysis Report for ",input$Name),
                     set_type=paste0(switch(input$Analysis_type,
                                            "Demo"="Demonstration evaluation analysis",
                                            "Eval" = "Evaluation of MPs analysis",
                                            "App" = "Application of an MP",
                                            "Ind" = "Ancillary indicators report")," (FRAME version ",FRAMEversion,")"),

                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     inputnames=inputnames,
                     MSEobj=MSEobj,
                     mm=mm,
                     ntop=input$ntop,
                     burnin=burnin,
                     SessionID=SessionID,
                     copyright="copyright (c) NRDC 2018"
      )

      out<-render("IndRep.Rmd", params = params)
      file.rename(out, file)

    }
  )

  # Conditioning report
  output$Build_Cond <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0(namconv(input$Name),"_Cond.html"), #"report.html",

    content = function(file) {
      doprogress("Building Conditioning report",1)
      OM<<-makeOM(PanelState,nsim=nsim)
      src <- normalizePath('CondRep.Rmd')

      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'CondRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Operating Model Conditioning Report for ",input$Name),
                     set_type=paste0("Demonstration Condinging analysis"," (FRAME version ",FRAMEversion,")"),
                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     ntop=input$ntop,
                     inputnames=inputnames,
                     SessionID=SessionID,
                     copyright="copyright (c) NRDC 2018"
      )

      output<-render(input="CondRep.Rmd",output_format="html_document", params = params)
      file.copy(output, file)

    }
  )

  # Fishery panel reactions ============================================================================================================

  observeEvent(input$Justification,{
    RecJust()
  })


  observeEvent(input$tabs1, {

    UpJust()
    Des<<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
    MSCsave_auto()
    #getMPs()
    selectedMP<<-MPs[2]

  })

  observeEvent(input$Fback,{

    if(input$tabs1==1 && Fpanel() >1){
      Fpanel(Fpanel()-1)
    }else if(input$tabs1==2 && Mpanel() >1){
      Mpanel(Mpanel()-1)
    }else if(input$tabs1==3 && Dpanel() >1){
      Dpanel(Dpanel()-1)
    }

    UpJust()
    Des<<-list(Name=input$Name,Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
    MSCsave_auto()

  })

  observeEvent(input$Fcont,{

    if(input$tabs1==1 && Fpanel() < 14){
      Fpanel(Fpanel()+1)
    }else if(input$tabs1==2 && Mpanel() < 3){
      Mpanel(Mpanel()+1)
    }else if(input$tabs1==3 && Dpanel() < 4){
      Dpanel(Dpanel()+1)
    }

    # Write old values
    UpJust()
    Des<<-list(Name=input$Name,Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
    MSCsave_auto()

  })


  # ---- Fishery all switches -----------------

  observeEvent(input$All_M,
     if(input$All_M == 0 | input$All_M%%2 == 0){
       updateCheckboxGroupInput(session,"M",choices=M_list,selected=M_list)
     }else{
       updateCheckboxGroupInput(session,"M",choices=M_list)
     }
  )
  observeEvent(input$All_D,
     if(input$All_D == 0 | input$All_D%%2 == 0){
        updateCheckboxGroupInput(session,"D",choices=D_list,selected=D_list)
     }else{
        updateCheckboxGroupInput(session,"D",choices=D_list)
     }
  )
  observeEvent(input$All_h,
     if(input$All_h == 0 | input$All_h%%2 == 0){
        updateCheckboxGroupInput(session,"h",choices=h_list,selected=h_list)
     }else{
        updateCheckboxGroupInput(session,"h",choices=h_list)
     }
  )
  observeEvent(input$All_FP,
     if(input$All_FP == 0 | input$All_FP%%2 == 0){
        updateCheckboxGroupInput(session,"FP",choices=FP_list,selected=FP_list)
     }else{
        updateCheckboxGroupInput(session,"FP",choices=FP_list)
     }
  )
  observeEvent(input$All_F,
     if(input$All_F == 0 | input$All_F%%2 == 0){
        updateCheckboxGroupInput(session,"F",choices=F_list,selected=F_list)
     }else{
        updateCheckboxGroupInput(session,"F",choices=F_list)
     }
  )
  observeEvent(input$All_sel,
     if(input$All_sel == 0 | input$All_sel%%2 == 0){
        updateCheckboxGroupInput(session,"sel",choices=sel_list,selected=sel_list)
     }else{
        updateCheckboxGroupInput(session,"sel",choices=sel_list)
     }
  )
  observeEvent(input$All_dome,
     if(input$All_dome == 0 | input$All_dome%%2 == 0){
        updateCheckboxGroupInput(session,"dome",choices=dome_list,selected=dome_list)
     }else{
        updateCheckboxGroupInput(session,"dome",choices=dome_list)
    }
  )
  observeEvent(input$All_DR,
     if(input$All_DR == 0 | input$All_DR%%2 == 0){
        updateCheckboxGroupInput(session,"DR",choices=DR_list,selected=DR_list)
     }else{
        updateCheckboxGroupInput(session,"DR",choices=DR_list)
     }
  )
  observeEvent(input$All_PRM,
    if(input$All_PRM == 0 | input$All_PRM%%2 == 0){
        updateCheckboxGroupInput(session,"PRM",choices=PRM_list,selected=PRM_list)
    }else{
        updateCheckboxGroupInput(session,"PRM",choices=PRM_list)
    }
  )
  observeEvent(input$All_sigR,
     if(input$All_sigR == 0 | input$All_sigR%%2 == 0){
        updateCheckboxGroupInput(session,"sigR",choices=sigR_list,selected=sigR_list)
     }else{
        updateCheckboxGroupInput(session,"sigR",choices=sigR_list)
     }
  )
  observeEvent(input$All_q,
     if(input$All_q == 0 | input$All_q%%2 == 0){
        updateCheckboxGroupInput(session,"q",choices=q_list,selected=q_list)
     }else{
        updateCheckboxGroupInput(session,"q",choices=q_list)
     }
  )
  observeEvent(input$All_A,
    if(input$All_A  == 0 | input$All_A%%2 == 0){
       updateCheckboxGroupInput(session,"A",choices=A_list,selected=A_list)
    }else{
       updateCheckboxGroupInput(session,"A",choices=A_list)
    }
  )
  observeEvent(input$All_V,
    if(input$All_V  == 0 | input$All_V%%2 == 0){
       updateCheckboxGroupInput(session,"V",choices=V_list,selected=V_list)
    }else{
       updateCheckboxGroupInput(session,"V",choices=V_list)
    }
  )


  # ---- Management all switches -------------

  observeEvent(input$All_M1,
               if(input$All_M1 == 0 | input$All_M1%%2 == 0){
                 updateCheckboxGroupInput(session,"M1",choices=M1_list,selected=M1_list)
               }else{
                 updateCheckboxGroupInput(session,"M1",choices=M1_list)
               }
  )
  observeEvent(input$All_IB,
               if(input$All_IB == 0 | input$All_IB%%2 == 0){
                 updateCheckboxGroupInput(session,"IB",choices=IB_list,selected=IB_list)
               }else{
                 updateCheckboxGroupInput(session,"IB",choices=IB_list)
               }
  )
  observeEvent(input$All_IV,
               if(input$All_IV == 0 | input$All_IV%%2 == 0){
                 updateCheckboxGroupInput(session,"IV",choices=IV_list,selected=IV_list)
               }else{
                 updateCheckboxGroupInput(session,"IV",choices=IV_list)
               }
  )


  # ---- Data all switches -------------

  observeEvent(input$All_D1,
               if(input$All_D1 == 0 | input$All_D1%%2 == 0){
                 updateCheckboxGroupInput(session,"D1",choices=D1_list,selected=D1_list)
               }else{
                 updateCheckboxGroupInput(session,"D1",choices=D1_list)
               }
  )
  observeEvent(input$All_CB,
               if(input$All_CB == 0 | input$All_CB%%2 == 0){
                 updateCheckboxGroupInput(session,"CB",choices=CB_list,selected=CB_list)
               }else{
                 updateCheckboxGroupInput(session,"CB",choices=CB_list)
               }
  )
  observeEvent(input$All_Beta,
               if(input$All_Beta == 0 | input$All_Beta%%2 == 0){
                 updateCheckboxGroupInput(session,"Beta",choices=Beta_list,selected=Beta_list)
               }else{
                 updateCheckboxGroupInput(session,"Beta",choices=Beta_list)
               }
  )
  observeEvent(input$All_Err,

              updateRadioButtons(session,"Err",choices=Err_list,selected="Err_bad")

  )

  # ======================= Explanatory Plots ===================================
  # Scheme
  fcol = rgb(0.4,0.8,0.95)#"#0299f"
  fcol2 = "dark grey"
  icol <- "dodgerblue4"
  maxcol="cadetblue"
  mincol="dark grey"

  # Fishery
  output$plotM <- renderPlot(plotM())
  output$plotD <- renderPlot(plotD())
  output$ploth <- renderPlot(ploth())
  output$plotFP <- renderPlot(plotFP())
  output$plotF <- renderPlot(plotF())
  output$plotsel <- renderPlot(plotsel())
  output$plotdome <- renderPlot(plotdome())
  output$plotDR <- renderPlot(plotDR())
  output$plotPRM <- renderPlot(plotPRM())
  output$plotsigR <- renderPlot(plotsigR())
  output$plotq <- renderPlot(plotq())
  output$plotA <- renderPlot(plotA())
  output$plotV <- renderPlot(plotV())

  # Management
  output$plotIB <- renderPlot(plotIB())
  output$plotIV <- renderPlot(plotIV())

  # Data
  output$plotBeta <- renderPlot(plotBeta())
  output$plotCB <- renderPlot(plotCB())

})
