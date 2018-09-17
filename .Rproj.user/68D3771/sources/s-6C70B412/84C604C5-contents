
library(shiny)
library(DLMtool)
library(MSEtool)
library(kableExtra)
library(formattable)
library(knitr)
library(dplyr)
library(httpuv)
library(shinyalert)

options(shiny.maxRequestSize=100*1024^2)
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
  source("./Update_objects.R",local=TRUE) # functions that update stored objects, panelstate justification etc
  source('./Fease_Functions.R',local=TRUE )
  source('./SSRA.R',local=TRUE ) # Stochastic SRA wrapper
  source('./StochasticSRA_MSC.R',local=TRUE ) # Stochastic SRA until progress bar update comes to DLMtool
  source('./Backwards.R',local=TRUE ) # Stochastic SRA until progress bar update comes to DLMtool
  #assignInNamespace("SampleCpars",SampleCpars_mod, ns="DLMtool")
  #assignInNamespace("incProgress",shiny::incProgress, ns="DLMtool")
  incProgress<-shiny::incProgress

  # --------------------------------------------------------------

  Fpanel<-reactiveVal(0)
  Mpanel<-reactiveVal(0)
  Dpanel<-reactiveVal(0)
  Started<-reactiveVal(0)
  Quest<-reactiveVal(0)
  Data<-reactiveVal(0)
  CondOM<-reactiveVal(0)
  MadeOM<-reactiveVal(0)
  Calc<-reactiveVal(0) # Have run Evaluation (multi MP)
  App<-reactiveVal(0)  # Have run Application (single MP)
  DataInd<-reactiveVal(0) # Indicator data loaded
  Ind<-reactiveVal(0)  # Have run Indicator (single MP)

  output$Fpanel <- reactive({ Fpanel()})
  output$Mpanel <- reactive({ Mpanel()})
  output$Dpanel <- reactive({ Dpanel()})

  output$Started  <- reactive({ Started()})
  output$Quest    <- reactive({ Quest()})
  output$Data     <- reactive({ Data()})
  output$CondOM   <- reactive({ CondOM()})
  output$MadeOM   <- reactive({ MadeOM()})

  output$Calc     <- reactive({ Calc()})
  output$App      <- reactive({ App()})
  output$DataInd  <- reactive({ DataInd()})
  output$Ind      <- reactive({ Ind()})

  outputOptions(output,"Fpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Mpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Dpanel",suspendWhenHidden=FALSE)

  outputOptions(output,"Started",suspendWhenHidden=FALSE)
  outputOptions(output,"Data",suspendWhenHidden=FALSE)
  outputOptions(output,"Quest",suspendWhenHidden=FALSE)

  outputOptions(output,"CondOM",suspendWhenHidden=FALSE)
  outputOptions(output,"MadeOM",suspendWhenHidden=FALSE)

  outputOptions(output,"Calc",suspendWhenHidden=FALSE)
  outputOptions(output,"App",suspendWhenHidden=FALSE)
  outputOptions(output,"DataInd",suspendWhenHidden=FALSE)
  outputOptions(output,"Ind",suspendWhenHidden=FALSE)

  output$Fpanelout <- renderText({ paste("Fishery",Fpanel(),"/ 14")})
  output$Mpanelout <- renderText({ paste("Management",Mpanel(),"/ 3")})
  output$Dpanelout <- renderText({ paste("Data",Dpanel(),"/ 4")})

  # Some useful things
  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

  CurrentYr<-as.integer(substr(as.character(Sys.time()),1,4))
  Just<-list(c("No introduction / general comments were provided",rep("No justification was provided",13)),rep("No justification was provided",3),rep("No justification was provided",4))
  FRAMEversion<<-"2.2"

  # Default simulation ttributes --------------------------------------------------------------------------------
  nyears<-68 # 1950-2018
  nsim<-48

  makeState<-function(x)rep(T,length(get(x)))

  Fpanel_names<-c("M_list","D_list","h_list","FP_list","F_list","sel_list","dome_list","DR_list","PRM_list","sigR_list","q_list","A_list","V_list")
  Mpanel_names<-c("M1_list","IB_list","IV_list")
  Dpanel_names<-c("D1_list","CB_list","Beta_list","Err_list")

  MasterList<<-list(Fpanel_names,Mpanel_names,Dpanel_names)

  PanelState<<-list(Fpanel=lapply(Fpanel_names, makeState),
                   Mpanel=lapply(Mpanel_names, makeState),
                   Dpanel=lapply(Dpanel_names, makeState))

  PanelState[[3]][[4]]<<-c(F,F,F,T) # Exception is the final selection of the data menu - quality is a radio button default to data-poor

  getinputnames<-function(x)strsplit(x,"_")[[1]][1]

  inputnames<<-list(Fpanel=lapply(Fpanel_names,getinputnames),
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


  # == File I/O ==========================================================================


  # Questionnaire save
  output$Save<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".frame"),

    content=function(file){
      Des<<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)

      MSClog<-list(PanelState, Just, Des)
      doprogress("Saving Questionnaire")
      saveRDS(MSClog,file)

    }

  )


  # Questionnaire load
  observeEvent(input$Load,{

    filey<-input$Load
    tryCatch({
        MSClog<-readRDS(file=filey$datapath)
        cond<-length(MSClog)==3 & sum(names(MSClog[[1]])==c("Fpanel","Mpanel","Dpanel"))==3

        if(cond){
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
        }else{
          shinyalert("File read error", "This does not appear to be a FRAME questionnaire file", type = "error")
        }

      },
      error = function(e){
        shinyalert("File read error", "This does not appear to be a FRAME questionnaire file", type = "error")
        return(0)
      }
    )


  })

  # Data load
  observeEvent(input$Load_Data,{

    filey<-input$Load_Data

    tryCatch(
      {
        dat<<-XL2Data(filey$datapath)
        updateTextAreaInput(session,"Data_Rep",value="Data successfully loaded")
        Data(1)
        shinyjs::enable("OM_Cond")
        MadeOM(0)
        Calc(0)
        App(0)
        Ind(0)
        DataInd(0)
      },
      error = function(e){
        shinyalert("File read error", "Make sure this file is a .csv file of the standard DLMtool 'Data' format", type = "error")
        Data(0)
        shinyjs::disable("OM_Cond")
        updateCheckboxInput(session,"OM_cond",value=FALSE)
      }
    )

  })

  # OM save
  output$Save_OM<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".OM"),

    content=function(file){

      doprogress("Saving Operating Model")
      saveRDS(OM,file)


    }


  )

  # OM load
  observeEvent(input$Load_OM,{

      filey<-input$Load_OM

      tryCatch({
        OM<-readRDS(file=filey$datapath)
      },
      error = function(e){
        shinyalert("File read error", "This does not appear to be a DLMtool OM object, saved by saveRDS()", type = "error")
        return(0)
      }
      )


      if(class(OM)=='OM'){
        MPs<<-getMPs()
        updateSelectInput(session=session,inputId="sel_MP",choices=MPs)
        MadeOM(1)
        CondOM(0)
        Quest(0)
      }else{
        shinyalert("Incorrect class of object", "This file should be an object of DLMtool class 'OM'", type = "error")
      }

  })


  # Eval save
  output$Save_Eval<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".Eval"),

    content=function(file){

      doprogress("Saving Evaluation data")
      saveRDS(list(MSEobj=MSEobj,MSEobj_reb=MSEobj_reb),file)


    }


  )

  # Eval load
  observeEvent(input$Load_Eval,{

    filey<-input$Load_Eval

    tryCatch({
      listy<-readRDS(file=filey$datapath)
    },
    error = function(e){
      shinyalert("File read error", "This does not appear to be a FRAME evaluation object", type = "error")
      return(0)
    }
    )

    cond<-class(listy[[1]])=="MSE" & class(listy[[2]])=="MSE" & listy[[1]]@nMPs>1

    if(cond){
      MSEobj<<-listy[[1]]
      MSEobj_reb<<-listy[[2]]
      Calc(1)
      App(0)
      MadeOM(0)
      CondOM(0)
      DataInd(0)
      Ind(0)
      Quest(0)
      redoEval(fease=T)
      updateTabsetPanel(session,"Res_Tab",selected="1")
    }else{
      shinyalert("File read error", "This does not appear to be a FRAME evaluation object", type = "error")
    }

  })

  # App save
  output$Save_App<- downloadHandler(

    filename = function()paste0(namconv(input$Name),".App"),

    content=function(file){

      doprogress("Saving Application data")
      saveRDS(list(MSEobj_app=MSEobj_app,MSEobj_reb_app=MSEobj_reb_app),file)


    }


  )

  # App load
  observeEvent(input$Load_App,{

    filey<-input$Load_App

    tryCatch({
      listy<-readRDS(file=filey$datapath)
    },
    error = function(e){
      shinyalert("File read error", "This does not appear to be a FRAME Evaluation object", type = "error")
      return(0)
    }
    )

    cond<-class(listy[[1]])=="MSE" & class(listy[[2]])=="MSE" & listy[[1]]@nMPs==1

    if(cond){
      MSEobj_app<<-listy[[1]]
      MSEobj_reb_app<<-listy[[2]]
      App(1)
      Calc(0)
      MadeOM(0)
      CondOM(0)
      DataInd(0)
      Quest(0)
      Ind(0)
      redoApp(fease=T)
      updateTabsetPanel(session,"Res_Tab",selected="2")
    }else{
      shinyalert("File read error", "This does not appear to be a FRAME Application object", type = "error")
    }


  })

  # Indicator Data load

  observeEvent(input$Load_Data_Ind,{

    filey<-input$Load_Data_Ind

    tryCatch(
      {
        dat_ind<<-XL2Data(filey$datapath)#readRDS(file=filey$datapath)

        DataInd(1)
      },
      error = function(e){
        shinyalert("File read error", "Make sure this file is a .csv file of the standard DLMtool 'Data' format", type = "error")
        DataInd(0)

      }
    )


  })

  observeEvent(input$getMPhelp,{

        browseURL(MPurl(input$help_MP))
        #js$browseURL(MPurl(input$help_MP))

  })

  # End of file I/O ===================================================================================


  observeEvent(input$nsim, {
    if(input$nsim<48) shinyjs::disable("Parallel")
    if(input$nsim>47) shinyjs::enable("Parallel")
  })

  observeEvent(input$sel_MP,{
    selectedMP<<-input$sel_MP
  })


  # ======= OM building =============================

  observeEvent(input$Build_OM_2,{

    nsim<<-input$nsim

    if(input$OM_cond & Data()==1){ # Build from SRA

      OM<<-makeOM(PanelState,nsim=nsim,nyears=ncol(dat@Cat),maxage=dat@MaxAge)

      saveRDS(OM,"OM_autosave.rda")
      saveRDS(dat,"Data_autosave.rda")

      updateTextAreaInput(session,"Debug1",value=paste(OM@nyears,ncol(dat@Cat)))
      withProgress(message = "Building OM from Questionnaire inc. conditioning using S-SRA", value = 0, {
        OM<<-SSRA_wrap(OM,dat)
      })
      #GoBackwards_SRA(OM)
      #UpdateQuest()
      Just[[1]][1+c(2,4,5,6,7,10)]<<-"Estimated by Stochastic SRA"
      CondOM(1)
      Fpanel(1)
      Mpanel(1)
      Dpanel(1)
      Data(1)

    }else{ # Build OM from questionnaire only

      doprogress("Building OM from Questionnaire",1)
      OM<<-makeOM(PanelState,nsim=nsim)

    }

    Quest(1)
    MadeOM(1)
    Calc(0)
    App(0)
    Ind(0)
    DataInd(0)

    MPs<<-getMPs()
    updateSelectInput(session=session,inputId="sel_MP",choices=MPs)
    selectedMP<<-MPs[2]

  })


#############################################################################################################################################################################
### MSE functions
#############################################################################################################################################################################

  observeEvent(input$Calculate,{

    Fpanel(1)

    MPs<<-getMPs()

    if(input$Ex_Ref_MPs)MPs<<-MPs[!MPs%in%c("FMSYref","FMSYref75","FMSYref50","NFref")]
    if(input$Demo)MPs<<-c("DCAC","matlenlim","MCD","AvC","curE75","IT10")
    if(input$Data_Rich){
      SCA_4010 <<- make_MP(SCA, HCR40_10)
      SCA_MSY <<- make_MP(SCA, HCR_MSY)
      DDSS_4010 <<- make_MP(DD_SS, HCR40_10)
      DDSS_MSY <<- make_MP(DD_SS, HCR_MSY)
      SPSS_4010 <<- make_MP(SP_SS, HCR40_10)
      SPSS_MSY <<- make_MP(SP_SS, HCR_MSY)
      MPs<-c(MPs,"DDSS_4010","DDSS_MSY","SPSS_4010","SPSS_MSY")
    }

    nsim<<-input$nsim

    parallel=F
    if(input$Parallel){

      if(nsim>47){
        parallel=T
        setup()
      }
    }

    #tags$audio(src = "RunMSE.mp3", type = "audio/mp3", autoplay = NA, controls = NA)


  tryCatch({
    withProgress(message = "Running Evaluation", value = 0, {
      MSEobj<<-runMSE(OM,MPs=MPs,silent=T,control=list(progress=T),PPD=T,parallel=parallel)
      MGT2<-ceiling(MSEobj@OM$MGT*2)
      MGT2[MGT2<5]<-5
      MGT2[MGT2>20]<-20

      OM_reb<-OM
      OM@proyears<-max(MGT2)+2 # only have to compute to this year
      OM_reb@cpars$D<-MSEobj@OM$SSBMSY_SSB0/2#apply(MSEobj@SSB_hist[,,MSEobj@nyears,],1, sum)/(MSEobj@OM$SSB0*2) # start from half BMSY

      withProgress(message = "Rebuilding Analysis", value = 0, {
        MSEobj_reb<<-runMSE(OM_reb,MPs=MPs,silent=T,control=list(progress=T),parallel=parallel)
      })
      save(MSEobj_reb,file="MSEobj_reb")

      save(MSEobj,file="MSEobj")
      save(PanelState,file="PanelState")

      # ==== Types of reporting ==========================================================

      redoEval(fease=T)
      Calc(1)
      updateTabsetPanel(session,"Res_Tab",selected="1")
    }) # with progress

    },
    error = function(e){
      shinyalert("Computational error", "This probably occurred because your simulated conditions are not possible.
                 For example a short lived stock a low stock depletion with recently declining effort.
                 Try revising operating model parameters.", type = "info")
      return(0)
    }
   )

  }) # press calculate


  # ------------------------------------------------------------------------------------------------------------------------------------

  observeEvent(input$Calculate_app,{

    Fpanel(1)

    selectedMP<<-input$sel_MP

    nsim<<-input$nsim_app
    parallel=F
    if(input$Parallel){

      if(nsim>47){
        parallel=T
        setup()
      }
    }

    tryCatch({
        withProgress(message = "Running Application", value = 0, {
          MSEobj_app<<-runMSE(OM,MPs=selectedMP,silent=T,control=list(progress=T),PPD=T,parallel=parallel)
        })

        MGT2<-ceiling(MSEobj_app@OM$MGT*2)
        MGT2[MGT2<5]<-5
        MGT2[MGT2>20]<-20

        OM_reb<-OM
        OM@proyears<-max(MGT2)+2 # only have to compute to this year
        OM_reb@cpars$D<-MSEobj_app@OM$SSBMSY_SSB0/2#apply(MSEobj@SSB_hist[,,MSEobj@nyears,],1, sum)/(MSEobj@OM$SSB0*2) # start from half BMSY

        withProgress(message = "Rebuilding Analysis", value = 0, {
          MSEobj_reb_app<<-runMSE(OM_reb,MPs=selectedMP,silent=T,control=list(progress=T),parallel=parallel)
        })
        save(MSEobj_reb_app,file="MSEobj_reb_app")

        save(MSEobj_app,file="MSEobj_app")
        App(1)
        redoApp(fease=T)
        updateTabsetPanel(session,"Res_Tab",selected="2")
      },
      error = function(e){
        shinyalert("Computational error", "This probably occurred because your simulated conditions are not possible.
                   For example a short lived stock a low stock depletion with recently declining effort.
                   Try revising operating model parameters.", type = "info")
        return(0)
      }
    ) # try catch

  }) # calculate MSE app

  observeEvent(input$Calculate_Ind,{

    redoInd()
    updateTabsetPanel(session,"Res_Tab",selected="3")
    Ind(1)

  })

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
      output$PI111_uncertain<-renderPlot(MSC_uncertain(MSEobj_top,MPcols=MPcols,maxMPs=MSEobj_top@nMPs, LTL=F,inc_thresh = F,burnin=burnin),height =ceiling(nMPs/6)*250 , width = 1100)
      incProgress(incrate)
      VOIout<<-getVOI(MSEobj_top)
      output$CCU<-renderPlot(CCU_plot(VOIout,MSEobj_top,MPcols=MPcols),height=ceiling(nMPs/3)*290,width=1300)
    })
  }

  redoApp<-function(fease=F){
    withProgress(message = "Calculating Application results", value = 0, {
      incrate<-1/5
      incProgress(incrate)

      burnin<<-input$burnin
      Ptab1_app<<-Ptab(MSEobj_app,MSEobj_reb_app,burnin=burnin,rnd=0)
      thresh<<-c(input$P111a,input$P111b,input$P112,input$P121a,input$P121b)
      temp<-Ptab_ord(Ptab1_app,burnin=burnin,ntop=input$ntop, Eval=F,fease=fease,thresh=thresh)
      incProgress(incrate)

      Ptab2_app<<-temp[[1]]
      MPcols_app<<-temp[[2]]
      output$App_Ptable <- function()Ptab_formatted(Ptab2_app,burnin=burnin,cols=MPcols_app,thresh=thresh)
      output$App_threshtable<-function()Thresh_tab(thresh)
      incProgress(incrate)

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
    PPD<-MSEobj_app@Misc[[1]]

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


  observeEvent(input$Default_thres,{
    updateSliderInput(session,"P111a",value=70)
    updateSliderInput(session,"P111b",value=50)
    updateSliderInput(session,"P112",value=70)
    updateSliderInput(session,"P121a",value=80)
    updateSliderInput(session,"P121b",value=50)

  })





  CheckJust<-function(){

    isjust<-function(x)sum(x=="No justification was provided")

    as.integer(sum(unlist(lapply(Just,isjust)))==0)

  }


  # Update tables if...
  observeEvent(input$burnin,{
    if(Calc()==1){
      redoEval(fease=T)
    }
    if(App()==1){
      redoApp(fease=T)
    }
  })

  observeEvent(input$ntop,{
    if(Calc()==1){
      redoEval(fease=T)
    }
    if(App()==1){
      redoApp(fease=T)
    }
  })

  observeEvent(input$P111a,{
    if(Calc()==1){
      redoEval(fease=T)
    }
    if(App()==1){
      redoApp(fease=T)
    }
  })
  observeEvent(input$P111b,{
    if(Calc()==1){
      redoEval(fease=T)
    }
    if(App()==1){
      redoApp(fease=T)
    }
  })
  observeEvent(input$P112,{
    if(Calc()==1){
      redoEval(fease=T)
    }
    if(App()==1){
      redoApp(fease=T)
    }
  })
  observeEvent(input$P121a,{
    if(Calc()==1){
      redoEval(fease=T)
    }
    if(App()==1){
      redoApp(fease=T)
    }
  })
  observeEvent(input$P121b,{
    if(Calc()==1){
      redoEval(fease=T)
    }
    if(App()==1){
      redoApp(fease=T)
    }
  })

  observeEvent(input$D1,{
    if(Calc()==1){
      UpPanelState()
      redoEval(fease=T)
    }
    if(App()==1){
      UpPanelState()
      redoApp(fease=T)
    }
  })

  observeEvent(input$M1,{
    if(Calc()!=0){
      UpPanelState()
      redoEval(fease=T)
    }
    if(App()==1){
      UpPanelState()
      redoApp(fease=T)
    }
  })


  # ===== Reports ==================================================================================================

  # OM questionnaire report
  output$Build_OM <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    #updateTextInput(session, "Name", value = input$Name),
    filename =  function(){  paste0(namconv(input$Name),"_OM.html") },
    content = function(file) {
      withProgress(message = "Building questionnaire report", value = 0, {
      #doprogress("Building OM report",1)
      OM<<-makeOM(PanelState,nsim=nsim)
      src <- normalizePath('OMRep.Rmd')

      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'OMRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Questionnaire Report for ",input$Name),
                     set_type=paste0("(FRAME version ",FRAMEversion,")"),
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
      }) # end of progress meter
    }
  )

  # Data report
  output$Build_Data <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_data.html")}, #"report.html",

    content = function(file) {
      withProgress(message = "Building data report", value = 0, {
      nsim<<-input$nsim
      OM<<-makeOM(PanelState,nsim=nsim)
      src <- normalizePath('DataRep.Rmd')

      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'DataRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Data report for ",input$Name),
                     set_type=paste0("Demonstration Data description"," (FRAME version ",FRAMEversion,")"),
                     dat=dat,
                     author=input$Author,
                     ntop=input$ntop,
                     inputnames=inputnames,
                     SessionID=SessionID,
                     copyright="copyright (c) NRDC 2018"
      )
      incProgress(0.2)
      output<-render(input="DataRep.Rmd",output_format="html_document", params = params)
      incProgress(0.7)
      file.copy(output, file)
      incProgress(0.1)
      }) # end of progress
    }
  )

  # Conditioning report
  output$Build_Cond <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_Cond.html")}, #"report.html",

    content = function(file) {
      withProgress(message = "Building conditioning report", value = 0, {

      incProgress(0.1)
      #OM<<-makeOM(PanelState,nsim=nsim)
      src <- normalizePath('CondRep.Rmd')

      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'CondRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Operating Model Conditioning Report for ",input$Name),
                     set_type=paste0("Demonstration Conditioning analysis"," (FRAME version ",FRAMEversion,")"),
                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     ntop=input$ntop,
                     inputnames=inputnames,
                     SessionID=SessionID,
                     copyright="copyright (c) NRDC 2018"
      )
      incProgress(0.1)
      output<-render(input="CondRep.Rmd",output_format="html_document", params = params)
      incProgress(0.8)
      file.copy(output, file)
      }) # end of progress
    }
  )

  # Full OM report
  output$Build_full_OM <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_full_OM.html")}, #"report.html",

    content = function(file) {
      withProgress(message = "Building conditioning report", value = 0, {
      OM<<-makeOM(PanelState,nsim=nsim)
      src <- normalizePath('OM_full_Rep.Rmd')
      incProgress(0.1)
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'OM_full_Rep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Full Operating Model Specification Report for ",input$Name),
                     set_type=paste0("(FRAME version ",FRAMEversion,")"),
                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     ntop=input$ntop,
                     inputnames=inputnames,
                     SessionID=SessionID,
                     copyright="copyright (c) NRDC 2018"
      )
      incProgress(0.1)
      output<-render(input="OM_full_Rep.Rmd",output_format="html_document", params = params)
      incProgress(0.8)
      file.copy(output, file)
      })
    }
  )

  # Evaluation MSE report
  output$Build_Eval <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_Eval.html")}, #"report.html",

    content = function(file) {
      withProgress(message = "Building evaluation report", value = 0, {
      src <- normalizePath('EvalRep.Rmd')
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'EvalRep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      thresh<-c(input$P111a,input$P111b,input$P112,input$P121a,input$P121b)
      Ptab1<<-Ptab(MSEobj,MSEobj_reb,burnin=burnin,rnd=0)
      thresh<<-c(input$P111a,input$P111b,input$P112,input$P121a,input$P121b)
      temp<-Ptab_ord(Ptab1,burnin=burnin,ntop=input$ntop,fease=fease,thresh=thresh)
      Ptab2<<-temp[[1]]
      MPcols<<-temp[[2]]

      params <- list(test = input$Name,
                     set_title=paste0("Evaluation Report for ",input$Name),
                     set_type=paste0("Evaluation of MPs analysis "," (FRAME version ",FRAMEversion,")"),

                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     MSEobj=MSEobj,
                     MSEobj_reb=MSEobj_reb,
                     MSEobj_top=MSEobj_top,
                     MSEobj_reb_top=MSEobj_reb_top,
                     Ptab2=Ptab2,
                     MPcols=MPcols,
                     ntop=input$ntop,
                     burnin=burnin,
                     thresh=thresh,
                     SessionID=SessionID,
                     copyright="copyright (c) NRDC 2018"
                     )

      out<-render("EvalRep.Rmd", params = params)
      file.rename(out, file)
      })
    }

  )

  # Application MSE report
  output$Build_App <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_App.html")}, #"report.html",

    content = function(file) {
      withProgress(message = "Building application report", value = 0, {

      src <- normalizePath('AppRep.Rmd')
      #doprogress("Building evaluation report",1)
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'AppRep.Rmd', overwrite = TRUE)

      burnin<<-input$burnin
      Ptab1_app<<-Ptab(MSEobj_app,MSEobj_reb_app,burnin=burnin,rnd=0)
      thresh<<-c(input$P111a,input$P111b,input$P112,input$P121a,input$P121b)
      temp<-Ptab_ord(Ptab1_app,burnin=burnin,ntop=input$ntop, Eval=F,fease=fease,thresh=thresh)
      Ptab2_app<<-temp[[1]]
      MPcols_app<<-temp[[2]]


      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Application Report for ",input$Name),
                     set_type=paste0("Application of an MP"," (FRAME version ",FRAMEversion,")"),

                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     MSEobj_app=MSEobj_app,
                     MSEobj_reb_app=MSEobj_reb_app,
                     MPcols=MPcols_app,
                     Ptab2_app=Ptab2_app,
                     thresh=thresh,
                     ntop=input$ntop,
                     burnin=burnin,
                     SessionID=SessionID,
                     copyright="copyright (c) NRDC 2018"
      )

      out<-render("AppRep.Rmd", params = params)
      file.rename(out, file)
      })
    }

  )

  # Anciliary indicators report
  output$Build_AI <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(namconv(input$Name),"_AI.html")}, #"report.html",
    content = function(file) {
      withProgress(message = "Building indicators report", value = 0, {
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
                     set_type=paste0("Ancillary indicators report"," (FRAME version ",FRAMEversion,")"),

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
      })
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
    #selectedMP<<-MPs[2]

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



  observeEvent(input$debug,

               updateTextAreaInput(session,"Debug1",value=MadeOM())
  )

})
