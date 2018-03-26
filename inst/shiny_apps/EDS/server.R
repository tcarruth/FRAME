
library(shiny)
library(DLMtool)
library(kableExtra)
library(formattable)
library(knitr)
library(dplyr)

source("./global.R")

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {

  Fpanel<-reactiveVal(0)
  Mpanel<-reactiveVal(0)
  Dpanel<-reactiveVal(0)

  output$Fpanel <- reactive({ Fpanel()})
  output$Mpanel <- reactive({ Mpanel()})
  output$Dpanel <- reactive({ Dpanel()})

  #vals <- reactiveValues(Fpanel = 0,Mpanel=0,Dpanel=0)
  outputOptions(output,"Fpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Mpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Dpanel",suspendWhenHidden=FALSE)

  output$Fpanelout <- renderText({ paste("Fishery",Fpanel(),"/ 14")})
  output$Mpanelout <- renderText({ paste("Management",Mpanel(),"/ 3")})
  output$Dpanelout <- renderText({ paste("Data",Dpanel(),"/ 4")})

  # Some useful things
  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  output$SessionID<-renderText(paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2]))
  CurrentYr<-as.integer(substr(as.character(Sys.time()),1,4))
  Just<-list(c("No introduction / general comments were provided",rep("No justification was provided",13)),rep("No justification was provided",3),rep("No justification was provided",4))

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

  MSCsave_auto<-function(){

    MSClog<-list(PanelState, Just, Des)
    saveRDS(MSClog,file=paste0(USERID,"_autosave.msc"))

  }

  # == File I/O =================

  doprogress<-function(message,duration=1,n=20){
    withProgress(message = message, value = 0, {
      inc<-duration/n
      for (i in 1:n) {
        incProgress(1/n, detail = round(i*(100/n)))
        Sys.sleep(inc)
      }
    })
  }

  output$Save<- downloadHandler(

    filename = paste0(namconv(input$Name),".msc"), #paste0(getwd(),"/",namconv(input$Name),".msc"),

    content=function(file){

      updateTextAreaInput(session,"Debug1",value=file)
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)
      doprogress("Saving")
      saveRDS(MSClog,file)

    }

  )

  observeEvent(input$Analysis_type,{

    if(input$Analysis_type=='Demo'){
      updateNumericInput(session,'nsim',value="16")
    }else{
      updateNumericInput(session,'nsim',value="96")
    }

    if(input$Analysis_type=='RE'){
      updateNumericInput(session,'proyears',value="100")
    }else{
      updateNumericInput(session,'proyears',value="50")
    }

  })

  namconv<-function(nam){
    nam<-gsub(" ","_",nam)
    nam<-gsub("[.]","",nam)
    nam<-gsub(",","",nam)
    substr(nam,1,15)[[1]]
  }


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
    updateTextInput(session, "Species", value= MSClog[[3]]$Species)
    updateTextInput(session, "Region", value= MSClog[[3]]$Region)
    updateTextInput(session, "Agency", value=MSClog[[3]]$Agency)
    updateTextInput(session, "nyears",   value= MSClog[[3]]$nyears)
    updateTextInput(session, "Author",   value= MSClog[[3]]$Author)

    #=== DEBUGGING WINDOW =====================================================
    #updateTextAreaInput(session,"Debug",value=choices)
    #updateTextAreaInput(session,"Debug2",value=selected)
    #updateTextAreaInput(session,"Debug3",value=inputId)
    # ==========================================================================

    Fpanel(1)
    Mpanel(1)
    Dpanel(1)

    doprogress("Loading")

  })

  getminmax<-function(panel,parameter,PanelState){
    loc<-match(parameter,inputnames[[panel]])
    mins<-get(paste0(parameter,"_mins"))
    maxs<-get(paste0(parameter,"_maxes"))
    cond<-unlist(PanelState[[panel]][loc])
    range(mins[cond],maxs[cond])
  }

  makeOM<-function(PanelState,nsim=48){

    OM<-testOM
    OM@nsim<-nsim

    OM@Linf<-c(3,3.5)
    OM@L50<-c(1.5,1.5)
    OM@L50_95<-c(0.2,0.2)
    OM@isRel<-"FALSE"

    OM@Name<-input$Name
    OM@Species<-input$Species
    OM@Region<-input$Region
    OM@Agency<-input$Agency
    if(is.na(as.integer(input$nyears))){
      OM@nyears<-68
    }else{
      OM@nyears<-as.integer(input$nyears)
    }
    nyears<-OM@nyears

    OM@Source<-input$Author
    OM@interval<-input$interval
    OM@proyears<-input$proyears

    #save(OM,file="OM.Rdata")  # debug

    loc<-match("Err",inputnames[[3]])
    cond<-as.vector(unlist(PanelState[[3]][loc]))
    Dquality<-as.vector(unlist(Err_list)[cond])


    if(Dquality=="Err_perf"){
      temp<-new('OM',Albacore,Generic_Fleet,Perfect_Info,Perfect_Imp)
    }else if(Dquality=="Err_good"){
      temp<-new('OM',Albacore,Generic_Fleet,Precise_Unbiased,Perfect_Imp)
    }else if(Dquality=="Err_mod"){
      temp<-new('OM',Albacore,Generic_Fleet,Generic_obs,Perfect_Imp)
    }else{
      temp<-new('OM',Albacore,Generic_Fleet,Imprecise_Biased,Perfect_Imp)
    }
    OM<-Replace(OM,temp,Sub="Obs")

    # Fishery characteristics -------
    OM@M<-getminmax(1,"M",PanelState)
    OM@D<-getminmax(1,"D",PanelState)
    OM@h<-getminmax(1,"h",PanelState)

    # Ftrend and error
    loc<-match("FP",inputnames[[1]])
    cond<-(1:length(unlist(PanelState[[1]][loc])))[unlist(PanelState[[1]][loc])]
    Ftype<<-sample(cond,nsim,replace=T)
    M1sim<-M1s[Ftype]
    M2sim<-M2s[Ftype]
    sd1sim<-sd1s[Ftype]
    sd2sim<-sd2s[Ftype]
    h2sim<-h2s[Ftype]
    Find<-array(NA,c(nsim,nyears))
    for(i in 1:nsim)Find[i,]<-Ftrendfunc(M1sim[i],M2sim[i],sd1sim[i],sd2sim[i],h2sim[i],nyears)
    Esd<-getminmax(1,"F",PanelState)
    Esd_max<-Esd[2]
    Esd_min<-Esd[1]
    Esdrand<-runif(nsim,Esd_min,Esd_max)
    Emu<-(-0.5*Esdrand^2)
    Esdarray<-array(exp(rnorm(nsim*nyears,Emu,Esdrand)),c(nsim,nyears))
    Find<-Find*Esdarray

    # --- Selectivity -----------------------
    Sel50<-getminmax(1,"sel",PanelState)
    Sel50sim<-runif(nsim,Sel50[1],Sel50[2])
    L5<-Sel50sim*0.8
    LFS<-Sel50sim*1.2

    OM@Vmaxlen<-getminmax(1,"dome",PanelState)
    OM@DR<-getminmax(1,"DR",PanelState)
    OM@Fdisc<-getminmax(1,"PRM",PanelState)
    OM@Perr<-getminmax(1,"sigR",PanelState)
    OM@qinc<-getminmax(1,"q",PanelState)
    Arng<-getminmax(1,"A",PanelState)
    Size_area_1<-Frac_area_1<-runif(nsim,Arng[1],Arng[2])
    OM@Prob_staying<-1-getminmax(1,"V",PanelState)[2:1]

    # Management parameters
    OM@TACFrac<-OM@TAEFrac<-getminmax(2,"IB",PanelState)
    OM@TACSD<-OM@TAESD<-getminmax(2,"IV",PanelState)

    # Data parameters
    CB_rng<-getminmax(3,"CB",PanelState)
    Cbias<-runif(nsim,CB_rng[1],CB_rng[2])

    OM@beta<-getminmax(3,"Beta",PanelState)

    # Custom parameters
    OM@cpars<-list(Find=Find, L5=L5, LFS = LFS,Size_area_1=Size_area_1,
                   Frac_area_1=Frac_area_1,Cbias=Cbias)

    saveRDS(OM,"OM_autosave.rda")
    OM

  }

  Ptab<-function(MSEobj,MSEobj_FB,Eyr=10,rnd=0,Pcrit=0.1){

    # PI 1.1.1
    PI.111.a<-round(apply(MSEobj@B_BMSY[,,1:Eyr]>0.5,2,mean)*100,rnd)
    PI.111.b<-round(apply(MSEobj@B_BMSY[,,1:Eyr]>1,2,mean)*100,rnd)

    # PI 1.1.2
    #MGT<-((-log(1-MSEobj@OM$L50/MSEobj@OM$Linf))/MSEobj@OM$K)+MSEobj@OM$t0
    #2*MGT
    MGT2<-ceiling(MSEobj@OM$MGT*2)
    MGT2[MGT2<3]=3
    Bind<-cbind(as.matrix(expand.grid(1:MSEobj@nsim,1:MSEobj@nMPs)),rep(MGT2,MSEobj@nMPs))
    Bmat<-array(MSEobj@B_BMSY[Bind],c(MSEobj@nsim,MSEobj@nMPs))
    PI.112<-round(apply(Bmat>1,2,mean)*100,rnd)

    # PI 1.2.1
    PI.121.a<-round(apply(MSEobj@B_BMSY[,,11:50]>0.5,2,mean)*100,rnd)
    PI.121.b<-round(apply(MSEobj@B_BMSY[,,11:50]>1,2,mean)*100,rnd)

    # PI 1.2.2
    PI.122.a<-rep(0,MSEobj@nMPs)

    for(i in 1:MSEobj@nMPs){

      cond<-MSEobj_FB@B_BMSY[,i,1]<1 & MSEobj_FB@B_BMSY[,i,1]>0.1
      dat<-data.frame(x=MSEobj_FB@B_BMSY[cond,i,1],y=MSEobj_FB@F_FMSY[cond,i,2])
      temp<-lm(y~x,dat=dat)
      stemp<-summary(temp)
      Pval<-stemp$coefficients[2,4]
      Slope<-stemp$coefficient[2,1]
      pos<-Slope>0

      if(pos&Pval<Pcrit)PI.122.a[i]<-1

    }

    PI.122.b<-round(apply(MSEobj@F_FMSY[,,1:50]>0.5 &  MSEobj@F_FMSY[,,1:50]<1.5,2,mean)*100,rnd)

    # LTY
    refY<-sum(MSEobj@C[,1,11:50])
    LTY<-round(apply(MSEobj@C[,,11:50],2,sum)/refY*100,rnd)
    MP<-MSEobj@MPs

    tab<-data.frame(MP,PI.111.a, PI.111.b, PI.112, PI.121.a, PI.121.b,
                    PI.122.a, PI.122.b, LTY)

    tab<-tab[order(tab$LTY,decreasing=T),]
    tab

  }


  #                                       11a 11b 12  21a 21a 21b 22  LTY
  Ptab_formatted<-function(Ptab1,thresh=c(80, 50, 50, 80, 50, 1,  60, 50)){

    # save(Ptab1,file="Ptab1")
    MPs<-as.character(Ptab1$MP)

    # Data Feasibility
    cond<-unlist(PanelState[[3]][1]) # cond=rep(T,9)
    FeasePos<-c("Catch","Catch","Index","Index","Index","Catch_at_length","Catch_at_age","Growth","Abundance")
    pot_slots<-unique(FeasePos)
    got_slots<-unique(FeasePos[cond])
    not_slots<-pot_slots[!pot_slots%in%got_slots]
    tempFease<-new('Fease')
    if(length(not_slots)>0)for(i in 1:length(not_slots))slot(tempFease,not_slots[i])<-0
    DFeasible<-Fease(tempFease,1)
    # MPs%in%DFeasible

    # TAC TAE Feasibility
    cond<-unlist(PanelState[[2]][1]) # cond=rep(T,4)
    Data <- DLMtool::SimulatedData
    runMPs <- applyMP(Data, MPs, reps = 2, nsims=1, silent=TRUE)
    recs <- runMPs[[1]]
    type <- matrix(0, nrow=length(MPs),ncol=4) # TAC TAE SL MPA
    for (mm in seq_along(recs)) {
      type[mm,1] <- as.integer(length(recs[[mm]]$TAC) > 0)
      type[mm,2] <- as.integer(length(recs[[mm]]$Effort)>0)
      type[mm,3] <- as.integer(length(recs[[mm]]$LR5)>0)
      type[mm,4] <- as.integer(!is.na(recs[[mm]]$Spatial[1,1]))
    }

    DFeasible<-unique(c(DFeasible,MPs[(type[,4]==1|type[,3]==1) & apply(type,1,sum)==1])) # Size limits and area closures might not need data
    totneeded<-apply(type,1,sum)
    speced<-matrix(rep(as.integer(cond),each=length(MPs)),nrow=length(MPs))
    MFeasible<-MPs[apply(speced*type,1,sum)==totneeded]

    Ptab2<-Ptab1#[,1:ncol(Ptab1)]
    names(Ptab2)<-c("MP","PI111a","PI111b","PI112","PI121a","PI121b","PI122a","PI122b","LTY")

    PIsmet<-Ptab2$PI111a >= thresh[1] & Ptab2$PI111b >= thresh[2] & Ptab2$PI112 >= thresh[3] & Ptab2$PI121a >= thresh[4] & Ptab2$PI121b >= thresh[5] & Ptab2$PI122a >= thresh[6] & Ptab2$PI122b >= thresh[7]
    MPcols<-rep('black',length(MPs))
    MPcols[MPs%in%MFeasible & MPs%in%DFeasible & PIsmet]<-'green'
    MPcols[MPs%in%MFeasible & MPs%in%DFeasible & !PIsmet]<-'red'

    available<-rep("",length(MPs))
    condD<-!MPs%in%DFeasible
    condM<-!MPs%in%MFeasible
    condDM<-condD&condM
    available[condD]<-"D"
    available[condM]<-"M"
    available[condDM]<-"D/M"

    Ptab2<-cbind(Ptab2,available)

    # Rankings
    rnkscore<-Ptab2$LTY
    rnkscore[MPcols=="green"]=rnkscore[MPcols=="green"]+2000
    rnkscore[MPcols=="red"]=rnkscore[MPcols=="red"]+1000
    ord<-order(rnkscore,decreasing = T)
    Ptab2<-Ptab2[ord,]
    MPcols<-MPcols[ord]

    Ptab2 %>%
      mutate(
        #MP = row.names(.),
        MP =  cell_spec(MP, "html", color = MPcols, bold = T),
        PI111a = ifelse(PI111a >= thresh[1],
                        cell_spec(PI111a, "html", color = "green", bold = T),
                        cell_spec(PI111a, "html", color = "red", italic = T)),
        PI111b = ifelse(PI111b >= thresh[2],
                        cell_spec(PI111b, "html", color = "green", bold = T),
                        cell_spec(PI111b, "html", color = "red", italic = T)),
        PI112 = ifelse(PI112 >= thresh[3],
                       cell_spec(PI112, "html", color = "green", bold = T),
                       cell_spec(PI112, "html", color = "red", italic = T)),
        PI121a = ifelse(PI121a >= thresh[4],
                        cell_spec(PI121a, "html", color = "green", bold = T),
                        cell_spec(PI121a, "html", color = "red", italic = T)),
        PI121b = ifelse(PI121b >= thresh[5],
                        cell_spec(PI121b, "html", color = "green", bold = T),
                        cell_spec(PI121b, "html", color = "red", italic = T)),
        PI122a = ifelse(PI122a >= thresh[6],
                        cell_spec(PI122a, "html", color = "green", bold = T),
                        cell_spec(PI122a, "html", color = "red", italic = T)),
        PI122b = ifelse(PI122b >= thresh[7],
                        cell_spec(PI122b, "html", color = "green", bold = T),
                        cell_spec(PI122b, "html", color = "red", italic = T)),
        LTY = ifelse(LTY >= thresh[8],
                     cell_spec(LTY, "html", color = "green", bold = T),
                     cell_spec(LTY, "html", color = "red", italic = T)),
        available =  cell_spec(available, "html", bold = T)

      )%>%
      #select(everything())%>%
      knitr::kable("html", escape = F,align = "c") %>%
      kable_styling("striped", full_width = F)%>%
      column_spec(5, width = "3cm") %>%
      add_header_above(c(" ", "> 0.5 BMSY" = 1, "> BMSY" = 1,
                         "> BMSY"=1,"> 0.5 BMSY"=1,"> BMSY"=1,"P/F"=1,
                         "0.5 - 1.5 FMSY"=1,"vs FMSYref"=1,"not"=1))%>%

      add_header_above(c(" ", "Biomass (yrs 1-10)" = 2, "Biomass (2 MGT)" = 1,
                         "Biomass (yrs 11-50)"=2,"F decrease w B"=1,
                         "Fishing Mortality (yrs 1-50)"=1,
                         "yrs 11-50"=1,"Reason"=1))%>%

      add_header_above(c(" ", "Stock Status" = 2, "Rebuilding" = 1,
                         "Harvest Strategy"=2,"HCR & Tools"=2,
                         "Long-Term Yield"=1," "=1))

  }

  P1_LTY_plot<<-function(MSEobj){

    Eyr<-10
    rnd<-0

    PI.111.a<-round(apply(MSEobj@B_BMSY[,,1:Eyr]>0.5,2,mean)*100,rnd)
    refY<-sum(MSEobj@C[,1,11:50])
    LTY<-round(apply(MSEobj@C[,,11:50],2,sum)/refY*100,rnd)
    MP<-MSEobj@MPs
    par(mai=c(0.8,0.8,0.1,0.1))
    ylim<-c(0,max(LTY))
    plot(c(-10,110),ylim,col='white',xlab="",ylab="")
    mtext("PI.1.1.1a",1,line=2.5,cex=1.2)
    mtext("Long term yield",2,line=2.5,cex=1.2)
    abline(v=c(0,100),col="#99999950")
    abline(h=c(0,100),col="#99999950")

    text(PI.111.a,LTY,MSEobj@MPs,col=icol,cex=1.2)

  }

  wormplot_msc<-function(MSEobj){

    par(mai=c(0.6,0.6,0.01,0.01))
    wormplot(MSEobj)

  }

  HCRplot<-function(MSEobj,Pcrit=0.2){

    nMPs<-MSEobj@nMPs
    ncol=ceiling(nMPs*0.5)
    nrow=ceiling(nMPs/ncol)
    par(mfrow=c(nrow,ncol),mai=c(0.4,0.4,0.01,0.01),omi=c(0.3,0.3,0.01,0.01))

    for(i in 1:MSEobj@nMPs){

      cond<-MSEobj_FB@B_BMSY[,i,1]<1 & MSEobj_FB@B_BMSY[,i,1]>0.1
      plot(MSEobj_FB@B_BMSY[,i,1],MSEobj_FB@F_FMSY[,i,2],col="white",pch=19,xlim=c(0,1.6),xlab="",ylab="",cex.axis=1.5)
      points(MSEobj_FB@B_BMSY[!cond,i,1],MSEobj_FB@F_FMSY[!cond,i,2],col="#99999998",pch=19)

      dat<-data.frame(x=MSEobj_FB@B_BMSY[cond,i,1],y=MSEobj_FB@F_FMSY[cond,i,2])
      temp<-lm(y~x,dat=dat)
      stemp<-summary(temp)
      pred<-predict(temp,newdata=data.frame(x=seq(0.1,1,length.out=20)))
      Pval<-stemp$coefficients[2,4]
      Slope<-stemp$coefficient[2,1]
      pos<-Slope>0

      col<-'red'
      if(pos & Pval<Pcrit)col="green"
      points(MSEobj_FB@B_BMSY[cond,i,1],MSEobj_FB@F_FMSY[cond,i,2],col=col,pch=19)
      lines(seq(0.1,1,length.out=20),pred,col=col)
      legend('bottomright',legend=c(paste("pval=",round(Pval,3)),paste("slope=",round(Slope,3))),bty='n',cex=1.3)
      legend('top',MSEobj@MPs[i],bty='n',text.font=2,cex=1.5)

    }

    mtext("F/FMSY",2,line=0.3,outer=T)
    mtext("B/BMSY",1,line=0.3,outer=T)

  }

  getVOI<-function(MSEobj){

    opt1<-  c("M",        "Depletion",      "hs",      "Esd",          "LFS",        "Vmaxlen",  "DR",          "PRM",             "procsd",   "qinc",            "Frac_area_1","Prob_staying",
              "TACFrac", "TACSD","Cbias","betas")

    opt2<-  c(rep("",12),"TAEFrac", "TAESD","","","RefY")

    MSEtemp<-MSEobj
    MSEtemp@OM<-cbind(MSEtemp@OM,betas=MSEtemp@Obs$betas,Cbias=MSEtemp@Obs$Cbias)
    MSEtemp@OM<-MSEtemp@OM[,names(MSEtemp@OM)%in%opt1 | names(MSEtemp@OM)%in%opt2]
    VOI(MSEtemp,ncomp=17,nbins=6)[[1]]

  }

  CCU_plot<-function(VOIout){

    qno<-   c("F2",       "F3",             "F4",       "F6",         "F7",       "F8",          "F9",              "F10",      "F11",             "F12",        "F13",          "F14",
              "M2",       "M3",  "D2",    "D3")
    qtext<- c("Longevity","Stock depletion","Resilence","Exploit. Var.","Selectivity","Dome Sel.","Discard rate","Post. Rel. Mort.","Rec. Var.","Fish. efficiency","MPA size",   "Mixing",
              "Imp. over/under",  "Imp. Var.", "Cat. Rep. Bias.","Hyperstability")
    opt1<-  c("M",        "Depletion",      "hs",      "Esd",          "LFS",        "Vmaxlen",  "DR",          "PRM",             "procsd",   "qinc",            "Frac_area_1","Prob_staying",
              "TACFrac", "TACSD","Cbias","betas")
    opt2<-  c(rep("",12),"TAEFrac", "TAESD","","","RefY")

    nMPs<-MSEobj@nMPs
    ncol=3
    nrow=ceiling(nMPs/ncol)
    par(mfrow=c(nrow,ncol),mai=c(1.8,0.4,0.01,0.01),omi=c(0.3,0.3,0.05,0.01))

    for(i in 1:MSEobj@nMPs){
      MP<-MSEobj@MPs[i]
      dat<-VOIout[match(MP,VOIout[,1])+0:1,2:18]
      lab1<-qno[match(as.factor(unlist(dat[1,])),opt1)]
      lab2<-qno[match(as.factor(unlist(dat[1,])),opt2)]
      lab1[is.na(lab1)]<-lab2[is.na(lab1)]
      dat2<-aggregate(as.numeric(as.character(unlist(dat[2,]))),by=list(lab1),max)
      dat2<-dat2[order(dat2$x,decreasing=T),]
      labs<-paste(qno,qtext,sep=" - ")
      barplot(dat2[,2],names.arg=labs[match(dat2[,1],qno)], las=2,col=fcol,border=NA,cex.axis=1.4,cex.names=1.3)
      legend('topright',MP,bty='n',text.font=2,cex=1.6)
    }

    mtext("Question / operating model characteristic",1,outer=T,line=0.5)
    mtext("Variability in Long Term Yield (% LTY)",2,outer=T,line=0.5)

  }

  observeEvent(input$Calculate,{

    OM<<-makeOM(PanelState,nsim=nsim)

    if(input$Analysis_type=="Demo"){
      #MPs<<-c('FMSYref','AvC','DCAC','curE','matlenlim','MRreal','MCD','MCD4010','DD4010')
      MPs<-c('FMSYref','DBSRA')#,'DCAC','curE','matlenlim')
      nsim<-32
    }else{
      MPs<<-avail('MP')
      cond<-grepl("MLL",MPs)|grepl('ML',MPs)|grepl('FMSYref',MPs)
      if(!input$Ref_MPs)cond<-cond|grepl('curE',MPs)|grepl('NFref',MPs)
      MPs<-c('FMSYref',MPs[!cond])
      nsim<-48
    }

    #tags$audio(src = "RunMSE.mp3", type = "audio/mp3", autoplay = NA, controls = NA)


    withProgress(message = "Running MSE", value = 0, {
      MSEobj<<-runMSE(OM,MPs=MPs,silent=T,control=list(progress=T),PPD=T)
    })

    OM_FB<-OM
    OM_FB@cpars$D<-seq(0.05,0.4,length.out=OM@nsim)
    OM_FB@proyears<-2
    OM_FB@interval<-1
    temp<-new('OM',Albacore,Generic_Fleet,Perfect_Info,Perfect_Imp)
    OM_FB<-Replace(OM_FB,temp,Sub="Obs")
    OM_FB<-Replace(OM_FB,temp,Sub="Imp")

    withProgress(message = "HCR evaluation", value = 0, {
      MSEobj_FB<<-runMSE(OM_FB,MPs=MPs,silent=T,control=list(progress=T))
    })

    save(MSEobj,file="MSEobj")
    #save(MSEobj_FB,file="MSEobj_FB")

    # ==== Types of reporting ==========================================================

    if(input$Analysis_type%in%c("Demo","Cert")){ # Certification or demo of certification

      Ptab1<<-Ptab(MSEobj,MSEobj_FB,Eyr=10,rnd=0)
      output$Ptable <- function()Ptab_formatted(Ptab1)
      output$P1_LTY<-renderPlot(P1_LTY_plot(MSEobj))
      output$wormplot<-renderPlot(Pplot2(MSEobj,traj="quant",incquant=T,quants=c(0.1,0.9),oneIt=F,yline=c(0.5,1),quantcol=fcol,maxMP=10))#wormplot_msc(MSEobj))
      output$HCR<-renderPlot(HCRplot(MSEobj_FB))
      VOIout<<-getVOI(MSEobj)
      output$CCU<-renderPlot(CCU_plot(VOIout))
    } else if(input$Analysis_type=="FIP"){ # FIP presentations


    } else { # leaving just generic risk assessment


    }


  })

  observeEvent(input$D1,{
    if(input$Calculate>0){
    if(input$Analysis_type%in%c("Demo","Cert")){ # Certification or demo of certification

      output$Ptable <- function()Ptab_formatted(Ptab1)

    } else if(input$Analysis_type=="FIP"){ # FIP presentations


    } else { # leaving just generic risk assessment


    }
    }

  })

  observeEvent(input$M1,{
    if(input$Calculate>0){
    if(input$Analysis_type%in%c("Demo","Cert")){ # Certification or demo of certification

      output$Ptable <- function()Ptab_formatted(Ptab1)

    } else if(input$Analysis_type=="FIP"){ # FIP presentations


    } else { # leaving just generic risk assessment


    }
    }

  })

  # OM report
  output$Build_OM <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = namconv(input$Name), #"report.html",
    content = function(file) {
      doprogress("Building OM report",3)
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
                     set_type=switch(input$Analysis_type,
                                     "Demo"="Demonstration certification analysis",
                                     "RE" = "Generic risk evaluation analysis",
                                     "FIP" = "Fishery Improvement Project (FIP) analysis",
                                     "Cert" = "MSC certification analysis"),

                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     inputnames=inputnames
      )

      out<-render("OMRep.Rmd", params = params)
      file.rename(out, file)

    }
  )


  # MSE report
  output$Build_Eval <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0(namconv(input$Name),"Eval.pdf"), #"report.html",

    content = function(file) {

      src <- normalizePath('MSERep.Rmd')
      doprogress("Building evaluation report",2)
      Des<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
      MSClog<-list(PanelState, Just, Des)

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'MSERep.Rmd', overwrite = TRUE)

      library(rmarkdown)
      params <- list(test = input$Name,
                     set_title=paste0("Evaluation Report for ",input$Name),
                     set_type=switch(input$Analysis_type,
                                     "Demo"="Demonstration certification analysis",
                                     "RE" = "Generic risk evaluation analysis",
                                     "FIP" = "Fishery Improvement Project (FIP) analysis",
                                     "Cert" = "MSC certification analysis"),

                     PanelState=MSClog[[1]],
                     Just=MSClog[[2]],
                     Des=MSClog[[3]],
                     OM=OM,
                     MSEobj=MSEobj,
                     MSEobj_FB=MSEobj_FB
                     )

      out<-render("MSERep.Rmd", params = params)
      file.rename(out, file)

    }

  )


  # Fishery panel reactions ============================================================================================================

  UpJust<-function(){

    if(input$tabs1==1){
      updateTextAreaInput(session,"Justification",value=Just[[1]][Fpanel()])
    }else if(input$tabs1==2){
      updateTextAreaInput(session,"Justification",value=Just[[2]][Mpanel()])
    }else if(input$tabs1==3){
      updateTextAreaInput(session,"Justification",value=Just[[3]][Dpanel()])
    }

  }

  RecJust<-function(){

    if(input$tabs1==1 & Fpanel()>0 & Fpanel()<14){
      Just[[1]][Fpanel()]<<-input$Justification
    }else if(input$tabs1==2 & Mpanel()>0 & Mpanel()<4){
      Just[[2]][Mpanel()]<<-input$Justification
    }else if(input$tabs1==3 & Dpanel()>0 & Dpanel()<5){
      Just[[3]][Dpanel()]<<-input$Justification
    }

  }

  observeEvent(input$Justification,{
    RecJust()
  })


  observeEvent(input$tabs1, {
    UpJust()
    Des<<-list(Name=input$Name, Species=input$Species, Region=input$Region, Agency=input$Agency, nyears=input$nyears, Author=input$Author)
    MSCsave_auto()
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

  # ======================= Calculation ========================================



  # ======================= Explanatory Plots ===================================

  # -------------- Scheme --------------------

  fcol = rgb(0.4,0.8,0.95)#"#0299f"
  fcol2 = "dark grey"
  icol <- "dodgerblue4"
  maxcol="cadetblue"
  mincol="dark grey"

  # -------------- Fishery -------------------

  plotM <- function(dummy=1){

    par( mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
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

  output$plotM <- renderPlot(plotM())

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

  output$plotD <- renderPlot(plotD())

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

  output$ploth <- renderPlot(ploth())

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

  output$plotFP <- renderPlot(plotFP())

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

  output$plotF <- renderPlot(plotF())

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

  output$plotsel <- renderPlot(plotsel())

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

  output$plotdome <- renderPlot(plotdome())

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

  output$plotDR <- renderPlot(plotDR())

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

  output$plotPRM <- renderPlot(plotPRM())

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

  output$plotsigR <- renderPlot(plotsigR())

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

  output$plotq <- renderPlot(plotq())

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

  output$plotA <- renderPlot(plotA())


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
      text(0.25,0.25,"MPA",cex=2.2,font=2,col="light grey")
      fishgrid3(Vmin,fcol=icol,mpacol=fcol)
      polygon(c(0.02,0.5,0.5,0.02),c(0.02,0.02,0.5,0.5),col=NA,border='black')
      mtext("Lowest mixing",3,line=0.4)
      plot(c(0,1),c(0,1),axes=F,col="white")
      text(0.25,0.25,"MPA",cex=2.2,font=2,col="light grey")
      fishgrid3(Vmax,fcol=icol,mpacol=fcol)
      mtext("Highest mixing",3,line=0.4)
      polygon(c(0.02,0.5,0.5,0.02),c(0.02,0.02,0.5,0.5),col=NA, border='black')



    }else{

      plot(c(0,3.5),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
      text(1.75,0.5,"< Unspecified >",col="grey")

    }

  }

  output$plotV <- renderPlot(plotV())

  plotIB <- function(dummy=1){
    #IB_list<<-list("Large underages" = "IB_n30", "Underages" = "IB_n30_n10","Slight underages" = "IB_n10_0",
    #               "Taken exactly"="IB_n5_5","Slight overages"="IB_0_10","Overages"="IB_10_30","Large overages"="IB_30")

    IB_nams<-unlist(IB_list)#c("IB_n30", "IB_n30_n10","IB_n10_0","IB_n5_5","IB_0_10","IB_10_30","IB_30")

    cond<-IB_nams%in%input$IB

    if(sum(cond)>0){

      par(mfrow=c(1,2),mai=c(0.3,0.5,0.01,0.01), omi=c(0.4,0.4,0.55,0.1),cex.main = 1.5, cex.lab=1.35 )
      IB_max<-max(IB_maxes[cond])
      IB_min<-min(IB_mins[cond])

      set.seed(1)

      ts1<-c(1:20,c(41:50)/2,rep(25.5,10),(51:30)/2)
      ny1<-length(ts1)
      ts1<-ts1*exp(rnorm(ny1,0,0.2))
      ts1<-ts1/mean(ts1)*8

      ts2<-c(1:10,rep(10.5,25),(6:20)*2,seq(40,1,length.out=10))
      ny2<-length(ts2)
      ts2<-ts2*exp(rnorm(ny2,0,0.4))
      ts2<-ts2/mean(ts2)

      cols<-colsbox<-c(fcol,"black",icol)
      colsbox[2]<-'white'

      # plot TS2
      yrs<-2017-(ny2:1)-1
      ny<-length(yrs)
      UB<- IB_max*ts2
      LB<- IB_min*ts2
      plot(yrs,ts2,col="white",ylim=c(0,max(UB,ts2)),xlab="",ylab="",type='l')
      if(IB_max<1){
        polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=cols[3])
      }else if(IB_min<1){
        polygon(c(yrs,yrs[ny:1]),c(LB,ts2[ny:1]),border=NA,col=cols[3])
      }
      if(IB_min>1){
        polygon(c(yrs,yrs[ny:1]),c(UB,LB[ny:1]),border=NA,col=cols[1])
      }else if(IB_max>1){
        polygon(c(yrs,yrs[ny:1]),c(UB,ts2[ny:1]),border=NA,col=cols[1])
      }
      lines(yrs,ts2,col=cols[2],lwd=1)
      mtext("Example 1",3,line=0.8)

      legend('topleft',legend=c("Overages","TAC","Underages"),
             fill=colsbox,border='white',col=cols,lty=c(NA,1,NA),bty='n',cex=0.8)

      # plot TS1
      yrs<-2017-(ny1:1)-1
      ny<-length(yrs)
      UB<- IB_max*ts1
      LB<- IB_min*ts1
      plot(yrs,ts1,col="white",ylim=c(0,max(UB,ts1)),xlab="",ylab="",type='l')
      if(IB_max<1){
        polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=cols[3])
      }else if(IB_min<1){
        polygon(c(yrs,yrs[ny:1]),c(LB,ts1[ny:1]),border=NA,col=cols[3])
      }
      if(IB_min>1){
        polygon(c(yrs,yrs[ny:1]),c(UB,LB[ny:1]),border=NA,col=cols[1])
      }else if(IB_max>1){
        polygon(c(yrs,yrs[ny:1]),c(UB,ts1[ny:1]),border=NA,col=cols[1])
      }
      lines(yrs,ts1,col=cols[2],lwd=1)
      mtext("Example 1",3,line=0.8)
      mtext("Year",1,line=1,outer=T)
      mtext("Catches (recommended / taken) (tonnes)",2,line=0.7,outer=T)


    }else{
      par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
      plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
      text(10,0.5,"< unspecified >", col="grey")

    }


  }

  output$plotIB <- renderPlot(plotIB())

  plotIV <- function(){

    IB_nams<-c("IB_n30", "IB_n30_n10","IB_n10_0","IB_n5_5","IB_0_10","IB_10_30","IB_30")

    cond<-IB_nams%in%input$IB

    IV_nams<-unlist(IV_list)#c("IV_1","IV_1_5","IV_5_10","IV_10_20","IV_20_40")
    cond2<-IV_nams%in%input$IV

    if(sum(cond)>0){

      if(sum(cond2)>0){

        IB_max<-max(IB_maxes[cond])
        IB_min<-min(IB_mins[cond])

        set.seed(1)

        # plot TS2
        par(mai=c(0.4,0.65,0.01,0.01), omi=c(0.4,0.01,0.55,0.01), cex.main = 1.5, cex.lab=1.35 )
        layout(matrix(c(1,2),nrow=1),widths=c(2,1))

        pch=19
        ny<-40
        yrs<-2018+(1:ny)

        maxcol<-icol
        mincol<-fcol

        gen_ts<-function(sig,ny=40)exp(rnorm(ny,-0.5*sig^2,sig))
        sigI_max<-max(IV_maxes[cond2])
        sigI_min<-min(IV_mins[cond2])

        IU_max<-IB_max*gen_ts(sigI_max,ny)
        IU_min<-IB_max*gen_ts(sigI_min,ny)
        IL_max<-IB_min*gen_ts(sigI_max,ny)
        IL_min<-IB_min*gen_ts(sigI_min,ny)
        ylim=c(min(c(IU_max,IU_min,IL_max,IL_min))-0.5,quantile(c(IU_max,IU_min,IL_max,IL_min),0.97))

        plot(yrs,rep(1,ny),col="black",ylim=ylim,xlab="",ylab="",type='l',lwd=2)
        #abline(h=0,col='light grey')
        mtext("Year",1,line=2.5)
        mtext("Implemented / Recommended",2,line=2.5)
        lines(yrs,IU_max,col=maxcol)
        lines(yrs,IU_min,col=mincol)
        lines(yrs,IL_max,col=maxcol,lty=2)
        lines(yrs,IL_min,col=mincol,lty=2)
        legend('bottomleft',legend=c("Highest mean level","Lowest mean level"),lty=c(1,2),bty='n')
        legend('bottomright',legend=c("Highest variance","Lowest variance"),text.col=c(maxcol,mincol),bty='n')

        minadjust=0.5
        maxadjust=0.5
        dU_max<-density(IB_max*gen_ts(sigI_max,10000),adjust=maxadjust)
        dU_min<-density(IB_max*gen_ts(sigI_min,10000),adjust=minadjust)
        dL_max<-density(IB_min*gen_ts(sigI_max,10000),adjust=maxadjust)
        dL_min<-density(IB_min*gen_ts(sigI_min,10000),adjust=minadjust)
        scale<-max(dU_max$y, dU_min$y, dL_max$y, dL_min$y)

        plot(c(0,2),ylim,axes=F,xlab="",ylab="",col="white")
        abline(h=1,lwd=2)
        #abline(h=0,col='light grey')
        powplot<-0.66
        polygon(x=1+(dU_max$y/scale)^powplot,y=dU_max$x,col=maxcol,border=maxcol)
        polygon(x=1+(dU_min$y/scale)^powplot,y=dU_min$x,col=mincol,border=mincol)

        polygon(x=(dL_max$y/scale)^powplot,y=dL_max$x,col=maxcol,border=maxcol)
        polygon(x=(dL_min$y/scale)^powplot,y=dL_min$x,col=mincol,border=mincol)

        legend('bottomright',legend=paste("V =",c(sigI_max,sigI_min)),text.font=2,text.col=c(maxcol,mincol),bty='n')

      }else{

        par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
        plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
        text(10,0.5,"< unspecified >", col="grey")

      }

    }else{

      par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
      plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
      text(10,0.5,"< Management Q2 is unspecified >", col="grey")

    }

  }

  output$plotIV <- renderPlot(plotIV())

  plotCB <- function(dummy=1){

    CB_nams<-unlist(CB_list)#c("CB_n50_n30", "CB_n30_n10","CB_n10_0","CB_n5_5","CB_0_10")
    cond<-CB_nams%in%input$CB

    if(sum(cond)>0){

      par(mfrow=c(1,2),mai=c(0.3,0.5,0.01,0.01), omi=c(0.4,0.4,0.55,0.1),cex.main = 1.5, cex.lab=1.35 )
      Cbias_max<-max(CB_maxes[cond])
      Cbias_min<-min(CB_mins[cond])

      set.seed(2)

      ts1<-c(1:20,c(41:50)/2,rep(25.5,30))
      ny1<-length(ts1)
      ts1<-ts1*exp(rnorm(ny1,0,0.2))
      ts1<-ts1/mean(ts1)*8

      ts2<-c(seq(1,10,length.out=5),seq(10,5,length.out=15),seq(5.5,10,length.out=5),rep(11,25))
      ny2<-length(ts2)
      ts2<-ts2*exp(rnorm(ny2,0,0.25))
      ts2<-ts2/mean(ts2)

      cols<-colsbox<-c(fcol,'black',icol)
      colsbox[2]<-'white'

      # plot TS2
      yrs<-2017-(ny2:1)-1
      ny<-length(yrs)
      UB<- Cbias_max*ts2
      LB<- Cbias_min*ts2
      plot(yrs,ts2,col="white",ylim=c(0,max(UB,ts2)),xlab="",ylab="",type='l')
      if(Cbias_max<1){
        polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=cols[3])
      }else if(Cbias_min<1){
        polygon(c(yrs,yrs[ny:1]),c(LB,ts2[ny:1]),border=NA,col=cols[3])
      }
      if(Cbias_min>1){
        polygon(c(yrs,yrs[ny:1]),c(UB,LB[ny:1]),border=NA,col=cols[1])
      }else if(Cbias_max>1){
        polygon(c(yrs,yrs[ny:1]),c(UB,ts2[ny:1]),border=NA,col=cols[1])
      }
      lines(yrs,ts2,col=cols[2],lwd=1)
      mtext("Example 1",3,line=0.8)

      legend('topleft',legend=c("Catches over-reported","Catches taken","Catches under-reported"),
             fill=colsbox,border='white',col=cols,lty=c(NA,1,NA),bty='n',cex=0.8)

      # plot TS1
      yrs<-2017-(ny1:1)-1
      ny<-length(yrs)
      UB<- Cbias_max*ts1
      LB<- Cbias_min*ts1
      plot(yrs,ts1,col="white",ylim=c(0,max(UB,ts1)),xlab="",ylab="",type='l')
      if(Cbias_max<1){
        polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=cols[3])
      }else if(Cbias_min<1){
        polygon(c(yrs,yrs[ny:1]),c(LB,ts1[ny:1]),border=NA,col=cols[3])
      }
      if(Cbias_min>1){
        polygon(c(yrs,yrs[ny:1]),c(UB,LB[ny:1]),border=NA,col=cols[1])
      }else if(Cbias_max>1){
        polygon(c(yrs,yrs[ny:1]),c(UB,ts1[ny:1]),border=NA,col=cols[1])
      }
      lines(yrs,ts1,col=cols[2],lwd=1)
      mtext("Example 2",3,line=0.8)
      mtext("Year",1,line=1,outer=T)
      mtext("Catches (taken) (tonnes)",2,line=0.7,outer=T)


    }else{

      par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
      plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
      text(10,0.5,"< unspecified >", col="grey")

    }

  }

  output$plotCB <- renderPlot(plotCB())

  plotBeta <- function(){

    Beta_nams<-unlist(Beta_list)#list("Beta_200_300", "Beta_125_200","Beta_80_125", "Beta_50_80","Beta_33_50")
    cond<-Beta_nams%in%input$Beta

    if(sum(cond)>0){

      par(mfrow=c(1,2),mai=c(0.6,0.7,0.01,0.01), omi=c(0.4,0.4,0.55,0.1),cex.main = 1.5, cex.lab=1.35 )
      Beta_max<-max(Beta_maxes[cond])
      Beta_min<-min(Beta_mins[cond])

      dep<-seq(0,1,length.out=100)
      plot(c(0,1),c(0,1),type='l',lwd=2,xlab="",ylab="")
      mtext("Real Stock Depletion (SSB relative to unfished)",1,line=2.2,xlab="",ylab="")
      mtext("Relative abundance index",2,line=2.2)
      mtext("Index relative to real depletion",3,line=0.5)
      Imax<-dep^Beta_max
      Imin<-dep^Beta_min
      lines(dep,Imax,col=icol)
      lines(dep,Imin,col=fcol)

      set.seed(2)

      ts1<-seq(1,0.4,length.out=61)*(2+(cos((-0:60)/10.18))/3)*exp(rnorm(61,0,0.05))
      ts1<-ts1/max(ts1)
      tLB<-ts1^Beta_max
      tUB<-ts1^Beta_min
      ts1<-ts1/mean(ts1)
      tLB<-tLB/mean(tLB)
      tUB<-tUB/mean(tUB)
      ny<-length(ts1)

      yrs<-CurrentYr-(ny:1)
      plot(yrs,ts1,lwd=2,type="l",ylim=range(c(ts1,tLB,tUB)),xlab="",ylab="")
      mtext("Year",1,line=2.2)
      mtext("Relative abundance",2,line=2.2)
      mtext("Example indices",3,line=0.5)

      lines(yrs,tLB,col=icol)
      lines(yrs,tUB,col=fcol)

      legend('topright',c(paste0("Beta = ",c(Beta_max,Beta_min)),"True biomass"),text.col=c(icol,fcol,"black"),text.font=c(1,1,2),bty='n')

    }else{

      par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
      plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
      text(10,0.5,"< unspecified >", col="grey")

    }

  }

  output$plotBeta <- renderPlot(plotBeta())


})
