doprogress<-function(message,duration=1,n=20){
  withProgress(message = message, value = 0, {
    inc<-duration/n
    for (i in 1:n) {
      incProgress(1/n, detail = round(i*(100/n)))
      Sys.sleep(inc)
    }
  })
}

MSCsave_auto<-function(){

  MSClog<-list(PanelState, Just, Des)
  saveRDS(MSClog,file=paste0(USERID,"_autosave.frame"))

}

namconv<-function(nam){
  nam<-gsub(" ","_",nam)
  nam<-gsub("[.]","",nam)
  nam<-gsub(",","",nam)
  substr(nam,1,15)[[1]]
}

getMPs<-function(){

  if(input$Demo){
    MPs<<-c('FMSYref','AvC','DCAC','curE75','matlenlim','MRreal','MCD','DD4010')
    #MPs<-c('FMSYref','DBSRA')#,'DCAC','curE','matlenlim')

  }else {
    MPs<<-avail('MP')
    cond<-grepl("MLL",MPs)|grepl('ML',MPs)|grepl('FMSYref',MPs)
    #if(!input$Ref_MPs)cond<-cond|grepl('curE',MPs)|grepl('NFref',MPs)
    MPs<-c('FMSYref',MPs[!cond])

  }

  MPs<-MPs[!MPs%in%c("YPR","YPR_CC","YPR_ML")]
  if(input$Ex_Ref_MPs)MPs<-MPs[!MPs%in%c("FMSYref","FMSYref75","FMSYref50","NFref")]

  MPs
}

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

  if(input$tabs1==1 & Fpanel()>0 & Fpanel()<15){
    Just[[1]][Fpanel()]<<-input$Justification
  }else if(input$tabs1==2 & Mpanel()>0 & Mpanel()<4){
    Just[[2]][Mpanel()]<<-input$Justification
  }else if(input$tabs1==3 & Dpanel()>0 & Dpanel()<5){
    Just[[3]][Dpanel()]<<-input$Justification
  }

}

LowSlopes<-function(OMin, except = NULL) {
  nms <- slotNames(OMin)
  # exceptions
  if (is.null(except)) except <- "EVERYTHING"
  exclude <- unique(grep(paste(except, collapse = "|"), nms, value = FALSE))

  vars <- c("grad", "inc","sd")
  ind <- unique(grep(paste(vars, collapse = "|"), nms, value = FALSE))
  ind <- ind[(!(nms[ind] %in% exclude))]
  for (X in seq_along(ind)) {
    slot(OMin, nms[ind[X]]) <- c(0, 1e-10)
  }

  return(OMin)
}

Data_parse<-function(file){

  parsed<-strsplit(file,"/")[[1]]
  out<-list()
  out$dir<-paste(parsed[1:(length(parsed)-1)],collapse="/")
  out$name<-parsed[length(parsed)]
  out

}
