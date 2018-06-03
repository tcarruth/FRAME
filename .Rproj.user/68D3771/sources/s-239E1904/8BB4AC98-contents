
Fease2 <- function(Data=NULL, TAC=TRUE, TAE=TRUE, SL=TRUE, Spatial=TRUE, names.only=TRUE, msg=TRUE, include.ref=FALSE) {
  if (msg) {
    message("Feasible management: ")
    if (TAC) message("TAC - total allowable catch")
    if (TAE) message("TAE - total allowable effort")
    if (SL) message("SL - size selectivity")
    if (Spatial) message("Spatial - spatial closures")
  }
  if (!(TAC | TAE | SL | Spatial)) stop("No feasible management options!", call.=FALSE)
  MPs <- avail('MP')
  if (class(Data) == "Data") {
    if (msg) message("Data object provided. Returning feasible and available MPs")
    canMPs <- Can(Data)
  } else {
    if (msg) message("No Data object provided. Returning feasible MPs")
    canMPs <- MPs
  }
  mptypes <- MPtype(MPs)
  mprecs <- mptypes[,3]
  isfease <- rep(TRUE, length(MPs))
  isfease[17]
  cbind(MPs, mprecs)

  if (!TAC) isfease[grepl("TAC", mprecs)] <- FALSE
  if (!TAE) isfease[grepl("TAE", mprecs)] <- FALSE
  if (!SL) isfease[grepl("SL", mprecs)] <- FALSE
  if (!Spatial) isfease[grepl("Spatial", mprecs)] <- FALSE


  df <- data.frame(MP=mptypes[,1], Can=mptypes[,1]%in%canMPs, Fease=isfease, stringsAsFactors = FALSE)
  df <- df[order(df$MP),]
  if (!include.ref)df <- df[mptypes[,2] != "Reference",]

  if (names.only) {
    return(df$MP[df$Can & df$Fease])
  } else {
    return(df)
  }
}

