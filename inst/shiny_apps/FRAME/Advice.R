runMP_MSC <- function(Data, MPs = NA, reps = 100, perc=0.5, chkMPs=TRUE, silent=TRUE,shiny=FALSE) {
  if (class(MPs) != 'character' && !all(is.na(MPs))) stop('MPs must be character string', call.=FALSE)
  if (class(Data) != 'Data') stop("Data must be class 'Data'", call.=FALSE)
  if (all(is.na(MPs))) {
    MPs <- avail("MP")
    if (!silent) message("running all available MPs")
  }
  if (chkMPs) {
    cans <- Can(Data, MPs=MPs)
    MPs <- MPs[MPs %in% cans]
  }
  if (length(MPs) <1) stop("No MPs possible")

  MPrecs <- applyMP_MSC(Data, MPs, reps, nsims=1, silent=silent,shiny=shiny)

  names <- c("TAC", "Effort", "LR5", "LFR", "HS", "Rmaxlen",
             "L5", "LFS", 'Vmaxlen', 'Spatial')
  mat <- matrix(0, nrow=length(MPs), ncol=length(names)+Data@nareas-1)
  for (x in seq_along(names)) {
    temp <- lapply(MPrecs[[1]], '[[', names[x])
    if (names[x]!="Spatial") {
      mat[,x] <- unlist(lapply(temp, quantile, probs=perc, na.rm=TRUE))
    } else {
      mat[,x:ncol(mat)] <- t(matrix(unlist(temp), nrow=Data@nareas, ncol=length(MPs)))
    }
  }

  rownames(mat) <- MPs
  names[length(names)] <- "Area 1"
  names <- c(names, paste('Area', 2:Data@nareas))
  colnames(mat) <- names

  if (nrow(mat) > 1) {
    allNA <- colSums(apply(mat, 2, is.na)) == length(MPs)
    matout <- data.frame(round(mat[,!allNA], 2), stringsAsFactors = FALSE)
    names(matout) <- names[!allNA]
  }

  if (nrow(mat) == 1) {
    mat <- data.frame(mat)
    matout <- mat[!is.na(mat)]
    matout <- matrix(matout, nrow=nrow(mat))
    colnames(matout) <- names[!is.na(mat)]
    rownames(matout) <- MPs
  }

  #matout[is.na(matout)]<-"-"

  list(matout=matout,Data=MPrecs[[2]])

}

#' Apply Management Procedures to an object of class Data
#'
#' @param Data An object of class Data
#' @param MPs Name(s) of the MPs to run
#' @param reps Number of samples
#' @param nsims Optional. Number of simulations.
#' @param silent Logical. Should messages be suppressed?
#'
#' @return A list with the first element a list of management recommendations,
#' and the second the updated Data object
#' @export
#'
applyMP_MSC <- function(Data, MPs = NA, reps = 100, nsims=NA, silent=FALSE,shiny=F) {
  if (class(Data) != "Data") stop("First argument must be object of class 'Data'", call.=FALSE)
  Data <- updateMSE(Data)
  if (is.na(nsims)) nsims <- length(Data@Mort)
  nMPs <- length(MPs)

  incrate<-1/nMPs


  if (.hasSlot(Data, "nareas")) {
    nareas <- Data@nareas
  } else {
    nareas <- 2
  }
  returnList <- list() # a list nMPs long containing MPs recommendations
  recList <- list() # a list containing nsim recommendations from a single MP
  TACout <- array(NA, dim=c(nMPs, reps, nsims))
  # if (!sfIsRunning() | (nMPs < 8 & nsims < 8)) {
  for (mp in 1:nMPs) {
    if(shiny)incProgress(incrate)
    temp <- lapply(1:nsims, MPs[mp], Data = Data, reps = reps)
    slots <- slotNames(temp[[1]])
    for (X in slots) { # sequence along recommendation slots
      if (X == "Misc") { # convert to a list nsim by nareas
        rec <- lapply(temp, slot, name=X)
      } else {
        rec <- do.call("cbind", lapply(temp, slot, name=X)) # unlist(lapply(temp, slot, name=X))
      }
      if (X == "Spatial") { # convert to a matrix nsim by nareas
        rec <- matrix(rec, nareas, nsims, byrow=FALSE)
      }
      recList[[X]] <- rec
      for (x in 1:nsims) Data@Misc[[x]] <- recList$Misc[[x]]
      recList$Misc <- NULL
    }
    if (length(recList$TAC)>0)  TACout[mp,,] <- recList$TAC
    returnList[[mp]] <- recList
    if (!silent && any(apply(is.na(recList$TAC), 2, sum) > rep(0.5 * reps, nsims)))
      message("Method ", MPs[mp], " produced greater than 50% NA values")
  }

  Data@TAC <- TACout
  Data@MPs <- MPs

  list(returnList, Data)
}
