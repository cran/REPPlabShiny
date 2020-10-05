X<-data.frame(x)

auswahl <- function(x) {
  op <- strsplit(x,",")
  result <- ""
  for (inhalt in unlist(op)) {
    if(grepl(':',inhalt)){
      charreihe <- strsplit(x,":")
      reihe <- c(as.numeric(unlist(strsplit(inhalt,":")))[1]:as.numeric(unlist(strsplit(inhalt,":")))[2])
      result <- c(result,reihe)
    }else{
      result <- c(result, as.numeric(inhalt))
    }
  }
  return(as.numeric(result[-1]))
}


