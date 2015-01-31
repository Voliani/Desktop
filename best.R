## Find the best hospital in a state

best <- function(state, outcome) {
  h <- data.frame()
  h <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ha <- vector()
  han <- vector()
  hf <- vector()
  hfn <- vector()
  p <- vector()
  pn <- vector()
  m <- data.frame()
  cm <- data.frame()
  n <- nrow(h)
  erro <- "S"
  for (i in 1:n) {
    if (h[[7]][[i]] == state) {
      ha[i] <- as.numeric(h[[11]][[i]]) ##"heart attack" - valor
      han[i] <- h[[2]][[i]] ##"heart attack" - nome hospital
      hf[i] <- as.numeric(h[[17]][[i]]) ##"heart failure" - valor
      hfn[i] <- h[[2]][[i]] ##"heart failure" - nome hospital
      p[i] <- as.numeric(h[[23]][[i]]) ##"pneumonia" - valor
      pn[i] <- h[[2]][[i]] ##"pneumonia" - nome hospital
      erro <- "N"
    }
  }
  if (erro == "S") {
    stop("invalid state")  
  }  
  if (outcome == "heart attack") {
    m <- data.frame(ha, han)
    cm <- m[complete.cases(m),]
    result <- cm[order(cm$ha),] 
  } else {  
    if (outcome == "heart failure") {
      m <- data.frame(hf, hfn)
      cm <- m[complete.cases(m),]
      result <- cm[order(cm$hf),] 
    } else {  
      if (outcome == "pneumonia") {
        m <- data.frame(p, pn)
        cm <- m[complete.cases(m),]
        result <- cm[order(cm$p),] 
      } else {
        stop("invalid outcome")
      }
    }
  }
  result[[2]][[1]]
}
