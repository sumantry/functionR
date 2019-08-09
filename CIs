cis <- function(x, to = "clipboard-4096", sep = "\t", decimals = 2, standardized = F, 
                stdtype = "std.all", inc.est = F, compact = F){
  if((standardized != T & standardized != F) | (compact != T & compact != F) | (inc.est != F & inc.est != T)){
    warning("standardized, inc.est, and compact must be given values of T or F\nusing default settings")
  }
  if(standardized == T){
    x <- standardizedSolution(x, ci = T, type = stdtype)
    g <- sprintf(paste0("%.",decimals,"f"), round(x[,"est.std"],decimals))
  }    
  else{
    x <- parameterestimates(x)
    g <- sprintf(paste0("%.",decimals,"f"), round(x[,"est"],decimals))
  }
  c <- sprintf(paste0("%.",decimals,"f"), round(x[,"ci.lower"],decimals))
  d <- sprintf(paste0("%.",decimals,"f"), round(x[,"ci.upper"],decimals))
  if(inc.est == T){
    f <- ifelse(is.na(x[,"pvalue"]),
                "NA",
                ifelse(x[,"pvalue"] < .001,
                       paste0(g,"**","[",c,", ",d, "]"),
                       ifelse(x[,"pvalue"] < .05,
                              paste0(g,"*","[",c,", ",d, "]"),
                              paste0(g,"[",c,", ",d, "]")))
    )
  }
  else{
    f <- paste0("[",c,", ",d, "]")
  }
  f <- data.frame(gsub("-.00", ".00", 
                       gsub(c("0."), c("."), f, useBytes = T, fixed = T),
                       useBytes = T, fixed = T))
  names(f) <- "cis"
  if(compact == T){
    if("group" %in% names(x)){
      gp <- x["group"]
      x <- bind_cols(x[1:3],gp)
      f <- bind_cols(x,f)
    } else {
      f <- bind_cols(x[c(1:3)],f)
    }
  }
  else {
    f <- bind_cols(x, f)
  }
  r <- paste0(to)
  if(to == "clipboard-4096"){
    write.table(f, to, sep = sep, row.names = F)
    cat("Copied to clipboard\n")
  }
  else{
    write.table(f, to, sep = sep, row.names = F)
    cat(paste("Saved file as:", to,"\n"))
  }}
