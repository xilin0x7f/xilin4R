stars.pval <- function(p) {
  if (length(p) == 0) {
    return(character())
  }

  if (p < 0.0001) {
    return("****")
  }else if (p < 0.001) {
    return("***")
  } else if (p < 0.01) {
    return("**")
  } else if (p < 0.05) {
    return("*")
  } else {
    return("")
  }
}
