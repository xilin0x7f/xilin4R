stars.pval <- function(p) {
  if (length(p) == 0) {
    return(character())
  }

  sapply(p, function(x) {
    if (x < 0.0001) {
      return("****")
    } else if (x < 0.001) {
      return("***")
    } else if (x < 0.01) {
      return("**")
    } else if (x < 0.05) {
      return("*")
    } else {
      return("")
    }
  })
}
