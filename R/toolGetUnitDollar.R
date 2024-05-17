toolGetUnitDollar <- function(returnOnlyBase = FALSE, inPPPP = TRUE) {
  base <- 2017
  if (returnOnlyBase) {
    return(base)
  }
  pppOrMer <- if (inPPPP) " Int$PPP" else " US$MER"
  paste0("constant ", base, pppOrMer)
}
