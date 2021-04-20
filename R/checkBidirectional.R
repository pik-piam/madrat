.checkBidirectional <- function(graph, details = FALSE, cutoff = 1000) {
  pkgdeps <- unique(graph[c("from_package","to_package")])
  pkgdeps <- pkgdeps[pkgdeps$from_package!=pkgdeps$to_package & pkgdeps$from_package!="UNKNOWN",]
  din <- paste0(pkgdeps$from_package,"-",pkgdeps$to_package)
  dout <- paste0(pkgdeps$to_package,"-",pkgdeps$from_package)  
  if(nrow(pkgdeps)>1 && any(din %in% dout)) {
    p <- pkgdeps[din %in% dout,]
    hints <- NULL
    if(details) {
      hints_full <- list()
      message("[warning]\n[warning] bidirectional dependencies found: \n[warning]   ",
              paste0(p$from_package," -> ",p$to_package,collapse="\n[warning]   "))
      message("[warning]\n[warning] The following links between these packages exist:")
    }
    for(i in 1:nrow(p)) {
      tmp <- graph[graph$from_package==p$from_package[i] & graph$to_package==p$to_package[i],]
      if(details) {
        hints_full[[i]] <- paste0(tmp$from," -> ",tmp$to)
        message("[warning]\n[warning]   .: PACKAGES | ",paste0(p$from_package[i]," -> ",p$to_package[i])," :.")
        if(length(hints_full[[i]]) > cutoff) {
          hints_short <- c(hints_full[[i]][1:cutoff],"...")
        } else {
          hints_short <- hints_full[[i]]
        }
        message("[warning]     ",paste0(hints_short,collapse="\n[warning]     "))
      }
      if(nrow(tmp)<5 & nrow(tmp)>0) hints <- paste0(hints,paste0(tmp$from,"->",tmp$to,collapse=", "), collapse=", ") 
      warning("Bidirectional package dependencies detected: ",paste0(p$from_package,"->",p$to_package,collapse=", "),
              "\n  You might want to have a look at the following connections: ",hints)
    }
  } else if (details) {
    message("[passed] no bidirectional package connections found!")
  }
}