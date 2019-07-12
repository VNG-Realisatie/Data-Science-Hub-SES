multimerge <- function (mylist) {
  ## mimics a recursive merge or full outer join
  
  unames <- unique(unlist(lapply(mylist, rownames)))
  
  n <- length(unames)
  
  out <- lapply(mylist, function(df) {
    
    tmp <- matrix(nr = n, nc = ncol(df), dimnames = list(unames,colnames(df)))
    tmp[rownames(df), ] <- as.matrix(df)
    rm(df); gc()
    
    return(tmp)
  })
  
  stopifnot( all( sapply(out, function(x) identical(rownames(x), unames)) ) )
  
  bigout <- do.call(cbind, out)
  colnames(bigout) <- paste(rep(names(mylist), sapply(mylist, ncol)), unlist(sapply(mylist, colnames)), sep = "")
  return(bigout)
}
