# :vim set filetype=R

#' Denormalize nested list structures into a flat data.frame
#'
#' @name denormalize
#' @param o A list representation of a JSON object
#' @param keep Only output these columns
#' @param drop Output everything but these columns
# denormalize(z$objects)
denormalize(o, keep, drop) %::% list : . : . : data.frame
denormalize(o, keep=NULL, drop=NULL) %as% {
  out <- ldply(o, function(x) as.data.frame(.denormalize(x)))
  out <- onlyif(!is.null(keep),
    function(x) x[,colnames(x) %in% keep, drop=FALSE], out)
  out <- onlyif(!is.null(drop),
    function(x) x[,!colnames(x) %in% drop, drop=FALSE], out)
  unique(out)
}

.denormalize <- function(o) {
  o <- o[sapply(o, function(x) !is.null(x))]
  idx <- sapply(o, function(x) is.list(x) && length(x) > 1)
  d <- o[!idx]
  out <- c(d, lapply(o[idx], function(x) .denormalize(x)))
  out
}
