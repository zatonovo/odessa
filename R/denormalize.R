# :vim set filetype=R

#' Denormalize nested list structures into a flat data.frame
#'
#' @name denormalize
#' @param o A list representation of a JSON object
#' @param keep Only output these columns
#' @param drop Output everything but these columns
#' @export
# denormalize(z$objects)
denormalize(o, keep, drop) %::% list : . : . : data.frame
denormalize(o, keep=NULL, drop=NULL) %as% {
  out <- ldply(o, function(x) as.data.frame(.denormalize(x)))
  out <- onlyif(!is.null(keep),
    function(x) x[,colnames(x) %in% keep, drop=FALSE], out)
  out <- onlyif(!is.null(drop),
    function(x) x[,!colnames(x) %in% drop, drop=FALSE], out)
  out <- unique(out)
  onlyif(! is.null(o@odessa.id), 
    function(x) { x@odessa.id <- o@odessa.id; x }, out)
}

.denormalize <- function(o) {
  o <- o[sapply(o, function(x) !is.null(x) && length(x) > 0)]
  if (length(o) == 0) return(NULL)

  idx <- sapply(o, function(x) is.list(x))
  d <- o[!idx]
  out <- c(d, do.call(c, lapply(o[idx], function(x) .denormalize(x))))
  out
}
