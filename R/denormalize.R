# :vim set filetype=R

#' Denormalize nested list structures into a flat data.frame
#'
#' @name denormalize
#' @export
#' @param o A list representation of a JSON object
#' @param keep Only output these columns
#' @param drop Output everything but these columns
# denormalize(z$objects)
denormalize(o, keep, drop, transform) %::% list : . : . : . : data.frame
denormalize(o, keep=NULL, drop=NULL, transform=NULL) %as% {
  out <- ldply(o, function(x) .denormalize(x, keep, drop, NULL, transform))
  #out <- onlyif(!is.null(keep),
  #  function(x) x[,colnames(x) %in% keep, drop=FALSE], out)
  #out <- onlyif(!is.null(drop),
  #  function(x) x[,!colnames(x) %in% drop, drop=FALSE], out)
  out <- unique(out)
  if (! is.null(o@odessa.id)) out@odessa.id <- o@odessa.id
  if (! is.null(keep)) out <- out[,intersect(keep,colnames(out))]
  out
}

.get_nodes <- function(keep, ns) {
  if (! is.null(ns)) 
    nodes <- strsplit(sub(sprintf('^%s',ns), '', keep),'.',fixed=TRUE)
  else
    nodes <- strsplit(keep,'.',fixed=TRUE)

  do.call(c, lapply(nodes, function(x) if (length(x) > 1) return(x[1])) )
}

.denormalize <- function(o, keep, drop, ns=NULL, transform) {
  o <- o[sapply(o, function(x) !is.null(x) && length(x) > 0)]
  # BOOK: Compare with pattern matching
  # Also immutability of variables in mathematical definitions
  if (is.null(ns)) k <- names(o)
  else k <- paste(ns, names(o), sep='.')

  # Add roots to ensure traversal of sub nodes
  keep <- c(keep, .get_nodes(keep,ns))

  # BOOK: Compare with onlyif
  # TODO: Ensure indices are compatible
  clean.fn <- function(x) {
    if (! is.null(keep)) x <- x[k %in% keep]
    if (! is.null(drop)) x <- x[! k %in% drop]
    x
  }
  o <- clean.fn(o)
  k <- clean.fn(k)
  if (length(o) == 0) return(NULL)

  idx <- sapply(o, function(x) is.list(x))
  # Apply transform
  if (is.null(transform)) d <- o[!idx]
  else d <- lapply(o[[!idx]], transform)
  names(d) <- k[!idx]

  ns.fn <- function(ns, x) {
    if (is.null(ns)) return(x)
    paste(ns,x,sep='.')
  }

  out <- c(d, do.call(c, lapply(names(o[idx]), 
    function(n) .denormalize(o[[n]], keep, drop, ns.fn(ns,n), transform))))
  as.data.frame(out)
}


#' Convert a column major data.frame into a row-major compatible list structure
#'
#' Standard encoding of a data.frame to a JSON structure is based on the
#' column major conventions of R whereas JSON table structures are
#' effectively row-major.
#'
#' @name row_major
#' @export
#' @param data The object to transform
#' @param names Whether to assign any names to the resulting rows. The
#'  default is NULL, which means that the conversion to JSON will be an
#'  array. If this is non null then the base structure will be a hash
#'  instead of an array.
#' @examples
#' df <- data.frame(a=1:4, b=2:5, c=3:6)
#' RJSONIO::toJSON(row_major(df))
row_major(data, names=NULL) %::% data.frame : . : list
row_major(data, names=NULL) %as% {
  o <- apply(data, 1, as.list)
  names(o) <- names
  o
}
