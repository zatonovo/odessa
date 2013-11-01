# :vim set filetype=R

#' Fetch a data package from the Odessa server
#'
#' Download a data package with the given ID. Once downloaded, the data
#' can be joined with other data.
#'
#' @section Usage:
#' fetch(id, fn=clean.format, ...)
#'
#' @section Details:
#' Datasets can be retrieved from the Odessa platform or locally.
#'
#' @name fetch
#' @aliases clean.format
#' @export
#' @param id The ID of the data on Odessa
#' @param fn A function to preprocess the data with
#' @param \dots Optional arguments to pass to read.csv
#' @return A data.frame containing the data
#'
#' @author Brian Lee Yung Rowe
#' @seealso \code{\link{package_list}}
#'
#' @examples
#' \dontrun{
#' # Get remote data
#' geo1 <- fetch('geolocation-1')
#' geo2 <- fetch('geolocation-2')
#' gg <- conjoin(geo1, geo2, 'location')
#'
#' # Get some local data
#' base <- system.file('extdata',package='odessa')
#' p1 <- paste("file:/", base, 'date1', sep='/')
#' p2 <- paste("file:/", base, 'date2', sep='/')
#' date1 <- fetch(p1)
#' date2 <- fetch(p2)
#' dd <- conjoin(date1,date2, c('year','month'))
#' }
fetch(id, ..., fn=clean.format) %as% {
  real.id <- id_from_path(id)
  package <- odessa.options(real.id)
  if (is.null(package)) {
    package <- Odessa(id, fn)
    updateOptions(odessa.options, real.id, package)
  }
  uri <- construct_query(package$data.uri, ...)
  flog.info("Loading dataset from %s", package$data.uri)
  if (package$data.format == 'csv') {
    o <- read.csv(uri, as.is=TRUE)
  } else {
    o <- fetch_json(uri)
  }
  o@odessa.id <- package$id
  o
}



# Escapes parentheses in the format column
#
#' @export
clean.format <- function(format) gsub('([()])', '\\\\\\1', format, perl=TRUE)

construct_query <- function(uri, ...) {
  ps <- list(...)
  qs <- sprintf("%s=%s", names(ps),
    sapply(ps, function(x) URLencode(as.character(x))))
  sprintf("%s?%s", uri, paste(qs, collapse="&"))
}

#' Get the list of available packages
#'
#' Searches Odessa for packages either using tag keywords along with
#' a specified group.
#'
#' @section Usage:
#' package_list(tag='', group='')
#'
#' @section Details:
#' As Odessa is a shared repository, there needs to be a convenient
#' way to find data on the platform. This function is an in-session
#' mechanism to search Odessa's online indices.
#'
#' @note No search capability is currently available.
#'
#' @name package_list
#' @export
#' @param tag A regular expression for searching tags
#' @param group A regular expression for searching groups
#' @return A character vector with matching results
#'
#' @author Brian Lee Yung Rowe
#' @seealso \code{\link{package_list}}
#'
package_list(tag='', group='') %as% {
  uri <- 'http://odessa.zatonovo.com/api/3/action/package_list'
  data <- .fetch_json(uri)
  data$result
}


#' Generate the index associated for the given field name. These
#' are the actual values that will be used to join with another
#' dataset.
#'
#' This is primarily used internally, but it can be useful for 
#' debugging to see what values are being generated.
#'
#' @section Usage:
#' binding.for(x, field)
#'
#' @section Details:
#' A binding is the essence of what Odessa provides. Bindings are 
#' generated indices based on construction rules and graph-based
#' inference rules.
#'
#' @name binding.for
#' @export
#' @param x An Odessa ID for a data package
#' @param field Find compatible bindings for this field
#' @return A character vector of compatible binding names
#' 
#' @author Brian Lee Yung Rowe
#'
#' @examples
#' \dontrun{
#' a <- fetch('tutorial-datetime-1')
#' binding.for(a,'date')
#' }
binding.for(x, field) %when% {
  field %in% colnames(x)
} %as% {
  x[,field]
}

binding.for(x, field) %as% {
  direct <- which.bindings(x)
  binding.for(x, field, direct)
}

# Use bindings defined directly in dataset
binding.for(x, field, direct) %when% {
  field %in% direct
} %as% {
  binding <- get_binding(x@odessa.id)
  out <- map.value(x, binding, field)
  gsub('\\','', out, fixed=TRUE)
}

# Search the Odessa graph
binding.for(x, field, direct) %as% {
  binding <- get_binding(x@odessa.id)
  graph <- field.graph(field)
  if (length(intersect(graph, direct)) < 1) {
    out <- map.ancestor(x, field)
  } else {
    node <- which(direct == graph)
    out <- map.value(x, binding, field, rev(graph[2:node]))
  }
  gsub('\\','', out, fixed=TRUE)
}


# Generate a search string to match a binding token for the given
# field name
search_string(field) %as% search_string(field, 'short')
search_string(field, 'short') %as% sprintf('\\$%s|\\$\\{%s\\}', field, field)
search_string(field, 'long') %as% sprintf('\\$%s\\{[^\\}]+\\}', field)

# Construct a regex pattern that will specify the characters to be
# extracted for a binding
match_string(field, 'short', format) %as% '(.*)'
match_string(field, 'long', format) %as% {
  pattern <- sprintf('^.*\\$%s\\{([^\\}]+)\\}.*$', field)
  sprintf('(%s)', sub(pattern, '\\1', format))
}

# Preprocess data to ensure well-formedness
clean(x) %as% {
  gsub('\n', ' ', x, fixed=TRUE)
}

map.type(x, EMPTY) %as% x
map.type(x, 'string') %as% as.character(x)
map.type(x, 'integer') %as% as.integer(x)
map.type(x, 'float') %as% as.numeric(x)
map.type(x, 'date') %as% as.Date(x)
map.type(x, 'datetime') %as% as.POSIXct(x,format='%Y-%m-%dT%H:%M:%S')
map.type(x, field) %as% {
  od <- odessa_graph()
  type <- od[od$field==field,'type']
  map.type(x, type)
}

# Map the binding format to an actual value
map.value(x, binding, field) %as% {
  types <- c('long','short')
  match.idx <- NA
  f <- function(type) {
    if (!is.na(match.idx)) return(NULL)
    search.string <- search_string(field, type)
    match.idx <- grep(search.string, binding$format)
    if (length(match.idx) == 0) return(NULL)

    match.idx <<- match.idx
    match.string <- match_string(field, type, binding$format[match.idx])
    sub(search.string, match.string, binding$format[match.idx])
  }
  regexes <- do.call(c, sapply(types, f))
  # Replace other $tokens with .*
  regexes <- gsub(BINDING_REPLACE_REGEX, '.*', regexes, perl=TRUE)
  # Now remove everything but the matched token
  map.type(sub(regexes,'\\1', clean(x[,binding$field[match.idx]])), field)
}

map.value(x, binding, field, EMPTY) %as% map.value(x, binding, field)

map.value(x, binding, field, path) %as% {
  od <- odessa_graph()
  node <- path[1]
  od <- od[od$field==node,]
  search.string <- search_string(od$field[1])
  binding$format <- sub(search.string, od$format[1], binding$format, perl=TRUE)
  map.value(x, binding, field, path[-1])
}

# Find the ancestors for the given field.
# This is causing problems:
# a <- fetch('nyc-energy-consumption-2010')
# binding.for(head(a), 'location')
map.ancestor(x, field) %as% {
  od <- odessa_graph()
  ancestor <- which.ancestors(x)
  if (all(! field %in% ancestor)) {
    msg <- "Cannot derive '%s' from available bindings: %s"
    stop(sprintf(msg, field, paste(which.bindings(x),collapse=', ')))
  }

  binding <- get_binding(x@odessa.id)
  fn <- function(a, b) {
    search.string <- search_string(a)
    match.idx <- grep(search.string, binding$format)
    apply(cbind(clean(x[, binding$field[match.idx]]), b), 1, 
      function(y) sub(search.string, y[1], y[2], perl=TRUE))
  }
  #template <- gsub('[\\\\/]','', od[od$field==ancestor,'format'], perl=TRUE)
  template <- od[od$field==ancestor,'format']
  fold(which.bindings(x), fn, rep(template,nrow(x)))
}

field.graph(NA, graph, acc) %as% acc
field.graph(EMPTY, graph, acc) %as% acc

field.graph(field, graph=odessa_graph(), acc=c()) %as% {
  parent <- graph[graph$field==field,'parent']
  field.graph(parent, graph, c(acc, field))
}


# Find the nearest common ancestor
#
# Get the greatest common factor between two fields. This is 
# equivalent to finding the nearest ancestor.
#
# If empty an error is thrown
# If field.a %in% result || field.b %in% result, then return result
# Otherwise throw error (higher common link is not supported)
gcf(field.a, field.b) %as% {
  graph.a <- field.graph(field.a)
  graph.b <- field.graph(field.b)
  intersection <- intersect(graph.a, graph.b)
  if (length(intersection) == 0) return(intersection)

  intersection[1]
}

field.path(field.a, field.b) %as% {
  fields <- c(field.a, field.b)
  gcf <- gcf(field.a, field.b)
  x <- fields[fields != gcf]
  g <- field.graph(x)
  g[1:which(g == gcf)]
}


odessa.id <- function(x) attr(x,'odessa.id')
'odessa.id<-' <- function(x,value) {
  attr(x,'odessa.id') <- value
  invisible(x)
}

#' Find the available bindings for a data package
#'
#' @name which.bindings
#' @export
#' @param a An Odessa dataset
#' @return A character vector with all available bindings
#'
#' @author Brian Lee Yung Rowe
#'
which.bindings(a) %as% {
  binding <- get_binding(a@odessa.id)
  ms <- gregexpr(BINDING_TOKEN_REGEX, binding$format, perl=TRUE)
  bs <- sapply(regmatches(binding$format, ms), function(x) x)
  if (is.list(bs)) bs <- do.call(c, bs)
  gsub('$','', as.vector(bs), fixed=TRUE)
}

#' Check if any bindings can be derived via the odessa graph
#'
#' @name which.ancestors
#' @export
#' @param a An Odessa dataset
#' @return A character vector with all available ancestors
#'
#' @author Brian Lee Yung Rowe
#'
which.ancestors(a) %as% {
  b <- which.bindings(a)
  od <- odessa_graph()
  fn <- function(format) {
    ms <- gregexpr(BINDING_TOKEN_REGEX, format, perl=TRUE)
    bs <- regmatches(format, ms)[[1]]
    bs <- gsub('$','', bs, fixed=TRUE)
    all(bs %in% b)
  }
  as <- sapply(od$format, fn)
  setdiff(od$field[which(as)], b)
}


# TODO: Data types in binding files for automatic conversion (default string)
# TODO: Automatic join column based on gcf (where possible)
# TODO: Composite joins (over multiple datasets n > 2)
# fold(c('id1','id2','id3'), conjoin)
#
#' Conjoin two data.frames together
#' 
#' Take two Odessa data packages and join them together given the 
#' common field. If no field is specified, then use the
#' greatest common factor based on the graph of each index.
#'
#' @section Usage:
#' conjoin(a,b, field, ...)
#'
#' @section Details:
#' The power of Odessa is in being able to join datasets together
#' without a lot of ceremony. Data that has been fetched via Odessa 
#' can be joined via their bindings. The ideal situation is that
#' compatible bindings are inferred from the metadata and master
#' graphs. Currently you have to specify the binding.
#'
#' @name conjoin
#' @export
#' @param a An Odessa data.frame
#' @param b An Odessa data.frame
#' @return A new data.frame conjoined on a common index
#'
#' @author Brian Lee Yung Rowe
#'
#' @examples
#' \dontrun{
#' df1 <- fetch('vbts-zqt4')
#' odessa.id(df1) <- 'vbts-zqt4.binding.csv'
#' df2 <- fetch('vz8e-347q')
#' odessa.id(df2) <- 'vz8e-347q.binding.csv'
#' conjoin(df1,df2, 'year')
#' }
#'
conjoin(a,b) %as% {
  abs <- which.bindings(a)
  bbs <- which.bindings(b)
  stop("Incomplete")
}

conjoin(a,b, field, ...) %as% {
  key.a <- sapply(field, function(f) binding.for(a, f))
  key.b <- sapply(field, function(f) binding.for(b, f))
  #key.names <- ifelse(length(field) > 1, paste('odessa.key',field, sep='.'), field)
  key.names <- paste('odessa.key',field, sep='.')
  anynames(key.a) <- anynames(key.b) <- key.names
  o <- merge(cbind(key.a,a), cbind(key.b,b), by=key.names, ...)
  o@odessa.id <- a@odessa.id
  o
}


