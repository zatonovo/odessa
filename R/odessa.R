# :vim set filetype=R

odessa.reset <- function() options(odessa.options=list())
if (!exists('odessa.options'))
  odessa.options <- OptionsManager('odessa.options')


#' @example
#' geo1 <- fetch('geolocation-1')
#' geo2 <- fetch('geolocation-2')
#' z <- conjoin(geo1, geo2, 'location')
fetch(id, fn=clean.format, ...) %as% {
  package <- odessa.options(id)
  if (is.null(package)) {
    package <- Odessa(id, fn)
    updateOptions(odessa.options, id,package)
  }
  z <- textConnection(getURL(package$data.uri))
  o <- read.csv(z, ..., as.is=TRUE)
  o@odessa.id <- package$id
  o
}

package_list() %as% {
  uri <- 'http://odessa.zatonovo.com/api/3/action/package_list'
  data <- .fetch_json(uri)
  data$result
}

#fetch(id, format='csv', fields=NULL, ...) %when% {
#  length(grep('://',id, fixed=TRUE)) > 0
#} %as% {
#  z <- paste(id,'csv', sep='.')
#  flog.info("Reading local file %s",z)
#  o <- read.csv(z, ..., as.is=TRUE)
#  o@odessa.id <- paste(id,'binding.csv', sep='.')
#  o
#}

search_string(field) %as% sprintf('\\$%s|\\$\\{%s\\}', field, field)

clean(x) %as% {
  gsub('\n', ' ', x, fixed=TRUE)
}

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

map.value(x, binding, field) %as% {
  search.string <- search_string(field)
  match.idx <- grep(search.string, binding$format)
  regexes <- sub(search.string, '(.*)', binding$format[match.idx])
  # Replace other $tokens with .*
  regexes <- gsub(BINDING_TOKEN_REGEX, '.*', regexes, perl=TRUE)
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
  fold(fn, which.bindings(x), rep(template,nrow(x)))
}

# 4 If match then find parent and search until match or terminate
# 5 Keep track of graph so each format can be applied
binding.for(x, field) %as% {
  direct <- which.bindings(x)
  binding.for(x, field, direct)
}

binding.for(x, field, direct) %when% {
  field %in% direct
} %as% {
  binding <- get_binding(x@odessa.id)
  map.value(x, binding, field)
}

binding.for(x, field, direct) %as% {
  binding <- get_binding(x@odessa.id)
  graph <- field.graph(field)
  if (length(intersect(graph, direct)) < 1)
    return(map.ancestor(x, field))

  node <- which(direct == graph)
  map.value(x, binding, field, rev(graph[2:node]))
}


field.graph(NA, graph, acc) %as% acc
field.graph(EMPTY, graph, acc) %as% acc

field.graph(field, graph=odessa_graph(), acc=c()) %as% {
  parent <- graph[graph$field==field,'parent']
  field.graph(parent, graph, c(acc, field))
}


#' Get the lowest common denominator between two fields
#' If empty an error is thrown
#' If field.a %in% result || field.b %in% result, then return result
#' Otherwise throw error (higher common link is not supported)
lcd(field.a, field.b) %as% {
  graph.a <- field.graph(field.a)
  graph.b <- field.graph(field.b)
  intersection <- intersect(graph.a, graph.b)
  if (length(intersection) == 0) return(intersection)

  intersection[1]
}

field.path(field.a, field.b) %as% {
  fields <- c(field.a, field.b)
  lcd <- lcd(field.a, field.b)
  x <- fields[fields != lcd]
  g <- field.graph(x)
  g[1:which(g == lcd)]
}


odessa.id <- function(x) attr(x,'odessa.id')
'odessa.id<-' <- function(x,value) {
  attr(x,'odessa.id') <- value
  invisible(x)
}

which.bindings(a) %as% {
  binding <- get_binding(a@odessa.id)
  ms <- gregexpr(BINDING_TOKEN_REGEX, binding$format, perl=TRUE)
  bs <- sapply(regmatches(binding$format, ms), function(x) x)
  gsub('$','', bs, fixed=TRUE)
}

#' Check if any bindings can be derived via the odessa graph
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


#' @example
#' df1 <- fetch('vbts-zqt4')
#' odessa.id(df1) <- 'vbts-zqt4.binding.csv'
#' df2 <- fetch('vz8e-347q')
#' odessa.id(df2) <- 'vz8e-347q.binding.csv'
#' conjoin(df1,df2, 'year')
conjoin(a,b, field, ...) %as% {
  key.a <- sapply(field, function(f) binding.for(a, f))
  key.b <- sapply(field, function(f) binding.for(b, f))
  key.names <- ifelse(length(field) > 1, paste('odessa.key',field, sep='.'), field)
  merge(cbind(odessa.key=key.a,a), cbind(odessa.key=key.b,b), by=key.names, ...)
}
