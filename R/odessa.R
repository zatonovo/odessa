# :vim set filetype=R

#' @example
#' # Number of sessions
#' df1 <- fetch('vbts-zqt4')
#' # Chicago course success rate
#' df2 <- fetch('vz8e-347q')

#' df1 <- fetch('zh3n-jtnt')
#' df2 <- fetch('zh3n-jtnt')
fetch(id, format='csv', fields=NULL, ...) %when% {
  length(grep('odessa://',id, fixed=TRUE)) > 0
} %as% {
  uri <- create_uri(id, format, fields)
  z <- textConnection(getURL(uri))
  o <- read.csv(z, ..., as.is=TRUE)
  o@odessa.id <- id
  o
}

fetch(id, format='csv', fields=NULL, ...) %as% {
  z <- paste(id,'csv', sep='.')
  flog.info("Reading local file %s",z)
  o <- read.csv(z, ..., as.is=TRUE)
  o@odessa.id <- paste(id,'binding.csv', sep='.')
  o
}

search_string(field) %as% sprintf('\\$%s|\\$\\{%s\\}', field, field)

map.value(x, binding, field) %as% {
  search.string <- search_string(field)
  match.idx <- grep(search.string, binding$format)
  regexes <- sub(search.string, '(.*)', binding$format[match.idx])
  # Replace other $tokens with .*
  regexes <- gsub(BINDING_TOKEN_REGEX, '.*', regexes, perl=TRUE)
  # Now remove everything but the matched token
  sub(regexes,'\\1', x[,binding$field[match.idx]])
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

# 4 If match then find parent and search until match or terminate
# 5 Keep track of graph so each format can be applied
binding.for(x, field) %as% {
  direct <- which.bindings(x)
  binding <- get_binding(x@odessa.id)

  if (field %in% direct) {
    map.value(x, binding, field)
  }
  else {
    graph <- field.graph(field)
    if (length(intersect(graph, direct)) < 1) {
      msg <- "Cannot derive '%s' from available bindings: %s"
      stop(sprintf(msg, field, paste(direct,collapse=', ')))
    }
    node <- which(direct == graph)
    map.value(x, binding, field, rev(graph[2:node]))
  }
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

transform.field(x, field) %as% {

}

odessa.id <- function(x) attr(x,'odessa.id')
'odessa.id<-' <- function(x,value) {
  attr(x,'odessa.id') <- value
  invisible(x)
}

which.bindings(a) %as% {
  binding <- get_binding(a@odessa.id)
  m <- regexpr(BINDING_TOKEN_REGEX, binding$format, perl=TRUE)
  b <- regmatches(binding$format, m)
  gsub('$','',b, fixed=TRUE)
}


#' @example
#' df1 <- fetch('vbts-zqt4')
#' odessa.id(df1) <- 'vbts-zqt4.binding.csv'
#' df2 <- fetch('vz8e-347q')
#' odessa.id(df2) <- 'vz8e-347q.binding.csv'
#' conjoin(df1,df2, 'year')
conjoin(a,b, field, ...) %as% {
  key.a <- binding.for(a, field)
  key.b <- binding.for(b, field)
  merge(cbind(odessa.key=key.a,a), cbind(odessa.key=key.b,b),
    by='odessa.key', ...)
}
