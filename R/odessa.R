# :vim set filetype=R

#' @example
#' df1 <- fetch('vbts-zqt4')
#' df2 <- fetch('vz8e-347q')

#' df1 <- fetch('zh3n-jtnt')
#' df2 <- fetch('zh3n-jtnt')
fetch(id, format='csv', fields=NULL, ...) %as% {
  uri <- create_uri(id, format, fields)
  z <- textConnection(getURL(uri))
  o <- read.csv(z, ...)
  o@odessa.id <- id
  o
}



# 1 Look for exact match in data set binding (in the format column)
# 2 If so then return parsed value (in case of pattern in format)
# 3 If not then look for match in odessa standard
# 4 If match then find parent and search until match or terminate
# 5 Keep track of graph so each format can be applied
binding.for(x, field) %as% {
  binding <- get_binding(x@odessa.id)
  # 1
  search.string <- sprintf('\\$%s|\\$\\{%s\\}', field, field)
  matches <- grep(search.string, binding$format, value=TRUE)
  regexes <- sub(search.string, '(.*)', matches)
  # Replace other $tokens with .*
  # Now remove everything but the matched token
  #sub("(.*)T.*", '\\1', '2012-23-23T12:33:32.233')
  sub(regexes,'\\1', data)
  
  apply()
}


conjoin(a,b, field) %as% {
  key.a <- binding.for(a, field)
  key.b <- binding.for(b, field)
  merge(cbind(odessa.key=key.a,a), cbind(odessa.key=key.b,b), by='odessa.key')
}
