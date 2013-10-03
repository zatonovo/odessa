# :vim set filetype=R
BINDING_TOKEN_REGEX <- '\\$\\w+|\\$\\{\\w+\\}'
BINDING_REPLACE_REGEX <- '\\$\\w+\\{[^\\}]+\\}|\\$\\w+|\\$\\{\\w+\\}'

fold(f, EMPTY , acc) %as% acc
fold(f, x, acc) %when% { is.null(dim(x)) } %as% fold(f, x[-1], f(x[[1]], acc))
fold(f, x, acc) %as% fold(f, x[,-1,drop=FALSE], f(x[,1], acc))


clean.format <- function(format) gsub('([()])', '\\\\\\1', format, perl=TRUE)

.fetch_json(uri) %as% {
  conn <- url(uri)
  #hand <- function(e)
  #  flog.error("Unable to find package named %s. Does it exist?", id)
  json <- readLines(conn, warn=FALSE)
  close(conn)
  fromJSON(json)
}


.package_uri(id) %as% {
  base <- 'http://odessa.zatonovo.com/api/3/action/package_show?id='
  uri <- paste(base,id, sep='')
  flog.debug("[.description_uri] Constructed %s", uri)
  uri
}

Package(id) %as% {
  uri <- .package_uri(id)
  .fetch_json(uri)
}

get_binding_uri(package) %::% Package : character
get_binding_uri(package) %as% {
  names <- sapply(package$result$resources, function(r) r$name)
  idx <- which(names=='binding')
  package$result$resources[[idx]]$url
}

get_data_uri(package) %::% Package : character
get_data_uri(package) %as% {
  names <- sapply(package$result$resources, function(r) r$name)
  idx <- which(names=='data')
  if (length(idx) == 0) return("")
  package$result$resources[[idx]]$url
}

Odessa(id, fn=clean.format) %as% {
  package <- Package(id)
  data.uri <- get_data_uri(package)
  binding.uri <- get_binding_uri(package)

  conn <- textConnection(getURL(binding.uri))
  binding <- read.csv(conn, as.is=TRUE)
  binding$field <- gsub(' ','.', binding$field, fixed=TRUE)
  binding$format <- fn(binding$format)
  close(conn)
  list(id=id, data.uri=data.uri, binding.uri=binding.uri, binding=binding)
}


# OBSOLETE
#' @example
#' create_uri('electric-consumption')
create_uri(id, format='csv', fields=NULL) %as% {
  base <- "https://data.cityofchicago.org/resource"
  resource <- paste(id, format, sep='.')
  uri <- paste(base, resource, sep='/')
  flog.info("Constructed %s", uri)
  uri
}

# Wi-Fi: https://data.cityofnewyork.us/Recreation/Wifi-Hotspot-Locations/ehc4-fktp
# Electric consumption: https://data.cityofnewyork.us/Environment/Electric-Consumption-by-ZIP-Code-2010/74cu-ncm4

odessa_graph() %as% {
  id <- 'odessa-binding'
  package <- odessa.options(id)
  if (is.null(package)) {
    package <- Odessa(id)
    updateOptions(odessa.options, id,package)
  }
  package$binding
}

#' A binding is what links a data set to the odessa standard
get_binding(id) %::% character : a
get_binding(id) %as% {
  package <- odessa.options(id)
  if (is.null(package)) stop("Package was not downloaded")
  package$binding
}

#' Update a binding
#'
#' Only use for dataset development
set_binding(id, binding) %as% {
  package <- odessa.options(id)
  if (is.null(package)) stop("Package was not downloaded")

  package$binding <- binding
  updateOptions(odessa.options, id,package)
}


#' Register a transform to a dataset. This is not portable so is
#' generally discouraged. 
get_transform(id) %as% { }

#get_binding(id) %when% {
#  length(grep('odessa://', id, fixed=TRUE)) == 0
#} %as% {
#  read.csv(id, as.is=TRUE)
#}

#get_binding(id) %as% {
#  uri <- paste('http://',id, sep='')
#  read.csv(uri, as.is=TRUE)
#}

