# :vim set filetype=R
BINDING_TOKEN_REGEX <- '\\$\\w+|\\$\\{\\w+\\}'
BINDING_REPLACE_REGEX <- '\\$\\w+\\{[^\\}]+\\}|\\$\\w+|\\$\\{\\w+\\}'

#' A type that represents a data package
#'
#' Every dataset in Odessa is a package containing the actual dataset
#' and a binding file containing metadata.
#'
#' @section Usage:
#' PackageDescriptor(id)
#'
#' @section Details:
#' This is the CKAN package information
#'
#' @name PackageDescriptor
#' @export
#' @param id The Odessa ID for the dataset
#' @return A typed data.frame
#'
#' @author Brian Lee Yung Rowe
#'
PackageDescriptor(id) %as% {
  uri <- .package_uri(id)
  fetch_data(uri, 'json')
}

#' A type that represents the Odessa binding information for a package
#'
#' Every dataset in Odessa is a package containing the actual dataset
#' and a binding file containing metadata.
#'
#' @section Usage:
#' Odessa(id, fn=clean.format)
#'
#' @section Details:
#' An end user generally doesn't need to work directly with an Odessa
#' object.
#'
#' @name Odessa
#' @export
#' @param id The Odessa ID for the dataset
#' @param path A local file path that ends in the id of the dataset
#' @param fn A function used to clean the data
#' @return A typed data.frame
#'
#' @author Brian Lee Yung Rowe
#'
Odessa(path, fn=clean.format) %when% {
  grepl('^file:', path)
} %as% {
  id <- id_from_path(path)
  data.uri <- paste(path,'csv', sep='.')
  binding.uri <- paste(path,'binding','csv', sep='.')

  binding <- read.csv(binding.uri, as.is=TRUE)
  binding$field <- gsub(' ','.', binding$field, fixed=TRUE)
  binding$format <- fn(binding$format)
  list(id=id, data.format='csv', data.uri=data.uri,
       binding.uri=binding.uri, binding=binding)
}

Odessa(id, fn=clean.format) %as% {
  package <- PackageDescriptor(id)
  data.uri <- get_data_uri(package)
  binding.uri <- get_binding_uri(package)

  conn <- textConnection(getURL(binding.uri))
  binding <- read.csv(conn, as.is=TRUE)
  binding$field <- gsub(' ','.', binding$field, fixed=TRUE)
  binding$format <- fn(binding$format)
  close(conn)
  list(id=id, data.format=data_format(package), data.uri=data.uri,
       binding.uri=binding.uri, binding=binding)
}

data_format <- function(package) {
  n <- sapply(package$result$resources, function(x) x$name)
  idx <- which(n == 'data')
  if (length(idx) == 0) return("")
  tolower(package$result$resources[[idx]]$format)
}

id_from_path(path) %when% { is.scalar(path) } %as% {
  id <- tail(strsplit(path,'[/\\]')[[1]],1)
  sub(".csv$","", id)
}

#' Show the raw binding data for the given data package
#'
#' This function displays the raw binding data for examination.
#' Typically it is not needed for casual use of the library.
#'
#' @section Usage:
#' get_binding %::% character : a
#' get_binding(id)
#'
#' @section Details:
#' A binding is what links a data set to the Odessa standard.
#' A data package can have multiple bindings. This function shows all
#' the bindings explicitly registered for the package.
#'
#' @name get_binding
#' @export
#' @param id The Odessa ID for the dataset
#' @return A data.frame containing the binding information
#'
#' @author Brian Lee Yung Rowe
#'
get_binding(id) %::% character : data.frame
get_binding(id) %as% {
  package <- odessa.options(id)
  if (is.null(package)) stop("Package was not downloaded")
  package$binding
}

#' Update the binding file for a package
#'
#' When developing a new data package, this is useful for verifying that
#' a binding file works correctly by updating a binding file locally.
#'
#' @section Usage:
#' set_binding(id, binding)
#'
#' @section Details:
#' Only use for dataset development
#'
#' @name set_binding
#' @export
#' @param id The Odessa ID for the dataset
#' @param binding A data.frame that represents the binding data
#'
#' @author Brian Lee Yung Rowe
#'
set_binding(id, binding) %as% {
  package <- odessa.options(id)
  if (is.null(package)) stop("Package was not downloaded")

  package$binding <- binding
  updateOptions(odessa.options, id,package)
}


fetch_data(uri, 'csv', ...) %as% {
  text <- getURL(uri, ...)
  read.csv(text=text, as.is=TRUE)
}

fetch_data(uri, 'json', ...) %as% {
  #conn <- url(uri)
  #hand <- function(e)
  #  flog.error("Unable to find package named %s. Does it exist?", id)
  #json <- readLines(conn, warn=FALSE)
  #close(conn)
  text <- getURL(uri, ...)
  fromJSON(paste(text, collapse=' '))
}


.package_uri(id) %as% {
  base <- 'http://odessa.zatonovo.com/api/3/action/package_show?id='
  uri <- paste(base,id, sep='')
  flog.debug("[.description_uri] Constructed %s", uri)
  uri
}

get_binding_uri(package) %::% PackageDescriptor : character
get_binding_uri(package) %as% {
  names <- sapply(package$result$resources, function(r) r$name)
  idx <- which(names=='binding')
  package$result$resources[[idx]]$url
}

get_data_uri(package) %::% PackageDescriptor : character
get_data_uri(package) %as% {
  names <- sapply(package$result$resources, function(r) r$name)
  idx <- which(names=='data')
  if (length(idx) == 0) return("")
  package$result$resources[[idx]]$url
}


# OBSOLETE
# @example
# create_uri('electric-consumption')
create_uri(id, format='csv', fields=NULL) %as% {
  base <- "https://data.cityofchicago.org/resource"
  resource <- paste(id, format, sep='.')
  uri <- paste(base, resource, sep='/')
  flog.info("Constructed %s", uri)
  uri
}

# Wi-Fi: https://data.cityofnewyork.us/Recreation/Wifi-Hotspot-Locations/ehc4-fktp
# Electric consumption: https://data.cityofnewyork.us/Environment/Electric-Consumption-by-ZIP-Code-2010/74cu-ncm4

#' Get the master binding graph of Odessa
#'
#' @name odessa_graph
#' @export
#' @return The master binding graph of Odessa itself
odessa_graph() %as% {
  id <- 'odessa-binding'
  package <- odessa.options(id)
  if (is.null(package)) {
    package <- Odessa(id)
    updateOptions(odessa.options, id,package)
  }
  package$binding
}

# Register a transform to a dataset. This is not portable so is
# generally discouraged. 
#get_transform(id) %as% { }

