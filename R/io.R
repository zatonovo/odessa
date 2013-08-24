# :vim set filetype=R
BINDING_TOKEN_REGEX <- '\\$\\w+|\\$\\{\\w\\}+'

fold(f, EMPTY , acc) %as% acc
fold(f, x, acc) %when% { is.null(dim(x)) } %as% fold(f, x[-1], f(x[[1]], acc))
fold(f, x, acc) %as% fold(f, x[,-1,drop=FALSE], f(x[,1], acc))


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
  map <- 'field,parent,type,format
datetime,NA,datetime,${date}T$time
date,datetime,date,$year-$month-$day
time,datetime,datetime,$hour:$minute:$second.$microsecond
season,NA,string,$season
year,date,integer,$year
month,date,integer,$month
day,date,integer,$day
hour,time,integer,$hour
minute,time,integer,$minute
second,time,float,$second
postal_code,NA,string,$postal_code
location,NA,string,"\\\\($latitude, $longitude\\\\)"
latitude,location,float,$latitude
longitude,location,float,$longitude
'
  read.csv(textConnection(map), as.is=TRUE)
}

#' A binding is what links a data set to the odessa standard
get_binding(id) %::% character : a
get_binding(id) %when% {
  length(grep('odessa://', id, fixed=TRUE)) == 0
} %as% {
  read.csv(id, as.is=TRUE)
}

get_binding(id) %as% {
  uri <- paste('http://',id, sep='')
  read.csv(uri, as.is=TRUE)
}

