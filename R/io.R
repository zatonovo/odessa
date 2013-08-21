# :vim set filetype=R
BINDING_TOKEN_REGEX <- '\\$\\w+|\\$\\{\\w\\}+'

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
foo <- function() {
text <- 'field,format
Zip.Code,$zip_code
Location,"($latitude, $longitude)"'


}

odessa_graph() %as% {
  map <- 'field,parent,type,format
datetime,NA,POSIXct,${date}T$time
date,datetime,POSIXct,$year-$month-$day
time,datetime,POSIXct,$hour:$minute:$second.$microsecond
season,NA,string,$season
year,date,integer,$year
month,date,integer,$month
day,date,integer,$day
hour,time,integer,$hour
minute,time,integer,$minute
second,time,integer,$second
microsecond,time,integer,$microsecond
postal_code,NA,string,$postal_code
location,NA,string,"($latitude, $longitude)"
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

