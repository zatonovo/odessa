# :vim set filetype=R
require(lambda.r)
require(futile.logger)
require(RCurl)

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

# binding.for(df,'year')
text <- 'field,format
Year,$year $season'

text <- 'field,format
MONTH,$month
YEAR,$year'


map <- 'field,format
datetime,${date}T$time
date,$year-$month-$day
time,$hour:$minute:$second.$microsecond
'

}

get_binding(id) %as% {

}
