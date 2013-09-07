#' geo1 <- fetch('geolocation-1')
#' geo2 <- fetch('geolocation-2')
#' z <- conjoin(geo1, geo2, 'location')
#' z <- conjoin(geo1, geo2, 'latitude')
#' z <- conjoin(geo1, geo2, 'longitude')

#' d1 <- fetch('datetime-1')
#' d2 <- fetch('datetime-2')
#' z <- conjoin(d1,d2, c('year','month'))
