\name{fetch}
\alias{clean.format}
\alias{fetch}
\title{Fetch a data package from the Odessa server}
\arguments{
  \item{id}{The ID of the data on Odessa}

  \item{fn}{A function to preprocess the data with}

  \item{\dots}{Optional arguments to pass to read.csv}
}
\value{
  A data.frame containing the data
}
\description{
  Download a data package with the given ID. Once
  downloaded, the data can be joined with other data.
}
\section{Usage}{
  fetch(id, fn=clean.format, ...)
}

\section{Details}{
  Datasets can be retrieved from the Odessa platform or
  locally.
}
\examples{
\dontrun{
# Get remote data
geo1 <- fetch('geolocation-1')
geo2 <- fetch('geolocation-2')
gg <- conjoin(geo1, geo2, 'location')

# Get some local data
base <- system.file('extdata',package='odessa')
p1 <- paste("file:/", base, 'date1', sep='/')
p2 <- paste("file:/", base, 'date2', sep='/')
date1 <- fetch(p1)
date2 <- fetch(p2)
dd <- conjoin(date1,date2, c('year','month'))
}
}
\author{
  Brian Lee Yung Rowe
}
\seealso{
  \code{\link{package_list}}
}

