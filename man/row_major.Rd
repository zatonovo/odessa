\name{row_major}
\alias{row_major}
\title{Convert a column major data.frame into a row-major compatible list structure}
\arguments{
  \item{data}{The object to transform}

  \item{names}{Whether to assign any names to the resulting
  rows. The default is NULL, which means that the
  conversion to JSON will be an array. If this is non null
  then the base structure will be a hash instead of an
  array.}
}
\description{
  Standard encoding of a data.frame to a JSON structure is
  based on the column major conventions of R whereas JSON
  table structures are effectively row-major.
}
\examples{
df <- data.frame(a=1:4, b=2:5, c=3:6)
RJSONIO::toJSON(row_major(df))
}

