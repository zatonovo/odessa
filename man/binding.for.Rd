\name{binding.for}
\alias{binding.for}
\title{Generate the index associated for the given field name. These
are the actual values that will be used to join with another
dataset.}
\arguments{
  \item{x}{An Odessa ID for a data package}

  \item{field}{Find compatible bindings for this field}
}
\value{
  A character vector of compatible binding names
}
\description{
  This is primarily used internally, but it can be useful
  for debugging to see what values are being generated.
}
\section{Usage}{
  binding.for(x, field)
}

\section{Details}{
  A binding is the essence of what Odessa provides.
  Bindings are generated indices based on construction
  rules and graph-based inference rules.
}
\examples{
\dontrun{
a <- fetch('tutorial-datetime-1')
binding.for(a,'date')
}
}
\author{
  Brian Lee Yung Rowe
}

