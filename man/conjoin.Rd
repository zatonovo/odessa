\name{conjoin}
\alias{conjoin}
\title{Conjoin two data.frames together}
\arguments{
  \item{a}{An Odessa data.frame}

  \item{b}{An Odessa data.frame}
}
\value{
  A new data.frame conjoined on a common index
}
\description{
  Take two Odessa data packages and join them together
  given the common field. If no field is specified, then
  use the greatest common factor based on the graph of each
  index.
}
\section{Usage}{
  conjoin(a,b, field, ...)
}

\section{Details}{
  The power of Odessa is in being able to join datasets
  together without a lot of ceremony. Data that has been
  fetched via Odessa can be joined via their bindings. The
  ideal situation is that compatible bindings are inferred
  from the metadata and master graphs. Currently you have
  to specify the binding.
}
\examples{
\dontrun{
df1 <- fetch('vbts-zqt4')
odessa.id(df1) <- 'vbts-zqt4.binding.csv'
df2 <- fetch('vz8e-347q')
odessa.id(df2) <- 'vz8e-347q.binding.csv'
conjoin(df1,df2, 'year')
}
}
\author{
  Brian Lee Yung Rowe
}

