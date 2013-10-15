\name{package_list}
\alias{package_list}
\title{Get the list of available packages}
\arguments{
  \item{tag}{A regular expression for searching tags}

  \item{group}{A regular expression for searching groups}
}
\value{
  A character vector with matching results
}
\description{
  Searches Odessa for packages either using tag keywords
  along with a specified group.
}
\note{
  No search capability is currently available.
}
\section{Usage}{
  package_list(tag='', group='')
}

\section{Details}{
  As Odessa is a shared repository, there needs to be a
  convenient way to find data on the platform. This
  function is an in-session mechanism to search Odessa's
  online indices.
}
\author{
  Brian Lee Yung Rowe
}
\seealso{
  \code{\link{package_list}}
}

