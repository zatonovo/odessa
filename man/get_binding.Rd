\name{get_binding}
\alias{get_binding}
\title{Show the raw binding data for the given data package}
\arguments{
  \item{id}{The Odessa ID for the dataset}
}
\value{
  A data.frame containing the binding information
}
\description{
  This function displays the raw binding data for
  examination. Typically it is not needed for casual use of
  the library.
}
\section{Usage}{
  get_binding %::% character : a get_binding(id)
}

\section{Details}{
  A binding is what links a data set to the Odessa
  standard. A data package can have multiple bindings. This
  function shows all the bindings explicitly registered for
  the package.
}
\author{
  Brian Lee Yung Rowe
}

