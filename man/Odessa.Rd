\name{Odessa}
\alias{Odessa}
\title{A type that represents the Odessa binding information for a package}
\arguments{
  \item{id}{The Odessa ID for the dataset}

  \item{path}{A local file path that ends in the id of the
  dataset}

  \item{fn}{A function used to clean the data}
}
\value{
  A typed data.frame
}
\description{
  Every dataset in Odessa is a package containing the
  actual dataset and a binding file containing metadata.
}
\section{Usage}{
  Odessa(id, fn=clean.format)
}

\section{Details}{
  An end user generally doesn't need to work directly with
  an Odessa object.
}
\author{
  Brian Lee Yung Rowe
}

