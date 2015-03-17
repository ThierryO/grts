\name{GRTS.ODBC}
\alias{GRTS.ODBC}
\title{Perform a GRTS sampling on a database table via ODBC}
\usage{
  GRTS.ODBC(channel, tablename, grts.vars, cellsize,
    samplesize, availableLevels = 20, reset = FALSE)
}
\arguments{
  \item{channel}{an ODBC channel to the database}

  \item{tablename}{the name of the table in the database}

  \item{grts.vars}{the name of the variabeles which define
  the GRTS sampling space. Typically an X and Y variable.
  However the only limitations are that all variables are
  numeric and the number of variables must be at least
  one.}

  \item{cellsize}{the size of the smallest GRTS cell for
  each dimension.}

  \item{samplesize}{the required sample size}

  \item{availableLevels}{the number of available GRTS
  levels in the database. These must be named L01, L02, ...
  Currently available levels is limited to 99}

  \item{reset}{logical value. If FALSE the current GRTS
  randomisation is completed. If TRUE a new GRTS
  randomisation will start.}
}
\description{
  Perform a GRTS sampling on a database table via ODBC
}

