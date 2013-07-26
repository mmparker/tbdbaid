\name{connect_to_dhdw}
\alias{connect_to_dhdw}
\title{Connect to the right DH Data Warehouse ODBC drivers, regardless of which
version of R is running}
\usage{
  connect_to_dhdw()
}
\description{
  This functions connects to the correct set of ODBC
  drivers for the DHDW, regardless of which architecture R
  is running on.
}
\examples{
plus <- connect_to_dhdw()
}
\keyword{database,}
\keyword{odbc}

