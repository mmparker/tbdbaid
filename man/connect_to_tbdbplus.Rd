\name{connect_to_tbdbplus}
\alias{connect_to_tbdbplus}
\title{Create an architecture-appropriate connection to TBdbPlus.}
\usage{
  connect_to_tbdbplus()
}
\description{
  This functions connects to the correct set of ODBC
  drivers for TBdbPlus, regardless of which architecture R
  is running on.
}
\examples{
plus <- connect_to_tbdbplus()
}
\keyword{database,}
\keyword{odbc}

