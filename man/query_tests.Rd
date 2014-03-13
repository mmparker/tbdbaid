\name{query_tests}
\alias{query_tests}
\title{Query TSTs, QFTs, and CXRs from TBdb.}
\usage{
query_tests(start_date, stop_date = Sys.Date())
}
\arguments{
  \item{start_date}{The first Date on which to count
  patients}

  \item{stop_date}{The last Date on which to count
  patients}
}
\value{
The number of patients seen in the indicated period.
}
\description{
This function provides a standardized method for querying
common diagnostics - TSTs, QFTs, and CXRs including date
and result.
}
\examples{
# Number of patients seen in the last thirty days
count_patients(start_date = Sys.Date() - 30,
               stop_date = Sys.Date())
}

