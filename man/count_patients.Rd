\name{count_patients}
\alias{count_patients}
\title{Count the total number of patients seen between two dates.}
\usage{
count_patients(start_date, stop_date)
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
This function counts the number of all patients seen
between the given dates; specifically, any individual with
a TST, QFT, encounter, or treatment during the period is
counted.
}
\examples{
# Number of patients seen in the last thirty days
count_patients(start_date = Sys.Date() - 30,
               stop_date = Sys.Date())
}

