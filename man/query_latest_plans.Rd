\name{query_latest_plans}
\alias{query_latest_plans}
\title{Query patients' latest treatment plans from TBdb}
\usage{
query_latest_plans(start_date, stop_date = Sys.Date())
}
\arguments{
  \item{start_date}{The start date of the earliest
  treatment plans to query}

  \item{stop_date}{The start date of the latest treatment
  plans to query}
}
\description{
This function provides a standardized method for querying
each patient's latest treatment plan status from TBdb,
including plan type, plan author, start date, end date (if
completed), reason stopped, plan author, and number of
treatments completed
}
\examples{
# Query all treatment plans started in the last week
query_latest_plans(start_date = Sys.Date() - 7,
                   stop_date = Sys.Date())
}

