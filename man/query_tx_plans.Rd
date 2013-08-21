\name{query_tx_plans}
\alias{query_tx_plans}
\title{Query treatment plan statuses from TBdb}
\usage{
  query_tx_plans(start_date, stop_date = Sys.Date())
}
\arguments{
  \item{start_date}{The start date of the earliest
  treatment plans to query}

  \item{stop_date}{The start date of the latest treatment
  plans to query}
}
\description{
  This function provides a standardized method for querying
  treatment plan status including plan type, plan author,
  start date, end date (if completed), reason stopped, plan
  author, and number of treatments completed
}
\examples{
# Query all treatment plans started in the last week
query_tx_plans(start_date = Sys.Date() - 7,
               stop_date = Sys.Date())
}

