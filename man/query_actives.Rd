\name{query_actives}
\alias{query_actives}
\title{Get basic info on active cases identified during a given period.}
\usage{
  query_actives(start_date, stop_date = Sys.Date())
}
\arguments{
  \item{start_date}{The earliest identification date to
  retrieve}

  \item{stop_date}{The latest identification date to
  retrieve}
}
\description{
  This function queries basic information on all active
  cases identified during a given period. "Identified" here
  means specifically the earlier of a cases treatment start
  date and report date - so cases that have a delayed
  report will still be given a date reasonably close to
  their initial identification, while cases that never
  start treatment (usually, those who die shortly before or
  after diagnosis) will still have an approximate date.
}

