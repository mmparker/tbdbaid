\name{query_visits}
\alias{query_visits}
\title{Query visits from TBdb}
\usage{
query_visits(start_date, stop_date = Sys.Date())
}
\arguments{
  \item{start_date}{The earliest visit date to retrieve}

  \item{stop_date}{The latest visit date to retrieve}
}
\description{
This function queries patient visits (face-to-face
encounters and directly-observed drug treatments) from TBdb
for a given period. The results include visit date,
location, and the staff member responsible.
}

