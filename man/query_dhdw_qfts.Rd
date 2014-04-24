\name{query_dhdw_qfts}
\alias{query_dhdw_qfts}
\title{Query QFT records from the Denver Health Data Warehouse}
\usage{
query_dhdw_qfts(start_date, stop_date = Sys.Date(), hosp_svc)
}
\arguments{
  \item{start_date}{Earliest collection date of QFTs to
  return}

  \item{stop_date}{Latest collection date of QFTs to return
  (defaults to the current date)}

  \item{hosp_serv}{The hospital service code of the QFTs
  you'd like to retrieve. For example, to return only QFTs
  from the TB and ID Clinics, set \code{hosp_svc = c("TBC",
  "IDC")}. All QFTs are returned by default.}
}
\value{
Some QFT records
}
\description{
This function returns Quantiferon results form the Denver
Health Data Warehouse. If you don't have access to the
DHDW, it won't obviously won't work...
}

