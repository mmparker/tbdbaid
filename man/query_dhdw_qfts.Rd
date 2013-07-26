\name{query_dhdw_qfts}
\alias{query_dhdw_qfts}
\title{Query QFT records from the Denver Health Data Warehouse}
\usage{
  query_dhdw_qfts(start_date, end_date, hosp_serv = "TBC")
}
\arguments{
  \item{start_date}{Earliest collection date of QFTs to
  return}

  \item{end_date}{Latest collection date of QFTs to return}

  \item{hosp_serv}{The hospital service code of the QFTs
  you'd like to retrieve (defaults to "TBC")}
}
\value{
  Some QFT records
}
\description{
  This function returns Quantiferon results form the Denver
  Health Data Warehouse. If you don't have access to the
  DHDW, it won't obviously won't work...
}

