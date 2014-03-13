\name{add_county}
\alias{add_county}
\title{Add county of residence to any `data.frame` with TBdb IDs or MRNs.}
\usage{
add_county(dat, id_var = "person_id")
}
\arguments{
  \item{dat}{The data.frame to be modified}

  \item{id_var}{The name of the identifier to merge on
  (`person_id` by default)}
}
\description{
This function adds a county of residence variable to any
`data.frame` that has a `person_id` column.
}

