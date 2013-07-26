


# Connect to the right TBdbPlus ODBC drivers, regardless of which
# version of R is running

#' Create an architecture-appropriate connection to TBdbPlus.
#'
#' This functions connects to the correct set of ODBC drivers for TBdbPlus,
#' regardless of which architecture R is running on.
#'
#' @keywords database, odbc
#' @export
#' 
#' @examples
#' plus <- connect_to_tbdbplus()

connect_to_tbdbplus <- function() {

    arch <- R.Version()$arch

    if(arch %in% "x86_64") { 
        return(odbcConnect("tbdbplus64"))
    } else {
        return(odbcConnect("tbdbplus32"))
    }

}
