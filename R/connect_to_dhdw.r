



#' Connect to the right DH Data Warehouse ODBC drivers, regardless of which
#' version of R is running
#'
#' This functions connects to the correct set of ODBC drivers for the DHDW,
#' regardless of which architecture R is running on.
#'
#' @keywords database, odbc
#' @export
#' 
#' @examples
#' plus <- connect_to_dhdw()

connect_to_dhdw <- function() {

    arch <- R.Version()$arch

    if(arch %in% "x86_64") { 
        return(odbcConnect("dhdw64"))
    } else {
        return(odbcConnect("dhdw32"))
    }

}
