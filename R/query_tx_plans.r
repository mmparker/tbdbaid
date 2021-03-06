


#' Query all treatment plans from TBdb
#' 
#' This function provides a standardized method for querying 
#' all treatment plan statuses (including multiples per patient) from TBdb,
#' including plan type, plan author, start date, end date (if completed),
#' reason stopped, plan author, and number of treatments completed
#' 
#' @param start_date The start date of the earliest treatment plans to query
#' @param stop_date The start date of the latest treatment plans to query
#' 
#' @export
#' 
#' @examples
#' # Query all treatment plans started in the last week
#' query_tx_plans(start_date = Sys.Date() - 7,
#'                stop_date = Sys.Date())
#' 



query_tx_plans <- function(start_date,
                           stop_date = Sys.Date()) {

    plus <- connect_to_tbdbplus()

    plans <- sqlQuery(plus, paste(
        "SELECT person_id,
                treat_plan,
                treat_plan_type,
                ltbi_drug,
                plan_author,
                author_affiliation,
                treat_plan_date,
                plan_status,
                reason_stopped,
                completion_status,
                treat_plan_end,
                n_tx_completed,
                days_to_end
         FROM Tx_Plan_View
         WHERE author_affiliation = 'Denver Metro TB Clinic'
            AND treat_plan_date  >= #",
            start_date, 
            "#",
            "ORDER BY person_id, treat_plan_date DESC",
        sep = "")
    )

    odbcClose(plus)


    # Convert dates to Dates
    plans$treat_plan_date <- as.Date(plans$treat_plan_date)
    plans$treat_plan_end <- as.Date(plans$treat_plan_end)


    # Add month-, quarter-, and year-of-plan variables for ease of aggregation
    plans$plan_mon <- as.character(
        format(plans$treat_plan_date, format = "%m")
    )

    plans$plan_yr <- as.character(
        format(plans$treat_plan_date, format = "%Y")
    )

    plans$plan_qtr <- (as.numeric(plans$plan_mon) + 2) %/% 3


    # Add month-, quarter-, and year-of-plan variables for ease of aggregation
    plans$plan_mon <- as.character(
        format(plans$treat_plan_date, format = "%m")
    )

    plans$plan_yr <- as.character(
        format(plans$treat_plan_date, format = "%Y")
    )

    plans$plan_qtr <- (as.numeric(plans$plan_mon) + 2) %/% 3


    # Return all plans started on or before the stop date
    plans[as.Date(plans$treat_plan_date) <= stop_date, ]


}
