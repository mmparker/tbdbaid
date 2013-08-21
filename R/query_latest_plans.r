



#' Query patients' latest treatment plans from TBdb
#' 
#' This function provides a standardized method for querying 
#' each patient's latest treatment plan status from TBdb,
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
#' query_latest_plans(start_date = Sys.Date() - 7,
#'                    stop_date = Sys.Date())
#' 



query_latest_plans <- function(start_date,
                               stop_date = Sys.Date()) {

    # Query all treatment plans
    plans <- query_tx_plans(start_date = start_date, 
                            stop_date = stop_date)

    
    # Select the most recent plan for each person
    sorted_plans <- plans[order(plans$person_id, plans$treat_plan_date,
                                decreasing = TRUE), ]

    latest_plan <- sorted_plans[!duplicated(sorted_plans$person_id), ]



    # Add month-, quarter-, and year-of-plan variables for ease of aggregation
    latest_plan$plan_mon <- as.character(
        format(latest_plan$treat_plan_date, format = "%m")
    )

    latest_plan$plan_yr <- as.character(
        format(latest_plan$treat_plan_date, format = "%Y")
    )

    latest_plan$plan_qtr <- (as.numeric(latest_plan$plan_mon) + 2) %/% 3


    # Return each individual's latest plan, if it started on or before
    # the stop date
    latest_plan[as.Date(latest_plan$treat_plan_date) <= stop_date, ]


}
