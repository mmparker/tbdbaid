


# This function provides a standardized method for querying 
# treatment plan status
# including plan type, start date, status, end date (if completed),




# Strings ain't factors
options(stringsAsFactors = FALSE)



query_tx_plans <- function(start_date,
                           stop_date = Sys.Date(),
                           odbc = "tbdbplus64") {

    # TODO: argument validation

    require(RODBC)

    dbconnect <- odbcConnect(odbc)

    plans <- sqlQuery(dbconnect, paste(
        "SELECT person_id,
                treat_plan,
                treat_plan_type,
                plan_author,
                author_affiliation,
                treat_plan_date,
                plan_status,
                reason_stopped,
                treat_plan_end,
                n_tx_completed,
                days_to_end
         FROM Tx_Plan_View
         WHERE treat_plan_date BETWEEN #", 
            start_date, 
            "# AND #", 
            stop_date, 
            "#",
        sep = "")
    )

    odbcClose(dbconnect)


    # Add month-, quarter-, and year-of-plan variables for ease of aggregation
    plans$plan_mon <- as.character(
        format(plans$treat_plan_date, format = "%m")
    )

    plans$plan_yr <- as.character(
        format(plans$treat_plan_date, format = "%Y")
    )

    plans$plan_qtr <- (as.numeric(plans$plan_mon) + 2) %/% 3


    plans

}
