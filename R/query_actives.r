#' Get basic info on active cases identified during a given period.
#' 
#' This function queries basic information on all active cases identified
#' during a given period. "Identified" here means specifically the earlier of
#' a cases treatment start date and report date - so cases that have a delayed
#' report will still be given a date reasonably close to their initial 
#' identification, while cases that never start treatment (usually, those who
#' die shortly before or after diagnosis) will still have an approximate date.
#' 
#' @param start_date The earliest identification date to retrieve
#' @param end_date The latest identification date to retrieve
#' 
#' @export
#' 
#' 
#' 


query_actives <- function(start_date, end_date = Sys.Date()) {

    plus <- connect_to_tbdbplus()

    actives <- sqlQuery(plus, "

        SELECT mrn,
               person_id, 
               last_name,
               first_name,
               date_of_birth,
               date_counted,
               tx_start_date,
               report_county,
               metro_case
        FROM Actives_List

    ")

    odbcClose(plus)


    # Convert the dates to Dates - POSIXct is overly complicated here
    actives$date_of_birth <- as.Date(actives$date_of_birth)
    actives$date_counted <- as.Date(actives$date_counted)
    actives$tx_start_date <- as.Date(actives$tx_start_date)


    # The minimum of date_counted and tx_start_date is generally a good
    # guess at when we identified a case. Count dates can be months after
    # the person starts treatment, but dead people can't take pills...
    actives$date_id <- with(actives, 
            pmin(date_counted, tx_start_date, na.rm = TRUE)
    )


    # Add month, quarter, and year ID'd variables as a 
    # convenience for aggregation later
    actives$mon_id <- as.character(
        format(actives$date_id, format = "%m")
    )

    actives$yr_id <- as.character(
        format(actives$date_id, format = "%Y")
    )

    actives$qtr_id <- (as.numeric(actives$mon_id) + 2) %/% 3



    # Subset to active cases in the requested date range
    queried_actives <- subset(actives, 
                              date_id >= start_date &
                              date_id <= end_date)

    queried_actives

}


