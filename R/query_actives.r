
# This function provides a standardized method for querying 
# reported active cases
# along with the TBdb IDs, MRNs, names, DOBs, county of report, 
# treatment start date, and official CDC report date.
# It will probably be expanded to include more/most of the RVCT details.


query_actives <- function(start_date, 
                          stop_date = Sys.Date(),
                          odbc = "tbdbplus64") {

    # TODO: argument validation

    require(RODBC)

    dbconnect <- odbcConnect(odbc)

    actives <- sqlQuery(dbconnect, "

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

    odbcClose(dbconnect)


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
                              date_id <= stop_date)

    queried_actives

}


