

# This function provides a standardized method for querying 
# visits (face-to-face encounters and drug treatments)
# including visit date, location, and staff responsible.
# It will probably be expanded to include demographic details later, if
# needed for reporting



query_visits <- function(start_date,
                         stop_date = Sys.Date(),
                         odbc = "tbdbplus64") {

    # TODO: argument validation

    require(RODBC)

    dbconnect <- odbcConnect(odbc)

    encounters <- sqlQuery(dbconnect, paste(
        "SELECT person_id, 
                eval_date AS visit_date,
                visit_location, 
                staff_responsible AS staff_enc
         FROM Medical_Eval
         WHERE eval_type in (1, 4)
             AND staff_responsible IN (
                 SELECT staff_name
                 FROM Def_staff
                 WHERE affiliation = 'Denver Metro TB Clinic'
                 )
             AND eval_date BETWEEN #",
             start_date, 
             "# AND #",
             stop_date,
             "#",
        sep = "")
    )



    dots <- sqlQuery(dbconnect, paste(
        "SELECT person_id, 
                treatment_date AS visit_date, 
                dispense_type,
                staff AS staff_tx
         FROM Drug_Treatment
         WHERE dispense_type IN ('DOT', 'DOPT')
             AND completed = 'Completed'
             AND staff IN (
                 SELECT staff_name
                 FROM Def_staff
                 WHERE affiliation = 'Denver Metro TB Clinic'
                 )
             AND treatment_date BETWEEN #",
             start_date, 
             "# AND #",
             stop_date,
             "#",
        sep = "")
    )


#    pickups <- sqlQuery(dbconnect, paste(
#        "SELECT person_id, 
#                treatment_date AS visit_date, 
#                dispense_type, 
#                staff AS staff_tx
#         FROM Drug_Treatment
#         WHERE dispense_type IN ('Pickup')
#             AND completed = 'Completed'
#             AND staff IN (
#                 SELECT staff_name
#                 FROM Def_staff
#                 WHERE affiliation = 'Denver Metro TB Clinic'
#                 )
#             AND treatment_date BETWEEN #",
#             start_date, 
#             "# AND #",
#             stop_date,
#             "#",
#        sep = "")
#    )



    odbcClose(dbconnect)


    # For reporting, we generally want just one visit per day, even if a person
    # has both an encounter and a treatment - so merge them
    visits <- merge(x = encounters,
                    y = dots,
                    by = c("person_id", "visit_date"),
                    all = TRUE)


    # Recode the visit locations
    visits$location <- NA 

    # For my purposes, visit_location = 0 is the same as visit_location = NA
    visits$visit_location[visits$visit_location %in% 0] <- NA

    visits$location[visits$visit_location %in% 1] <- "Clinic"
    visits$location[visits$visit_location %in% 2] <- "Outreach"
    visits$location[visits$visit_location %in% 3] <- "Outreach"
    visits$location[visits$visit_location %in% 4] <- "Outreach"
    visits$location[visits$visit_location %in% 9] <- "Other"

    # If there isn't a visit location but there is a DOT/DOPT, it was
    # probably outreach
    visits$location[is.na(visits$visit_location) &
                    visits$dispense_type %in% c("DOT", "DOPT")] <- "Outreach"


    # Add month-, quarter-, and year-of-visit variables for ease of aggregation
    visits$visit_mon <- as.character(
        format(visits$visit_date, format = "%m")
    )

    visits$visit_yr <- as.character(
        format(visits$visit_date, format = "%Y")
    )

    visits$visit_qtr <- (as.numeric(visits$visit_mon) + 2) %/% 3


    visits

}
