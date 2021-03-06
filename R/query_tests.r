



#' Query TSTs, QFTs, and CXRs from TBdb.
#' 
#' This function provides a standardized method for querying 
#' common diagnostics - TSTs, QFTs, and CXRs
#' including date and result.
#' 
#' @param start_date The first Date on which to count patients
#' @param stop_date The last Date on which to count patients
#' 
#' @return The number of patients seen in the indicated period.
#' 
#' @export
#' 
#' @examples
#' # Number of patients seen in the last thirty days
#' count_patients(start_date = Sys.Date() - 30,
#'                stop_date = Sys.Date())
#' 


query_tests <- function(start_date, stop_date = Sys.Date()) {


    # Initialize these variables to prevent global var warnings from R CMD check
    test_date <- NULL

    plus <- connect_to_tbdbplus()

    tsts <- sqlQuery(plus, paste(
        "SELECT DISTINCT person_id, 
                date_given AS test_date,
                result, 
                tst_read_by,
                reader_affiliation,
                pt_agency,
                project_type
         FROM TST_View
         WHERE external_clinic = ''
            AND (reader_affiliation = 'Denver Metro TB Clinic'
                OR (pt_agency = 'Denver Public Health - Metro TB Clinic'
                    AND tst_read_by IS NULL))
             AND date_given BETWEEN #",
             start_date, 
             "# AND #",
             stop_date,
             "#",
        sep = "")
    )




    qfts <- sqlQuery(plus, "
        SELECT DISTINCT person_id,
               collection_date,
               result,
               project_type
        FROM QFT_View
        WHERE lab = 'Denver Public Health'
            AND collection_date IS NOT NULL

    ")


    cxrs <- sqlQuery(plus, paste(
        "SELECT DISTINCT person_id,
                cxr_date_taken AS test_date,
                abnormal AS result,
                NULL AS project_type
         FROM CXR_View
         WHERE cxr_lab_affiliation = 'Denver Metro TB Clinic'
             AND cxr_date_taken BETWEEN #",
             start_date, 
             "# AND #",
             stop_date,
             "#",
        sep = "")
    )



    odbcClose(plus)


    # Convert QFT collection dates into proper Dates
    qfts$test_date <- as.Date(qfts$collection_date, format = "%m/%d/%Y")


    # Subset QFTs by date
    qfts_datelim <- qfts[qfts$test_date >= start_date &
                         qfts$test_date <= stop_date, ]


    # Melt it into a single dataset
    tsts$test <- "TST"
    qfts_datelim$test <- "QFT"
    cxrs$test <- "CXR"


    # Some people do have more than one TST on the same day, but I think that's
    # always just a clerical error. Makes reshaping a pain, though.
    # Adding a simple throw-away sequence ID makes it possible to reshape.
    # This will result in some false counts, but few relative to the total
    # number of TSTs.
    tsts$seq_id <- seq_along(tsts$person_id)
    qfts_datelim$seq_id <- seq_along(qfts_datelim$person_id)
    cxrs$seq_id <- seq_along(cxrs$person_id)

    # Combine into a single data.frame for ease of plotting
    tests <- rbind(

               dcast(melt(tsts, 
                          id.vars = c("person_id", "test_date", "seq_id"),
                          measure.vars = c("test", "project_type", "result")),
                     person_id + test_date + seq_id ~ variable),

               dcast(melt(qfts_datelim, 
                          id.vars = c("person_id", "test_date", "seq_id"),
                          measure.vars = c("test", "project_type", "result")),
                     person_id + test_date + seq_id ~ variable),

               dcast(melt(cxrs, 
                          id.vars = c("person_id", "test_date", "seq_id"),
                          measure.vars = c("test", "project_type", "result")),
                     person_id + test_date + seq_id ~ variable)

    )


    # Convert test dates to Dates
    tests$test_date <- as.Date(tests$test_date)


    # Add month-, quarter-, and year-of-visit variables for ease of aggregation
    tests$test_mon <- as.character(
        format(tests$test_date, format = "%m")
    )

    tests$test_yr <- as.character(
        format(tests$test_date, format = "%Y")
    )

    tests$test_qtr <- (as.numeric(tests$test_mon) + 2) %/% 3


    tests

}
