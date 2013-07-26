#' Count the total number of patients seen between two dates.
#' 
#' This function counts the number of all patients seen between the given dates;
#' specifically, any individual with a TST, QFT, encounter, or treatment during
#' the period is counted.
#' 
#' @param start_date The first Date on which to count patients
#' @param end_date The last Date on which to count patients
#' 
#' @return The number of patients seen in the indicated period.
#' 
#' @export
#' 
#' @examples
#' # Number of patients seen in the last thirty days
#' count_patients(start_date = Sys.Date() - 30,
#'                end_date = Sys.Date())
#' 



count_patients <- function(start_date, end_date) {

    plus <- connect_to_tbdbplus()

    # Get the IDs of every person with a TST, QFT, encounter, or treatment
    # during the report period
    tsts <- sqlQuery(plus, paste("

        SELECT person_id 
        FROM TST
        WHERE date_given BETWEEN #",
        start_date,
        "# AND #",
        end_date,
        "#",
        sep = "")

    )


    qfts_all <- sqlQuery(plus, "

        SELECT person_id, collection_date 
        FROM Quantiferon
        
    ")


    encounters <- sqlQuery(plus, paste("
                                       
        SELECT person_id 
        FROM Medical_Eval
        WHERE eval_type IN (1, 4)
        AND eval_date BETWEEN #",
        start_date,
        "# AND #",
        end_date,
        "#",
        sep = "")

    )


    treatments <- sqlQuery(plus, paste("
                                       
        SELECT person_id 
        FROM Drug_Treatment
        WHERE completed = 'Completed'
        AND treatment_date BETWEEN #",
        start_date,
        "# AND #",
        end_date,
        "#",
        sep = "")

    )



    odbcClose(plus)



    # Convert the QFT dates to Dates
    qfts_all$c_date <- as.Date(qfts_all$collection_date, "%m/%d/%Y")

    # Subset the QFTs to the report period
    qfts <- qfts_all[qfts_all$c_date >= as.Date(start_date) & 
                     qfts_all$c_date <= as.Date(end_date), ]


    # Count the unique patients
    all_ids <- c(tsts$person_id, 
                 qfts$person_id, 
                 encounters$person_id, 
                 treatments$person_id)


    length(unique(all_ids))


}
