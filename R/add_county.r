#' Add county of residence to any `data.frame` with TBdb IDs or MRNs.
#' 
#' This function adds a county of residence variable to any `data.frame`
#' that has a `person_id` column.
#' 
#' @param dat The data.frame to be modified
#' @param id_var The name of the identifier to merge on (`person_id` by default)
#' 
#' @export
#' 
#' 
#' 

add_county <- function(dat, id_var = "person_id") {

    # Determine county of residence
    # For now, I'm going to use the county indicated on the Person table
    plus <- connect_to_tbdbplus()

    all_counties <- sqlQuery(plus, "

        SELECT person_id, county
        FROM Demos_View

    ")

    odbcClose(plus)


    # Merge into the input data.frame
    dat_countied <- merge(x = dat,
                          y = all_counties,
                          by.x = id_var,
                          by.y = "person_id",
                          all.x = TRUE)


    # Return
    dat_countied

}
