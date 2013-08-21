

#' Query QFT records from the Denver Health Data Warehouse
#' 
#' This function returns Quantiferon results form the Denver Health Data
#' Warehouse. If you don't have access to the DHDW, it won't obviously won't
#' work...
#' 
#' @return
#' Some QFT records
#' 
#' @param start_date Earliest collection date of QFTs to return
#' @param stop_date Latest collection date of QFTs to return
#' @param hosp_serv The hospital service code of the QFTs you'd like to
#'        retrieve (defaults to "TBC")
#' 
#' @export
#' 
#' 


query_dhdw_qfts <- function(start_date,
                            stop_date,
                            hosp_serv = "TBC") {

    # Initialize these variables to prevent global var warnings from R CMD check
    result.type <- NULL


    # Connect to the DHDW
    dhdw <- connect_to_dhdw()

    # Pull in the raw QFT records
    qfts.raw <- sqlQuery(dhdw, "

        SELECT med_rec_no AS mrn, 
               obsv_dtime AS qft_dt, 
               obsv_rslt_text AS res_txt
        FROM lcr_ods.dbo.LCR_observation_v WITH(nolock)
        WHERE obsv_term_no IN (100693,100694)
        ORDER BY med_rec_no, obsv_dtime

    ", stringsAsFactors = FALSE)


    odbcClose(dhdw)



    # Identify which result is in which records
    qfts.raw$result.type <- NA

    qfts.raw$result.type[grep(qfts.raw$res_txt, pattern = "^Nil:")] <- "nil"
    qfts.raw$result.type[grep(qfts.raw$res_txt, pattern = "^TB Ag:")] <- "tb"
    qfts.raw$result.type[grep(qfts.raw$res_txt, pattern = "^Mito:")] <- "mito"
    qfts.raw$result.type[grep(qfts.raw$res_txt,
        pattern = "^(Negative|Positive|Indeterminate)")] <- "result"

    table(qfts.raw$result.type, exclude = NULL)
    sort(unique(qfts.raw$res_txt[is.na(qfts.raw$result.type)]))



    # Pull out the values
    qfts.raw$result <- NA

    # Nil, TB, Mito quantitative result
    qfts.raw$result[qfts.raw$result.type %in% c("nil", "tb", "mito")] <- 
        gsub(x = qfts.raw$res_txt[qfts.raw$result.type %in% 
                 c("nil", "tb", "mito")],
             pattern = "^.*:\\s?(\\S*).*", 
             replacement = "\\1")

    # Qualitative results
    qfts.raw$result[qfts.raw$result.type %in% "result"] <- 
        gsub(x = qfts.raw$res_txt[qfts.raw$result.type %in% "result"],
             pattern = "^(\\w*).*$", 
             replacement = "\\1")


    # Cast wide, excluding duplicate lines
    dhdw.qfts <- dcast(unique(subset(qfts.raw, !is.na(result.type))),
                       mrn + qft_dt ~ result.type, 
                       value.var = "result")


    # Convert nil, tb, mito to numeric
    dhdw.qfts$nil.num <- as.numeric(gsub(x = dhdw.qfts$nil,
                                         pattern = ">",
                                         replacement = ""))

    dhdw.qfts$tb.num <- as.numeric(gsub(x = dhdw.qfts$tb,
                                        pattern = ">",
                                        replacement = ""))

    dhdw.qfts$mito.num <- as.numeric(gsub(x = dhdw.qfts$mito,
                                          pattern = ">",
                                          replacement = ""))


    # Return those QFTs
    dhdw.qfts



}
