

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
#' @param stop_date Latest collection date of QFTs to return (defaults to the current date)
#' @param hosp_serv The hospital service code of the QFTs you'd like to
#'        retrieve. For example, to return only QFTs from the TB and ID Clinics, set \code{hosp_svc = c("TBC", "IDC")}. All QFTs are returned by default. 
#' 
#' @export
#' 
#' 


query_dhdw_qfts <- function(start_date,
                            stop_date = Sys.Date(),
                            hosp_svc) {


    # Check for missing start_date
    if(missing(start_date)) { stop('You must specify a start date for the query (e.g., as.Date("2010-01-01") ).') }

    # Initialize these variables to prevent global var warnings from R CMD check
    result.type <- NULL


    # Connect to the DHDW
    dhdw <- connect_to_dhdw()

    # Pull in the raw QFT records
    qfts.raw <- sqlQuery(dhdw, paste0("

            SELECT lcr.med_rec_no AS mrn, 
                   LTRIM(RTRIM(vst.hosp_svc)) AS hosp_svc,
                   lcr.obsv_trans_id AS qft_id,
                   lcr.obsv_dtime AS qft_dt, 
                   lcr.obsv_rslt_text AS res_txt
            FROM lcr_ods.dbo.LCR_observation_v lcr
            LEFT OUTER JOIN dhdcdtw.smsmir.mir_vst vst
            ON CAST(lcr.pt_id AS INTEGER) = CAST(vst.pt_id AS INTEGER)
            WHERE lcr.obsv_term_no IN (100693,100694)
                AND CONVERT(date, lcr.obsv_dtime) BETWEEN '",
            start_date,
            "' AND '",
            stop_date,
            "'",
            "ORDER BY lcr.med_rec_no, lcr.obsv_dtime"

            ),

        stringsAsFactors = FALSE
    )

    head(qfts.raw)


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
                       mrn + qft_id + qft_dt + hosp_svc ~ result.type, 
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


    # If a particular service or services have been requsted,
    # return just those QFTs; otherwise, return all
    if(missing(hosp_svc)) {
        return(dhdw.qfts) } else { 
            return(dhdw.qfts[dhdw.qfts$hosp_svc %in% hosp_svc, ]) }



}
