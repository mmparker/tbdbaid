

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
#' @param end_date Latest collection date of QFTs to return
#' @param odbc Name of the local ODBC connection to the DHDW


start_date <- as.Date("2012-01-01")
end_date <- as.Date("2012-12-31")
hosp_serv <- "TBC"
odbc <- "dhdw64"

query_dhdw_qfts <- function(start_date,
                            end_date,
                            hosp_serv = "TBC",
                            odbc = "dhdw64") {


    require(RODBC)
    require(reshape2)

    # Connect to the DHDW
    dhdw <- odbcConnect(odbc)

    # Pull in the raw QFT records
    # TODO: limit by dates
    # TODO: limit by hospital service
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
             replace = "\\1")

    # Qualitative results
    qfts.raw$result[qfts.raw$result.type %in% "result"] <- 
        gsub(x = qfts.raw$res_txt[qfts.raw$result.type %in% "result"],
             pattern = "^(\\w*).*$", 
             replace = "\\1")


    # Cast wide, excluding duplicate lines
    dhdw.qfts <- dcast(unique(subset(qfts.raw, !is.na(result.type))),
                       mrn + qft_dt ~ result.type, 
                       value.var = "result")


    # Convert nil, tb, mito to numeric
    dhdw.qfts$nil.num <- as.numeric(gsub(x = dhdw.qfts$nil,
                                         pattern = ">",
                                         replace = ""))

    dhdw.qfts$tb.num <- as.numeric(gsub(x = dhdw.qfts$tb,
                                        pattern = ">",
                                        replace = ""))

    dhdw.qfts$mito.num <- as.numeric(gsub(x = dhdw.qfts$mito,
                                          pattern = ">",
                                          replace = ""))


    # Return those QFTs
    dhdw.qfts



}
