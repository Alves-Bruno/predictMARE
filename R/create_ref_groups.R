#' Create the refinement groups 
#'
#' This function simulates the MARE2DEM's 
#' refinement groups creation 
#'
#' @export
create_ref_groups <- function(emdata, config){

    n_tx<-max(emdata$TxId)
    n_rx<-max(emdata$RxId)
    n_fq<-max(emdata$FreqId)

    gvalues <- as.integer(stringr::str_split(config, "-")[[1]])
    group_tx<- gvalues[1]
    group_rx<- gvalues[2]
    group_fq<- gvalues[3]

    df.tx <- tibble::tibble(
        TxFrom=seq(1, n_tx, group_tx),
        TxTo=TxFrom+group_tx-1
    )

    df.rx <- tibble::tibble(
        RxFrom=seq(1, n_rx, group_rx),
        RxTo=RxFrom+group_rx-1
    )

    df.fq <- tibble::tibble(
        FqFrom=seq(1, n_fq, group_fq),
        FqTo=FqFrom+group_fq-1
    )

    tidyr::crossing(df.tx, df.rx, df.fq) %>%
        dplyr::mutate(Pre.RefGroup = 1:dplyr::n()) %>%
        setDT() -> dt.pre_groups

    emdata %>% setDT() -> dt.emdata

    dt.pre_groups[
        dt.emdata,
        on = .(FqFrom <= FreqId, FqTo >= FreqId,
               TxFrom <= TxId,   TxTo >= TxId,
               RxFrom <= RxId,   RxTo >= RxId)
    ] %>%
        dplyr::filter(!is.na(Pre.RefGroup)) -> temp 
         temp %>% 
         dplyr::group_by(Pre.RefGroup) %>% 
         dplyr::summarize(COUNT = dplyr::n(), .groups="drop") %>%
         dplyr::arrange(Pre.RefGroup) %>%
         dplyr::mutate(RefGroup = 1:dplyr::n()) %>% # Renumber since some pre.refgroups dropped
         dplyr::select(Pre.RefGroup, RefGroup, COUNT) -> temp.2

         temp.2 %>% 
           dplyr::left_join(
              temp %>% 
                #filter(Type==3) %>%
                dplyr::select(Pre.RefGroup, Tx=TxFrom, Rx=RxFrom, Fq=FqFrom),
                by = "Pre.RefGroup"
           ) %>% 
           dplyr::mutate(COUNT = COUNT / 2) %>%
           dplyr::select(-Pre.RefGroup) %>%
           dplyr::group_by(RefGroup, COUNT, Tx, Rx, Fq) %>%
           dplyr::distinct() %>% dplyr::ungroup() -> data

        rm(temp)
        return(data)
}
