read_emdata <- function(file){
   readr::read_csv(file,show_col_types = FALSE, progress=FALSE) %>% 
   tibble::as_tibble() %>%
   dplyr::mutate(Order = 1:dplyr::n()) %>%
   dplyr::select(Order, Type, FreqId, TxId, RxId) %>%
   dplyr::mutate(dplyr::across(Type:RxId, as.integer)) 
}

read_tx <- function(file){
  readr::read_csv(file, show_col_types = FALSE, progress=FALSE) %>% 
  tibble::as_tibble() %>%
  dplyr::mutate(TxId = 1:dplyr::n(), Y=Inline)
}

read_rx <- function(file){
  readr::read_csv(file, show_col_types = FALSE, progress=FALSE) %>% 
  tibble::as_tibble() %>%
  dplyr::mutate(RxId = 1:dplyr::n(), Y=Inline)
}

#' Load the emdata
#'
#' This function loads the emdata
#'
#' @export
load_emdata <- function(example="04Tx13"){

  if(example=="04Tx13"){
    file = system.file("extdata", "04Tx13_emdata.csv", package = "predictMARE")
    return(read_emdata(file))
  }
  else if(example=="Demo"){
    file = system.file("extdata", "Demo_emdata.csv", package = "predictMARE")
    return(read_emdata(file))
  }
}

#' Load the tx position
#'
#' This function loads the tx
#'
#' @export
load_tx <- function(example="04Tx13"){

  if(example=="04Tx13"){
    file = system.file("extdata", "04Tx13_tx.csv", package = "predictMARE")
    return(read_tx(file))
  }
  else if(example=="Demo"){
    file = system.file("extdata", "Demo_tx.csv", package = "predictMARE")
    return(read_tx(file))
  }
}

#' Load the rx position
#'
#' This function loads the rx
#'
#' @export
load_rx <- function(example="04Tx13"){

  if(example=="04Tx13"){
    file = system.file("extdata", "04Tx13_rx.csv", package = "predictMARE")
    return(read_rx(file))
  }
  else if(example=="Demo"){
    file = system.file("extdata", "Demo_rx.csv", package = "predictMARE")
    return(read_rx(file))
  }
}
