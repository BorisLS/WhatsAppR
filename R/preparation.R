#' Returns a Dataframe that only contains posts with type 'media' oder 'message'
#'
#' @param chat Chat history that was imported with function wapp_import
#' @param type String: 'media' for returning only media; 'message' for returning only pure messages without media
#' @return Dataframe only with chats from the type 'media' or 'message'
#' @examples
#' friends <- wapp_import("Data/WhatsApp Chat mit Freunde Uni.txt", language = "de", techuser = FALSE)
#' @import dplyr
#' @export
wapp_divide <- function(chat, type){
  if(type == "media"){
    data <- chat %>%
            filter(type == "media")
  }else if(type == "message"){
    data <- chat %>%
            filter(type == "message")
  }else{
    data <- chat
  }
  return(data)
}


#' Transformation of Time Values: Adds Column time.intervall
#'
#' @param chat Chat history that was imported with function wapp_import
#' @param intervall hour or quartar
#' @return Adds
#' @examples
#' test
#' @import dplyr
#' @export
wapp_prep_time <- function(chat, intervall){

  if(intervall == "hour"){
    inter.time <- seq(0,24,1)
  } else if(intervall == "quarter"){
    inter.time <- seq(0,24,3)
  }
  l.i.t <- length(inter.time)

  inter.label <- paste0(inter.time[1:(l.i.t-1)], "-", inter.time[2:(l.i.t)])
  time.hour <- as.integer(stringr::str_sub(chat$time, 1, 2))

  chat$time.intervall <- cut(time.hour, inter.time,
                             labels=inter.label, right=FALSE)
  return(chat)
}
