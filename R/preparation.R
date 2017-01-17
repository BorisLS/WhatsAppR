#' Returns a Dataframe that only contains posts with type 'media' oder 'message'
#'
#' @param chat Chat history that was imported with function wapp_import
#' @param type String: 'media' for returning only media; 'message' for returning only pure messages without media
#' @return Dataframe only with chats from the type 'media' or 'message'
#' @examples
#' friends <- wapp_import("Data/WhatsApp Chat mit Freunde Uni.txt", language = "de", techuser = FALSE)
#' @import dplyr
#' @export
wapp_filter <- function(chat, type){
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


#' Transformation of Time Values: Adds Column moment.intervall
#'
#' @param chat Chat history that was imported with function wapp_import
#' @param intervall hour or quartar
#' @return Adds
#' @examples
#' test
#' @import dplyr
#' @import lubridate
#' @export
wapp_prep_time <- function(moment, intervall){

  if(intervall == "hour"){
    moment.intervall <- lubridate::hour(moment)
  } else if(intervall == "quarter"){
    moment.intervall <- cut(lubridate::hour(moment),
                            seq(0,24,3),
                            paste0("[", seq(0,24,3)[1:8], "-", seq(0,24,3)[2:9], ")"),
                            right=FALSE)
  } else if(intervall == "day"){
    moment.intervall <- lubridate::day(moment)
  } else if(intervall == "month"){
    moment.intervall <- lubridate::month(moment, label = TRUE, abbr = TRUE)
  } else if(intervall == "year"){
    moment.intervall <- lubridate::year(moment)
  } else if(intervall == "weekday"){
    moment.intervall <- lubridate::wday(moment, label = TRUE, abbr = FALSE)
  } else{
    moment.intervall <- "unknown intervall"
  }

  return(moment.intervall)
}

#' Replaces author names
#'
#' @param author column of author importet
#' @param author.old old names
#' @param author.new new names
#' @return Adds
#' @examples
#' test
#' @export
wapp_prep_author <- function(author, author.old, author.new){

  # import checks
  # techuser
  # length(unique(author)) < length(author.old)
  # length(author.old) != length(author.new)
  author.adjusted <- author

  for(i in seq_len(length(author.old))){
    author.adjusted <- ifelse(author.adjusted == author.old[i],
                              author.new[i], author.adjusted)
  }

  return(author.adjusted)
}

