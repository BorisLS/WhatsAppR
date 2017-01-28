#' Return rows with matching conditions: if type is 'media' or 'message'
#'
#' @param chat a WhatsApp chat history that was imported using \code{\link{wapp_import}}
#' @param type a string. 'message' for returning only pure messages without media. 'media' for returning only media;
#' @return an object of the same class as chat
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


#' Conversion of time data in 'moment' column to time intervals
#'
#' @param moment column with time data in a WhatsApp chat history that was imported using \code{\link{wapp_import}}
#' @param interval a string with the possible expressions 'hour', 'quarter', 'day', 'month', 'year' and 'weekday'
#' @return new column with transformed time intervals
#' @import dplyr
#' @export
wapp_prep_time <- function(moment, interval){

  if(interval == "hour"){
    moment.interval <- lubridate::hour(moment)
  } else if(interval == "quarter"){
    moment.interval <- cut(lubridate::hour(moment),
                            seq(0,24,3),
                            paste0("[", seq(0,24,3)[1:8], "-", seq(0,24,3)[2:9], ")"),
                            right=FALSE)
  } else if(interval == "day"){
    moment.interval <- lubridate::day(moment)
  } else if(interval == "month"){
    moment.interval <- lubridate::month(moment, label = TRUE, abbr = TRUE)
  } else if(interval == "year"){
    moment.interval <- lubridate::year(moment)
  } else if(interval == "weekday"){
    moment.interval <- lubridate::wday(moment, label = TRUE, abbr = FALSE)
  } else{
    moment.interval <- "unknown interval"
  }

  return(moment.interval)
}

#' Replaces author names
#'
#' @param author column with names of authors in a WhatsApp chat history that was imported using \code{\link{wapp_import}}
#' @param author.old a vector with the 'old' names of authors that should be replaced
#' @param author.new a vector with the 'new' names of authors that should be replaced. Positions in vector has to correspond to positions in 'author.old'
#' @return a column with replaced names of authors
#' @export
wapp_prep_author <- function(author, author.old, author.new){

  author.adjusted <- author

  for(i in seq_len(length(author.old))){
    author.adjusted <- ifelse(author.adjusted == author.old[i],
                              author.new[i], author.adjusted)
  }

  return(author.adjusted)
}

