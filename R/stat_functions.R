#' Aggregegates author
#'
#' @param chat Chat history that was imported with function wapp_import
#' @return Grouped Dataframe with Informations which Author writes how much posts
#' @export
#' @import dplyr
wapp_stat_author <- function(chat){
  data <- chat %>%
          group_by(author) %>%
          summarise(posts = n()) %>%
          mutate(share.posts = round(posts / sum(posts), 4)) %>%
          arrange(desc(posts))
  return(data)
}

#' Aggregegates Date
#'
#' @param chat Chat history that was imported with function wapp_import
#' @param type String: 'media' for returning only media; 'message' for returning only pure messages without media
#' @return Grouped Dataframe with Informations about the Top - n Dates with Postings
#' @export
#' @import dplyr
wapp_stat_date <- function(chat, n){
  data <- chat %>%
          group_by(day) %>%
          summarise(posts = n()) %>%
          mutate(share.posts = round(posts / sum(posts), 4)) %>%
          arrange(desc(posts)) %>%
          top_n(n, posts)
  return(data)
}

#' Aggregegates Time intervall
#'
#' @param chat Chat history that was imported with function wapp_import
#' @param intervall String: 'media' for returning only media; 'message' for returning only pure messages without media
#' @return Grouped Dataframe with Informations in which time intervall the most posts occur
#' @export
#' @import dplyr
wapp_stat_time <- function(chat, intervall){

  data <- chat %>%
          mutate(time.intervall = wapp_prep_time(moment, intervall)) %>%
          group_by(time.intervall) %>%
          summarise(posts = n()) %>%
          mutate(share.posts = round(posts / sum(posts), 4)) %>%
          arrange(desc(posts))
  return(data)
}


#' Who posts the first post on an day
#'
#' @param chat Chat history that was imported with function wapp_import
#' @return Grouped Dataframe with Informations who posts the first post
#' @export
#' @import dplyr
wapp_stat_firstpost <- function(chat){

  data <- chat %>%
          select(day, author) %>%
          group_by(day) %>%
          summarise(first.post = head(author,1)) %>%
          ungroup()

  nu.days <- nu_days <- dim(data)[1]

  data <- data %>%
          group_by(first.post) %>%
          summarise(count = n()) %>%
          mutate(share = round(count/nu_days, 4)) %>%
          arrange(desc(count))

  return(data)
}

