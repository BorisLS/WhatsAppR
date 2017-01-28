#' Group an imported chat history by author
#'
#' @param chat a WhatsApp chat history that was imported using \code{\link{wapp_import}}
#' @return Grouped dataframe with informations which author writes how many posts and what is proportion of total posts
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

#' Group an imported chat history by date
#'
#' @param chat a WhatsApp chat history that was imported using \code{\link{wapp_import}}
#' @param n number of days
#' @return Grouped dataframe with informations in which time interval how many posts and what is proportion of total posts
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

#' Group an imported chat history by time interval
#'
#' @param chat a WhatsApp chat history that was imported using \code{\link{wapp_import}}
#' @param interval interval a string with the possible expressions 'hour', 'quarter', 'day', 'month', 'year' and 'weekday'
#' @return Grouped dataframe with informations on which 'n' days the many posts were posted towhat is proportion of total posts
#' @export
#' @import dplyr
wapp_stat_time <- function(chat, interval){

  data <- chat %>%
          mutate(time.interval = wapp_prep_time(moment, interval)) %>%
          group_by(time.interval) %>%
          summarise(posts = n()) %>%
          mutate(share.posts = round(posts / sum(posts), 4)) %>%
          arrange(desc(posts))
  return(data)
}


#' Who posts the first post on an day
#'
#' @param chat a WhatsApp chat history that was imported using \code{\link{wapp_import}}
#' @return Grouped dataframe with informations which author has posted how often the first entry of the day
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

