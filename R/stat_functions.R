wapp_stat_author <- function(chat, type, n){
  data <- wapp_divide(chat,type) %>%
    group_by(author) %>%
    summarise(posts = n()) %>%
    mutate(share.posts = round(posts / sum(posts), 4)) %>%
    arrange(desc(posts))
  return(data)
}

wapp_stat_date <- function(chat, type, n){
  data <- wapp_divide(chat,type) %>%
    group_by(day) %>%
    summarise(posts = n()) %>%
    mutate(share.posts = round(posts / sum(posts), 4)) %>%
    arrange(desc(posts)) %>%
    top_n(n)
  return(data)
}

wapp_stat_time <- function(chat, type, intervall){

  data <- wapp_prep_time(chat, type, intervall)

  data <- wapp_divide(data,type) %>%
    group_by(time.intervall) %>%
    summarise(posts = n()) %>%
    mutate(share.posts = round(posts / sum(posts), 4)) %>%
    arrange(desc(posts))
  return(data)
}
