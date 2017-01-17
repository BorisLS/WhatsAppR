#' Emoticons per Author
#'
#' @param chat Chat history that was imported with function wapp_import
#' @param author which author
#' @param n top_n
#' @return Grouped Dataframe with Informations which Author writes how much posts
#' @export
#' @import dplyr
wapp_stat_emoticon <- function(chat, author = "John Doe", n=3){
  a <- author

  # Unnest Tokens
  data <- chat %>%
          filter(type=="message") %>%
          tidytext::unnest_tokens(char, content, token = "characters", drop = TRUE) %>%
          select(-rawdata)

  # Restrict author (open issue: to it the tidy way)
  data <- data[data$author == a, ]

  # Restrict emoticons
  data <- data %>%
          inner_join(emoticons, by = c("char" = "emoticon")) %>%
          select(-unicode,-bytes.utf.8)

  no.emoticons <- dim(data)[1]

  # Group Data
  data <- data %>%
          group_by(char, description) %>%
          summarise(number = n()) %>%
          ungroup() %>%
          mutate(share = round(number / no.emoticons,4)) %>%
          arrange(desc(share)) %>%
          top_n(n, share)

  result <- list(top_list = data,
                 no_emoticons = no.emoticons)

  return(result)
}


#' Word Occurence
#'
#' @param chat Chat history that was imported with function wapp_import
#' @param word which word occurence
#' @return Grouped Dataframe with Informations which Author writes how much posts
#' @export
#' @import dplyr
wapp_stat_word <- function(chat, word){

  # Unnest Tokens
  data <- chat %>%
          filter(type=="message") %>%
          tidytext::unnest_tokens(words, content, token = "words", drop = TRUE) %>%
          select(-rawdata)

  nu.words <- dim(data)[1]

  # Restrict word (open issue: to it the tidy way)
  data <- data[data$words == word, ]

  data <- data %>%
          group_by(author) %>%
          summarise(number = n()) %>%
          arrange(desc(number))

  result <- list(author_list = data,
                 nu_word = nu.words)

  return(result)
}
