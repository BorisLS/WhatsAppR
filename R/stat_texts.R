#' Group an imported chat history by emoticons an author has used
#'
#' @param chat a WhatsApp chat history that was imported using \code{\link{wapp_import}}
#' @param author name of author in a WhatsApp chat history
#' @param n number of top 'n' emoticons
#' @return Grouped Dataframe with informations which top 'n' emoticons are used by the choosen 'author'
#' @export
#' @import dplyr
wapp_stat_emoticons <- function(chat, author, n=3){

  # Unnest Tokens
  data <- chat %>%
          filter(type=="message") %>%
          tidytext::unnest_tokens(char, content, token = "characters", drop = TRUE) %>%
          select(-rawdata)

  # Restrict author (open issue: do it the tidy way)
  a <- author
  data <- data[data$author %in% a, ]

  # Restrict emoticons
  data <- data %>%
          inner_join(emoticons, by = c("char" = "emoticon")) %>%
          select(-unicode,-bytes.utf.8)

  no.emoticons <- dim(data)[1]

  # Group Data
  result <- data %>%
          group_by(char, description) %>%
          summarise(number = n()) %>%
          ungroup() %>%
          mutate(share = round(number / no.emoticons,4)) %>%
          arrange(desc(share)) %>%
          top_n(n, share)

  return(result)
}

#' Count occurence of (unique) emoticons an author has used in an imported chat history
#'
#' @param chat a WhatsApp chat history that was imported using \code{\link{wapp_import}}
#' @param author name of author in a WhatsApp chat history
#' @param singular TRUE for only counting unique emoticons and FALSE for counting all posted emoticons
#' @return Number of (unique) emoticons are used by the choosen 'author'
#' @export
#' @import dplyr
wapp_count_emoticons <- function(chat, author, singular){

  # Unnest Tokens
  data <- chat %>%
          filter(type=="message") %>%
          tidytext::unnest_tokens(char, content, token = "characters", drop = TRUE) %>%
          select(-rawdata)

  # Restrict author (open issue: do it the tidy way)
  a <- author
  data <- data[data$author %in% a, ]

  # Restrict emoticons
  data <- data %>%
          inner_join(emoticons, by = c("char" = "emoticon")) %>%
          select(-unicode,-bytes.utf.8)

  if(singular == FALSE){
    result <- dim(data)[1]
  }else{
    result <- length(unique(data$char))
  }

  return(result)
}

#' Group an imported chat history by a specific word an author has used
#'
#' @param chat a WhatsApp chat history that was imported using \code{\link{wapp_import}}
#' @param word a specific word in the imported chat history
#' @return Grouped Dataframe with informations which authors has used the word how many times
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

  result <- data %>%
            group_by(author) %>%
            summarise(number = n()) %>%
            arrange(desc(number))

  return(result)
}
