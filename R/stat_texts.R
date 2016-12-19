#' Emoticons per Author
#'
#' @param chat Chat history that was imported with function wapp_import
#' @return Grouped Dataframe with Informations which Author writes how much posts
#' @export
#' @import dplyr
#' @import tidytext
wapp_stat_emoticon <- function(chat, author = "John Doe", n=3){
  a <- author

  # Unnest Tokens
  data <- chat %>%
          filter(type=="message") %>%
          unnest_tokens(char, content, token = "characters", drop = TRUE) %>%
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
