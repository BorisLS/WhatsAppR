## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("BorisLS/WhatsAppR")

## ---- warning=FALSE, error=FALSE, message=FALSE--------------------------
library(WhatsAppR)
library(dplyr)

## ------------------------------------------------------------------------
test_chat <- wapp_import(history = 'C:/WhatsApp Chat mit Test-Chat.txt',
                         language = 'de',
                         techuser = FALSE)

## ---- include = FALSE----------------------------------------------------
test_chat <- test_chat %>%
             mutate(author = wapp_prep_author(author,
                                              author.old = c("boris luetke schelhowe", "Christian Haefner", 
                                                             "Sebastian Welling",  "Fabian Steghaus", "Jörg Schmitz",
                                                             "Manuel van de Kamp", "Stefan Haefner"),
                                               author.new = c("Bobby123", "Chris", "Seb", 
                                                              "Flo-BeerPong", "Jesse", "Marc", "Steve")))

## ------------------------------------------------------------------------
test_chat <- test_chat %>%
             mutate(author = wapp_prep_author(author,
                                              author.old = c("Bobby123", "Flo-BeerPong"),
                                              author.new = c("Bobby", "Flo")))

## ------------------------------------------------------------------------
test_chat <- test_chat %>%
             filter(day >= as.Date('2016-01-01'),
                    day <= as.Date('2016-12-31'))

## ---- eval = FALSE-------------------------------------------------------
#  test_chat %>%
#    wapp_stat_author()

## ---- echo=FALSE---------------------------------------------------------
kb <- test_chat %>% wapp_stat_author()
knitr::kable(kb)

## ---- eval = FALSE-------------------------------------------------------
#  test_chat %>%
#    wapp_filter(type = "media") %>%
#    wapp_stat_author()

## ---- echo=FALSE---------------------------------------------------------
kb <- test_chat %>% wapp_filter(type="media") %>% wapp_stat_author()
knitr::kable(kb)

## ---- eval=FALSE---------------------------------------------------------
#  test_chat %>%
#    wapp_filter(type = "message") %>%
#    wapp_stat_date(n = 5)

## ---- echo=FALSE---------------------------------------------------------
kb <- test_chat %>% wapp_filter(type="message") %>% wapp_stat_date(n=5)
knitr::kable(kb)

## ---- eval=FALSE---------------------------------------------------------
#  test_chat %>%
#    filter(author == "Jesse") %>%
#    wapp_filter(type = "media") %>%
#    wapp_stat_date(n=1)

## ---- echo=FALSE---------------------------------------------------------
kb <- test_chat %>% filter(author == "Jesse") %>% wapp_filter(type="media") %>% wapp_stat_date(n=1)
knitr::kable(kb)

## ---- eval=FALSE---------------------------------------------------------
#  test_chat %>%
#    wapp_filter(type = "message") %>%
#    wapp_stat_time(interval = "quarter")

## ---- echo=FALSE---------------------------------------------------------
kb <- test_chat %>% wapp_filter(type = "message") %>% wapp_stat_time(interval = "quarter")
knitr::kable(kb)

## ---- eval=FALSE---------------------------------------------------------
#  test_chat %>%
#    wapp_filter(type = "message") %>%
#    wapp_stat_firstpost()

## ---- echo=FALSE---------------------------------------------------------
kb <- test_chat %>% wapp_filter(type = "message") %>% wapp_stat_firstpost()
knitr::kable(kb)

## ---- eval=FALSE---------------------------------------------------------
#  test_chat %>%
#    wapp_stat_emoticons(author = "Steve", n = 5)

## ---- echo=FALSE---------------------------------------------------------
kb <- test_chat %>% wapp_stat_emoticons(author = "Steve", n = 5)
knitr::kable(kb)

## ---- eval=FALSE---------------------------------------------------------
#  test_chat %>%
#    wapp_stat_word(word = "bier")

## ---- echo=FALSE---------------------------------------------------------
kb <- test_chat %>% wapp_stat_word(word = "bier")
knitr::kable(kb)

