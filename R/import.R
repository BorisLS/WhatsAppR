#' Imports an exported WhatsApp chat history
#'
#' @param history a file path (txt-file) that contains the exported WhatsApp history
#' @param language is necessary to identify the type of the message. Note: Currently only the German language ('de') is supported
#' @param techuser a boolean variable. If WhatsApp TechUser-Posts should be part of the dataframe or not
#' @return a dataframe with the imported and processed WhatsApp history
#' @export
#' @import dplyr
wapp_import <- function(history, language = "de", techuser = FALSE){
  #' @import dplyr
  # import data ---------------------------
  # read in the chat export file
  # each line ist separated with \n

  chat <- readr::read_lines(history, locale = readr::locale(encoding = "UTF-8"))
  rawdata.rows <- length(chat)

  chat.df <- data.frame(moment = rep(as.POSIXct("16.1.2017 07:04", format="%d.%m.%y, %H:%M"),
                                     rawdata.rows),
                        day = rep("t.na", rawdata.rows),
                        author = rep("v.na", rawdata.rows),
                        content = rep("t.na", rawdata.rows),
                        rawdata = rep("r.na", rawdata.rows),
                        stringsAsFactors = FALSE)

  # data processing ---------------------------
  for(i in 1:rawdata.rows){

    raw.data <- as.character(chat[i])
    chat.df[i, "rawdata"] <- raw.data

    # estimate if the line contains all information
    third.pos <- stringr::str_sub(raw.data, start = 3, end = 3)
    fifth.pos <- stringr::str_sub(raw.data, start = 6, end = 6)
    sep.pos <- length(unlist(stringr::str_locate_all(pattern =":", raw.data)))

    if(third.pos == "." && fifth.pos == "." && sep.pos >= 4){
      # full information in line available
      chat.df[i, "moment"] <- as.POSIXct(stringr::str_sub(raw.data, start = 1, end = 15),
                                         format="%d.%m.%y, %H:%M")
      chat.df[i, "day"]   <- stringr::str_sub(raw.data, start = 1, end = 8)

      raw.data2 <- stringr::str_sub(raw.data, start = 19, end = nchar(raw.data))
      raw.data3 <- stringr::str_split_fixed(raw.data2, ":", n=2)
      chat.df[i, "author"] <- raw.data3[1]
      chat.df[i, "content"]      <- stringr::str_trim(raw.data3[2], side = "left")

    } else if(third.pos == "." && fifth.pos == "." && sep.pos < 4){
      # technical information in line (e.g. add members, change chat name)
      chat.df[i, "moment"] <- as.POSIXct(stringr::str_sub(raw.data, start = 1, end = 15),
                                         format="%d.%m.%y, %H:%M")
      chat.df[i, "day"]   <- stringr::str_sub(raw.data, start = 1, end = 8)

      raw.data2 <- stringr::str_sub(raw.data, start = 19, end = nchar(raw.data))
      chat.df[i, "author"] <- "TechUser"
      chat.df[i, "content"]      <- stringr::str_trim(raw.data2, side = "left")

    } else{
      # no complete Information in line (user information is missing)
      chat.df[i, "moment"]  <- chat.df[i-1, "moment"]
      chat.df[i, "day"]     <- chat.df[i-1, "day"]
      chat.df[i, "author"]  <- chat.df[i-1, "author"]
      chat.df[i, "content"] <- stringr::str_trim(raw.data, side = "left")
    }
  }


  # data transformation  ---------------------------
  chat.df$day <- as.Date(chat.df$day, "%d.%m.%y")
  chat.df$message.length = nchar(chat.df$content)

  # identify message type
  if(language == "de"){
    chat.df$type <- ifelse(chat.df$content == "<Medien weggelassen>",
                           "media", "message")
  } else(
    chat.df$type <- "unkown language"
  )

  # handling of techuser
  if(techuser == FALSE){
    chat.df <- chat.df[chat.df$author != "TechUser",]
  }

  # remove lines with no content
  chat.df <- chat.df[chat.df$rawdata != "",]

  return(chat.df)
}
