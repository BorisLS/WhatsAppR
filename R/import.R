#' Imports an exported WhatsApp chat history
#'
#' @param history a file path (txt-file) that contains the exported WhatsApp history
#' @param os the operating system of the smartphone 'iOS' or 'android'
#' @param language Note: Currently only the German language ('de') is supported
#' @param techuser a boolean variable. If WhatsApp TechUser-Posts should be part of the dataframe or not
#' @return a dataframe with the imported and processed WhatsApp history
#' @export
#' @import dplyr
wapp_import <- function(history,
                        os,
                        language = "de",
                        techuser = FALSE){
  # import data ---------------------------
  chat <- readr::read_lines(history, locale = readr::locale(encoding = "UTF-8"))
  rawdata.rows <- length(chat)

  # initialise result dataframe ---------------------------
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

    if(os == "android"){
      raw.time  <- stringr::str_split_fixed(raw.data, " - ", n=2)[1]
      raw.data1 <- stringr::str_split_fixed(raw.data, " - ", n=2)[2]

    } else if(os == "iOS"){
      raw.time  <- stringr::str_split_fixed(raw.data, ": ", n=2)[1]
      raw.data1 <- stringr::str_split_fixed(raw.data, ": ", n=2)[2]
    }

    # is raw.time a date -> depend on language -> define separat function
    raw.time.3 <- stringr::str_sub(raw.time, 3, 3)
    raw.time.6 <- stringr::str_sub(raw.time, 6, 6)
    raw.time.9 <- stringr::str_sub(raw.time, 9, 9)
    if(raw.time.3 == "." && raw.time.6 == "." && raw.time.9 == ","){
      is.time <- TRUE
    } else{
      is.time <- FALSE
    }

    if(is.time == FALSE){
      # no complete Information in line (user information and time is missing)
      chat.df[i, "moment"]  <- chat.df[i-1, "moment"]
      chat.df[i, "day"]     <- chat.df[i-1, "day"]
      chat.df[i, "author"]  <- chat.df[i-1, "author"]
      chat.df[i, "content"] <- stringr::str_trim(raw.data, side = "left")

    } else{
      chat.df[i, "moment"]  <- as.POSIXct(raw.time,
                                          format="%d.%m.%y, %H:%M")
      chat.df[i, "day"]     <- stringr::str_sub(raw.time,
                                                start = 1, end = 8)

      raw.author <- stringr::str_split_fixed(raw.data1, ": ", n = 2)[1]
      raw.content <- stringr::str_split_fixed(raw.data1, ": ", n = 2)[2]

      if(raw.author != "" && raw.content !=""){
        # complete information in line
        chat.df[i, "author"]  <- raw.author
        chat.df[i, "content"] <- raw.content

      } else{
        # technical information
        chat.df[i, "author"]  <- "TechUser"
        chat.df[i, "content"] <- stringr::str_trim(raw.data1,
                                                   side = "left")
      }
    }
  }
  # data transformation  ---------------------------
  chat.df$day <- as.Date(chat.df$day, "%d.%m.%y")
  chat.df$message.length = nchar(chat.df$content)

  # identify message type
  first.char <- stringr::str_sub(chat.df$content,
                                 start = 1,
                                 end = 1)
  last.char  <- stringr::str_sub(chat.df$content,
                                 start = chat.df$message.length,
                                 end   = chat.df$message.length)

  chat.df$type <- ifelse(first.char == "<" & last.char == ">",
                         "media", "message")

  # handling of techuser
  if(techuser == FALSE){
    chat.df <- chat.df[chat.df$author != "TechUser",]
  }

  # remove lines with no content
  chat.df <- chat.df[chat.df$rawdata != "",]

  return(chat.df)
}
