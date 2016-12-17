#' Imports an exported WhatsApp chat history
#'
#' @param history File path that contains the chat history.
#' @param language Only German Language is supported.
#' @param techuser Boolean: If WhatsApp TechUser-Posts should be part of the Dataframe
#' @return Dataframe with the WhatsApp chat history
#' @examples
#' friends <- wapp_import("Data/WhatsApp Chat mit Freunde Uni.txt", language = "de", techuser = FALSE)
wapp_import <- function(history, language = "de", techuser = FALSE){

  # Import data ---------------------------
  # Read in the chat export file
  # Each line ist separated with \n
  chat <- read_lines(history, locale = locale(encoding = "UTF-8"))

  #Each line is separated with \n
  rawdata.rows <- length(chat)

  chat.df <- data.frame(day = rep("t.na", rawdata.rows),
                        time = rep("z.na", rawdata.rows),
                        author = rep("v.na", rawdata.rows),
                        content = rep("t.na", rawdata.rows),
                        rawdata = rep("r.na", rawdata.rows),
                        stringsAsFactors = FALSE)

  # Data preparation ---------------------------
  for(i in 1:rawdata.rows){

    raw.data <- as.character(chat[i])
    chat.df[i, "rawdata"] <- raw.data

    # Estimate if the line contains all information
    third.pos <- str_sub(raw.data, start = 3, end = 3)
    fifth.pos <- str_sub(raw.data, start = 6, end = 6)
    sep.pos <- length(unlist(str_locate_all(pattern =":", raw.data)))

    if(third.pos == "." && fifth.pos == "." && sep.pos >= 4){
      # Full information in line
      chat.df[i, "day"]   <- str_sub(raw.data, start = 1, end = 8)
      chat.df[i, "time"]  <- str_sub(raw.data, start = 11, end = 15)

      raw.data2 <- str_sub(raw.data, start = 19, end = nchar(raw.data))
      raw.data3 <- str_split_fixed(raw.data2, ":", n=2)
      chat.df[i, "author"] <- raw.data3[1]
      chat.df[i, "content"]      <- str_trim(raw.data3[2], side = "left")

    } else if(third.pos == "." && fifth.pos == "." && sep.pos < 4){
      # Technical information in line (e.g. add members, change chat name)
      chat.df[i, "day"]   <- str_sub(raw.data, start = 1, end = 8)
      chat.df[i, "time"]  <- str_sub(raw.data, start = 11, end = 15)

      raw.data2 <- str_sub(raw.data, start = 19, end = nchar(raw.data))
      chat.df[i, "author"] <- "TechUser"
      chat.df[i, "content"]      <- str_trim(raw.data2, side = "left")

    } else{
      # No complete Information about User
      chat.df[i, "day"]       <- chat.df[i-1, "day"]
      chat.df[i, "time"]      <- chat.df[i-1, "time"]
      chat.df[i, "author"]    <- chat.df[i-1, "author"]
      chat.df[i, "content"]      <- str_trim(raw.data, side = "left")
    }
  }


  # Data transformation  ---------------------------
  chat.df$day <- as.Date(chat.df$day, "%d.%m.%y")
  chat.df$message.length = nchar(chat.df$content)

  if(language == "de"){
    chat.df$type <- ifelse(chat.df$content == "<Medien weggelassen>",
                           "media", "message")
  } else(
    chat.df$type <- "unkown language"
  )

  if(techuser == FALSE){
    chat.df <- chat.df %>% filter(author != "TechUser")
  }

  return(chat.df)
}
