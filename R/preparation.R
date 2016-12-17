wapp_divide <- function(chat, type){
  if(type == "media"){
    data <- chat %>%
      filter(type == "media")
  }else if(type == "message"){
    data <- chat %>%
      filter(type == "message")
  }else{
    data <- chat
  }
  return(data)
}

wapp_prep_time <- function(chat, intervall){

  if(intervall == "hour"){
    inter.time <- seq(0,24,1)
  } else if(intervall == "quarter"){
    inter.time <- seq(0,24,3)
  }
  l.i.t <- length(inter.time)

  inter.label <- paste0(inter.time[1:(l.i.t-1)], "-", inter.time[2:(l.i.t)])
  time.hour <- as.integer(str_sub(chat$time, 1, 2))

  chat$time.intervall <- cut(time.hour, inter.time,
                             labels=inter.label, right=FALSE)
  return(chat)
}
