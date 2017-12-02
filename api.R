# .libPaths("/home/chiffon/R/x86_64-pc-linux-gnu-library/3.3")
library(fiery)

source('config.ini')
source("utils.R")
# Create a New App
app <- Fire$new()
app$host = IP
app$port = port


app$on('start', function(server, ...) {
  server$set_data('visits', 0)
  server$set_data('cycles', 0)
})

# Count the number of cycles (internal loops)
app$on('cycle-start', function(server, ...) {
  server$set_data('cycles', server$get_data('cycles') + 1)
})

# Count the number of requests
app$on('before-request', function(server, ...) {
  server$set_data('visits', server$get_data('visits') + 1)
})

# Handle requests
app$on('request', function(server, request, ...) {
  # request$set_body(request)
  # servera<<-server
  # aaa<<-request
  
  request$parse_raw()
  message(request$as_message())
  print(rawToChar(request$body))

  msgXML = rawToChar(request$body)
  ori = extractWeixin(msgXML, "ToUserName")
  user = extractWeixin(msgXML, "FromUserName")
  time = as.POSIXct(as.numeric(extractWeixin(msgXML, "CreateTime",F)),
                    origin = "1970-01-01")
  msgType = extractWeixin(msgXML, "MsgType")
  content = extractWeixin(msgXML, "Content")
  messageId =  extractWeixin(msgXML, "MsgId",F)
  PicUrl = extractWeixin(msgXML, "PicUrl")
  print(PicUrl)
  # output = sprintf('"%s","%s","%s","%s","%s"\n',user,time,msgType,content,messageId)
  # cat(output)
  #cat(rawToChar(request$body))
  #cat('\n')
  # print(names(request))
  response <- request$respond()
  response$status <- 200L
  #response$body <- paste0('<h1>This is indeed a test. You are number ', server$get_data('visits'), '</h1>')
  
  theQuery = request$query
  # theQ <<- theQuery
  if('echostr' %in% names(theQuery)){
    
    response$body = regmatches(request$querystring,
                               gregexpr("(?<=echostr=).+(?=&t)",
                                        request$querystring,
                                        perl = TRUE))
  }else{
    print(123)
    response$body = returnMsg(ori,user, time, msgType, content, messageId, PicUrl)
  }
  # cat(response$body)
  # response$body <- 'success'
  response$type <- 'html'
})

# Show number of requests in the console
app$on('after-request', function(server, ...) {
  #message(server$get_data('visits'))
  flush.console()
})

# Terminate the server after 50 cycles
# app$on('cycle-end', function(server, ...) {
#   if (server$get_data('cycles') > 500) {
#     message('Ending...')
#     flush.console()
#     server$extinguish()
#   }
# })

# Be polite
app$on('end', function(server) {
  # sink()
  #message('Goodbye')
  flush.console()
})

# sink('data.txt', append = T)
app$ignite(showcase = TRUE)
#> Fire started at 127.0.0.1:8080
#> 1
#> Ending...
#> Goodbye