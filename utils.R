# 微信数据截取函数
extractWeixin = function(msgXML, pattern,CDATA = T){
  if(CDATA)
    regPattern = paste0("(?<=", pattern, "><\\!\\[CDATA\\[)[\\s\\S]+(?=\\]\\]></",
                        pattern, ")")
  else
    regPattern = paste0("(?<=", pattern, ">).+(?=</",
                        pattern, ")")
  regmatches(msgXML,
             gregexpr(regPattern,
                      msgXML,
                      perl = TRUE))[[1]]
}



if(!require(mxnet)){
  install.packages("drat", repos="https://cran.rstudio.com")
  drat:::addRepo("dmlc")
  install.packages("mxnet")
}

library(mxnet)
library(imager)
# 载入模型
model = mx.model.load("Inception/Inception_BN", iteration=39)
# 载入mean image
mean.img = as.array(mx.nd.load("Inception/mean_224.nd")[["mean_img"]])

im <- load.image(system.file("extdata/parrots.png", package = "imager"))
plot(im)


preproc.image <- function(im, mean.image) {
  # crop the image
  shape <- dim(im)
  short.edge <- min(shape[1:2])
  xx <- floor((shape[1] - short.edge) / 2)
  yy <- floor((shape[2] - short.edge) / 2)
  cropped <- crop.borders(im, xx, yy)
  # resize to 224 x 224, needed by input of the model.
  resized <- resize(cropped, 224, 224)
  # convert to array (x, y, channel)
  arr <- as.array(resized) * 255
  dim(arr) <- c(224, 224, 3)
  # subtract the mean
  normed <- arr - mean.img
  # Reshape to format needed by mxnet (width, height, channel, num)
  dim(normed) <- c(224, 224, 3, 1)
  return(normed)
}

normed <- preproc.image(im, mean.img)

prob <- predict(model, X=normed)
max.idx <- max.col(t(prob))
synsets <- readLines("Inception/synset.txt")
print(paste0("Predicted Top-class: ", synsets[[max.idx]]))

showPic = function(input){
  cat <- load.image(input)
  # plot(cat)
  normed <- preproc.image(cat, mean.img)
  prob <- predict(model, X=normed)
  max.idx <- max.col(t(prob))
  print(paste0("Predicted Top-class: ", synsets[[max.idx]]))
  output = strsplit(synsets[[max.idx]]," ")[[1]]
  output[1] = ''
  return(paste(output,collapse=" "))
}



returnMsg = function(ori,user, time, msgType, content, messageId,PicUrl){
  if(length(PicUrl)==0){
    cat(234)
    return('success')
  }
  cat(123)
  filename = paste0("data/",format(Sys.time(),"%Y%m%d%M"))
  download.file(PicUrl,destfile = filename)
  
  output = sprintf("<xml>
    
    <ToUserName><![CDATA[%s]]></ToUserName>
    
    <FromUserName><![CDATA[%s]]></FromUserName>
    
    <CreateTime>%s</CreateTime>
    
    <MsgType><![CDATA[text]]></MsgType>
    
    <Content><![CDATA[介尼玛似一个%s]]></Content>
    
    </xml>",user,ori,as.numeric(Sys.time()),showPic(filename))
  return(output)
  
}