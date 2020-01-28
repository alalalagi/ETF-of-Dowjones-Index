#道瓊工業指數ETF

#Read data-------------------------
DJ = read.csv("stock/^DJI.csv")
plot(DJ$Adj.Close,pch=16,col="red",xlab = 'time',main = "DJ price")
file_list = list.files("stock")
data = numeric()
for (i in c(1:length(file_list))){
  #if (i ==8)
    #next
  #print(i)
  name = paste0('stock/',file_list[i])
  temp = read.csv(name)[,6]
  data = cbind(data,temp)
}

rownames(data) = read.csv(name)$Date
colnames(data) = file_list

#Calculate the Denominator-------------------------
Deno = numeric()
for (i in c(1:nrow(DJ))){
  Deno = append(Deno,sum(data[i,])/DJ$Close[i])
}
plot(c(1:708),Deno,pch=16,col="black",xlab = 'time',main = "Dinominator over time")

#Linear regression,y是道瓊指數收盤回報率,x是道瓊成分股收盤回報率-------------------
DJreturn = numeric()
for (i in c(1:707)){
  DJreturn[i] = (DJ[i+1,5]-DJ[i,5])/DJ[i,5]
  #DJreturn[i] = log(DJ[i,5])
}
datareturn = matrix(nrow = 707,ncol = 29)
for (i in c(1:707)){
  for(j in c(1:29)){
    datareturn[i,j] = (data[i+1,j]-data[i,j])/data[i,j]
    #datareturn[i,j] = log(data[i,j])
  }
}
datareturn = datareturn[,-1]

fit <- lm(DJreturn ~ datareturn)
summary(fit)

#Prediction error-------------------------
predreturn <- predict(fit, as.data.frame(datareturn))
error = predreturn-DJreturn
plot(error,xlab = 'time',main = "Error of return prediction")
sum(error) #return的總誤差

#Predicing DJ price-------------------------
predprice = numeric()
predprice[1] = DJ$Close[1]
for (i in c(1:707)){
  predprice[i+1] = predprice[i]*(1+predreturn[i])
}

plot(DJ$Close,pch=16,col="red",
     xlab = 'time',main = "ETF price")
points(predprice,pch=16,col="navy")
legend("topleft",pch = 1,
       col = c("red","blue"),
       legend = c("Real DJ Close","Prediction price")
)



