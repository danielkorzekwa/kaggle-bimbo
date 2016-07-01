library(data.table)
library(ggplot2)

#load data
train_all <- fread('./segments/train_3_to_8.csv')
test_all <- fread('./segments/train_9.csv')

test <- test_all[Producto_ID==43231]
s_1 <- fread('prediction_analysis/submission_best.csv')
s_2 <- fread('prediction_analysis/submission_isNewProduct.csv')

#compute se
test$pred1 <- s_1$Demanda_uni_equil
test$se1 <- (log(test$Demanda_uni_equil+1) - log(test$pred1+1))^2
test$pred2 <- s_2$Demanda_uni_equil
test$se2 <- (log(test$Demanda_uni_equil+1) - log(test$pred2+1))^2


#analysis
merged <- merge(test,train_all[,.N,by=list(Cliente_ID,Producto_ID)],by=c('Cliente_ID','Producto_ID'),all.x=T)
merged[,list(c= .N,rmse1=sqrt(mean(se1)),rmse2=sqrt(mean(se2))),by=Agencia_ID][order(-c)][1:20]
merged <- merge(merged,Venta_hoy_avglog,by='Cliente_ID')

#plot over time
d1 <- merged[is.na(N)]
d2 <- merged[is.na(N)]

ggplot() + stat_smooth(aes(col='d1',x=d1$Venta_hoy_avglog,y=log(d1$Demanda_uni_equil+1))) +  geom_smooth(aes(col='d2',x=d2$Venta_hoy_avglog,y=log(d2$pred2+1))) 

ggplot() + stat_summary(fun.y="mean",geom="point",aes(x=d$Venta_hoy_avglog,y=log(d$Demanda_uni_equil+1))) 
