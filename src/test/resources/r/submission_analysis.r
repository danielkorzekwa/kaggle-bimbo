#load data
train_all <- fread('./segments/train_3_to_8.csv')
test_all <- fread('./segments/train_9.csv')

test <- test_all
s_1 <- fread('prediction_analysis/submission_best_1306.csv')
s_2 <- fread('prediction_analysis/submission_covse_trained_1240.csv')

#compute se
test$pred1 <- s_1$Demanda_uni_equil
test$se1 <- (log(test$Demanda_uni_equil+1) - log(test$pred1+1))^2
test$pred2 <- s_2$Demanda_uni_equil
test$se2 <- (log(test$Demanda_uni_equil+1) - log(test$pred2+1))^2

#analysis
 merged <- merge(test,train_all[,.N,by=list(Producto_ID)],by=c('Producto_ID'))
merged[,list(c= .N,rmse1=sqrt(mean(se1)),rmse2=sqrt(mean(se2))),by=N][order(-c)][1:20]

#plot over time
d <- train_all2[Producto_ID==42128][1:1000]
ggplot() + stat_summary(fun.y="mean",geom="point",aes(x=d$Venta_hoy_avglog,y=d$Demanda_uni_equil))
