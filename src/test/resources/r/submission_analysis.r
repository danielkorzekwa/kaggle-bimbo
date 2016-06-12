#load data
train_all <- fread('./segments/train_3_to_8.csv')
test_all <- fread('./segments/train_9.csv')

test <- test_all[Producto_ID==47336]
s <- fread('c:/perforce/git/kaggle-bimbo/target/submission.csv')

#compute se
test$pred <- s$Demanda_uni_equil
test$se <- (log(test$Demanda_uni_equil+1) - log(test$pred+1))^2

#analysis
merged <- merge(test,train[,.N,by=list(Cliente_ID,Producto_ID)],by=c('Cliente_ID','Producto_ID'))
merged[,list(c= .N,rmse=sqrt(mean(se))),by=Producto_ID][order(Producto_ID)][Producto_ID==42128]

#plot over time
d <- train_all2[Producto_ID==42128][1:1000]
ggplot() + stat_summary(fun.y="mean",geom="point",aes(x=d$Venta_hoy_avglog,y=d$Demanda_uni_equil))


#Compute avg log venta hoy
Venta_hoy_avglog <- train_all[,list(Venta_hoy_avglog = log(sum(Venta_hoy+1))),by=list(Semana,Cliente_ID)]
Venta_hoy_avglog <- Venta_hoy_avglog[,list(Venta_hoy_avglog=mean(Venta_hoy_avglog)),by=Cliente_ID]