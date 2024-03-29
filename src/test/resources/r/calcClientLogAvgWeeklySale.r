#load data
train_all <- fread('./segments/train_3_to_8.csv')

#Compute avg log venta hoy
Venta_hoy_avglog <- train_all[,list(Venta_hoy_avglog = log(sum(Venta_hoy+1))),by=list(Semana,Cliente_ID)]
Venta_hoy_avglog <- Venta_hoy_avglog[,list(Venta_hoy_avglog=mean(Venta_hoy_avglog)),by=Cliente_ID]

#Compute avg log demand
avgDemandByClient = train_all[,list(avgLogDemand = mean(log(Demanda_uni_equil+1))),by=Cliente_ID]

#Compute avg price
train_all[Venta_uni_hoy>0,list(avgLogPrice=mean(log(Venta_hoy/Venta_uni_hoy+1))),by=Producto_ID]

#Compute return ratio
train_all$return_ratio <- train_all$Dev_uni_proxima / (train_all$Dev_uni_proxima + train_all$Venta_uni_hoy)
avgReturnRatioByClient <- train_all[,list(avgReturnRatio=mean(return_ratio)),by=Cliente_ID]