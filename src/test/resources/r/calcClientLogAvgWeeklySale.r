#load data
train_all <- fread('./segments/train_8.csv')

#Compute avg log venta hoy
Venta_hoy_avglog <- train_all[,list(Venta_hoy_avglog = log(sum(Venta_hoy+1))),by=list(Semana,Cliente_ID)]
Venta_hoy_avglog <- Venta_hoy_avglog[,list(Venta_hoy_avglog=mean(Venta_hoy_avglog)),by=Cliente_ID]

#Compute avg log demand
avgDemandByClient = train_all[,list(avgLogDemand = mean(log(Demanda_uni_equil+1))),by=Cliente_ID]