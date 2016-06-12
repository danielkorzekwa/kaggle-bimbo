#load data
train_all <- fread('./segments/train_3_to_8.csv')

#Compute avg log venta hoy
Venta_hoy_avglog <- train_all[,list(Venta_hoy_avglog = log(sum(Venta_hoy+1))),by=list(Semana,Cliente_ID)]
Venta_hoy_avglog <- Venta_hoy_avglog[,list(Venta_hoy_avglog=mean(Venta_hoy_avglog)),by=Cliente_ID]