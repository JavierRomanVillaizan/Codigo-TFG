#Se cargan los datos de los vuelos nacionales:

Sobrevuelos<-as.data.frame(ext(Datos,"Overflight")[1])
sob_train<-as.data.frame(ext(Datos,"Overflight")[2])
sob_test<-as.data.frame(ext(Datos,"Overflight")[3])
sob_serie<-ts(Sobrevuelos$Overflight,start=c(2013,1),end=c(2021,4),frequency=12)

#Anális gráfico

plot_time_series(Sobrevuelos,Fecha,Overflight,.interactive=FALSE,.title="Evolución de los sobrevuelos",.x_lab="Año",.y_lab="Vuelos")

boxplot(nac_serie~cycle(sob_serie),main="Distribución de los sobrevuelos por mes",
          xlab="Mes",ylab="Vuelos")


## Ajuste modelo Arima

#Se empieza comprobando la estacionariedad en media y varianza:
box_cox <- boxcox(Overflight~Fecha,data=sob_train,lambda = c(0, 0.5, 1))

lambda <- box_cox$x[which.max(box_cox$y)]
lambda #No se transforma

sob_train.ts<-ts(sob_train$Overflight,start=c(2013,1),end=c(2020,7),frequency=12)
adf.test(sob_train.ts,k=12) #Diferencia estacional
sob_dif<-diff(sob_train.ts,lag=12)

#Ajuste ARIMA
acf2(sob_train.ts)#p=1

ajuste_1<-Arima(nac_train.ts,order=c(1,0,0),seasonal=list(order=c(0,1,0),frequency=12))
coeftest(ajuste_1)
acf2(ajuste_1$residuals) #Diferencia porque AR(1) próximo a 1

ajuste_2<-Arima(nac_train.ts,order=c(0,1,0),seasonal=list(order=c(0,1,0),frequency=12))
acf2(ajuste_2$residuals)#p=1 o q=1

ajuste_2b<-Arima(sob_train.ts,order=c(1,1,0),seasonal=list(order=c(0,1,0),frequency=12))
coeftest(ajuste_2b)
acf2(ajuste_2b$residuals)#Ajustado

ajuste_3<-Arima(nac_train.ts,order=c(0,1,1),seasonal=list(order=c(0,1,0),frequency=12))
coeftest(ajuste_3)
acf2(ajuste_3$residuals)#Nos quedamos con el modelo

checkresiduals(ajuste_3)#No hay Ruido Blanco

#Con diferentes estacionalidades:
adf.test(sob_train.ts,k=3)#Estacionaria
ajuste3<-sarima(sob_train.ts,0,1,1,0,0,0,3,no.constant = TRUE)

adf.test(sob_train.ts,k=4)# No estacionaria al 5%
ajuste4<-sarima(sob_train.ts,1,0,1,1,1,0,4,no.constant = TRUE)

adf.test(sob_train.ts,k=6)#no estacionaria al 5%
ajuste6<-sarima(sob_train.ts,1,1,0,1,1,0,6)

# ETS
modelo_ets <-exp_smoothing()%>% set_engine(engine="ets")
ets<-modelo_ets%>% fit(Overflight~Fecha, data=sob_train)

arima<-arima_reg(seasonal_period = 12,non_seasonal_ar =0 ,
                        non_seasonal_differences =1 ,non_seasonal_ma =1 ,seasonal_ar =0 ,
                        seasonal_differences =1, seasonal_ma =0 )%>%set_engine("arima")%>%
                        fit(Overflight~Fecha,data=sob_train)

modelos<- modeltime_table(
  arima,
  ets
)
calibracion<-modelos %>%modeltime_calibrate(new_data = sob_test)
calibracion%>%modeltime_forecast(new_data = sob_test)%>%
  plot_modeltime_forecast(.legend_max_width = 25,.interactive=FALSE, .title="PredicciÃ³n sobre test de los sobrevuelos",.x_lab="AÃ±o",.y_lab="Vuelos" )
calibracion %>%modeltime_accuracy()%>%table_modeltime_accuracy(.interactive=FALSE)


#Random Forest
sob_lag<-embed(Sobrevuelos$Overflight,13)
Fecha_lag<-Sobrevuelos$Fecha[13:100]
serie_lag<-as.data.frame(sob_lag)
serie_lag["Fecha"]=Fecha_lag
lag_train<-subset(serie_lag,Fecha<="2020-08-01")
lag_test<-subset(serie_lag,Fecha>"2020-08-01")
sob_val_lag<-time_series_cv(lag_train,Fecha,67,12)


rf_mod <-rand_forest( min_n = tune(),trees=tune()) %>% 
           set_engine("randomForest") %>% set_mode("regression")

rf_workflow <- workflow() %>% add_formula(V1~.)%>%add_model(rf_mod)
set.seed(0)
grid_rf<-grid_random(min_n(), trees(), size = 100)

rf_res <- rf_workflow %>% tune_grid(sob_val_lag,grid = grid_rf,
                                   metrics = metric_set(rmse,mape))
rf_res%>%show_best(metric="mape")
params_rf<-select_best(rf_res,metric="mape")
modelo_rf<-finalize_model(rf_mod,params_rf)

random_forest<-modelo_rf%>%fit(V1~., data=lag_train)

rf<-modeltime_table(random_forest)
rf_cal<-rf%>%modeltime_calibrate(lag_test)
rf_for<-modeltime_forecast(rf,lag_test)
rf_cal%>%modeltime_accuracy()


#XGBoost
xg_mod<- boost_tree(tree_depth=tune(),trees=tune(),learn_rate=tune(),
                    min_n=tune(),loss_reduction=tune())%>%
                    set_engine("xgboost")%>%set_mode("regression")


xg_workflow <- workflow() %>% add_formula(V1~.)%>%add_model(xg_mod)
set.seed(0)
grid_xg<-grid_random(tree_depth(), trees(),learn_rate(),
                     loss_reduction(),min_n(), size=100)
xg_res <- xg_workflow %>% tune_grid(sob_val_lag,grid = grid_xg,
                                   metrics = metric_set(rmse,mape))
xg_res%>%show_best(metric="mape")
params_xg<-select_best(xg_res,metric="mape")
modelo_xg<-finalize_model(xg_mod,params_xg)
xgboost<-modelo_xg%>%fit(V1~., data=lag_train)



xg<-modeltime_table(xgboost)
xg_cal<-xg%>%modeltime_calibrate(lag_test)
xg_for<-modeltime_forecast(xg,lag_test)
xg_cal%>%modeltime_accuracy()

#Predicción RF y XGBoost
sob_extended <- Sobrevuelos %>%
  future_frame(
    .length_out = 12,
    .bind_data  = TRUE
  ) 

lag_transformer_grouped <- function(data){
  data %>%
    tk_augment_lags(Overflight, .lags = 1:12) 
}

sob_lags <- sob_extended %>%
  lag_transformer_grouped()


future_data <-sob_lags %>%
  filter(is.na(Overflight))

train_data <- sob_lags %>%
  drop_na()

model_xg<- modelo_xg %>%
  fit(Overflight ~ ., data = train_data) %>%
  
  recursive(
    transform = lag_transformer_grouped,
    train_tail = tail(train_data, 12)
  )

model_rf<- modelo_rf %>%
  fit(Overflight ~ ., data = train_data) %>%
  
  recursive(
    transform = lag_transformer_grouped,
    train_tail = tail(train_data, 12)
  )

for_tree<-modeltime_table(model_rf,model_xg) %>%
  modeltime_forecast(
    new_data    = future_data,
    actual_data = sob_lags,
  )


pred_tree<-for_tree%>%filter(.key=="prediction")
for_rf=pred_tree$.value[1:12]
for_xg=pred_tree$.value[13:24]
Fecha=pred_tree$.index[1:12]

predict_tree<-data.frame("Fecha"=Fecha,"Random Forest"=for_rf,"XGBoost"=for_xg)



#Prophet
Fecha_pr<-sob_train$Fecha
valor_pr<-sob_train$Overflight
df_pr<-data.frame("ds"=Fecha_pr,"y"=valor_pr)
pr<-prophet::prophet(yearly.seasonality = 12)
pr<-prophet::fit.prophet(pr,df_pr)

futuro<-prophet::make_future_dataframe(pr,9,freq="month")
prediccion<-predict(pr,futuro)

df_met<-data.frame("Real"=sob_test,"Estimación"=prediccion$yhat[92:100])
metricas<-metric_set(mae,mape,mase,smape,rmse)
metricas(df_met,Real.Overflight,Estimación)

#BSTS
ss <- AddLocalLevel(list(), sob_train.ts)
ss <- AddSeasonal(ss, sob_train.ts, nseasons = 12)
ss<-AddSemilocalLinearTrend(ss,sob_train.ts)
model <- bsts(sob_train.ts, state.specification = ss, niter = 500)
pred<- predict.bsts(model, horizon = 9,burn=100)

df_met<-data.frame("Real"=sob_test,"Estimación"=pred$mean)
metricas(df_met,Real.Overflight,Estimación)



##Con datos prepandemia

sob_train_2019<-subset(Sobrevuelos, Fecha<="2019-04-01")
sob_test_2019<-subset(Sobrevuelos,"2019-04-01"< Fecha & Fecha<="2020-04-01")
sob_train_2019.ts<-ts(sob_train_2019$Overflight,start=c(2013,1),end=c(2019,3),frequency=12)

#ARIMA
ajuste_12<-arima(sob_train_2019.ts,order=c(1,1,0),seasonal=list(order=c(1,1,0),frequency=12))

ajustecon3<-sarima(sob_train_2019.ts,0,1,0,0,0,2,3)

ajustecon4<-sarima(sob_train_2019.ts,1,0,0,1,0,0,4)

ajustecon6<-sarima(sob_train_2019.ts,2,1,0,1,0,0,6)

#ETS
modelo_ets <-exp_smoothing()%>% set_engine(engine="ets")
ets<-modelo_ets%>% fit(Overflight~Fecha, data=sob_train_2019)

arima<-arima_reg(seasonal_period = 12,non_seasonal_ar =1 ,
                        non_seasonal_differences =1 ,non_seasonal_ma =0 ,seasonal_ar =1 ,
                        seasonal_differences =1, seasonal_ma =0 )%>%set_engine("arima")%>%
                        fit(Overflight~Fecha,data=sob_train_2019)


modelos<- modeltime_table(
  arima,
  ets
)
calibracion<-modelos %>%modeltime_calibrate(new_data = sob_test_2019)
calibracion %>%modeltime_accuracy()%>%table_modeltime_accuracy(.interactive=FALSE)


#Random Forest

lag_train_2019<-subset(serie_lag,Fecha<="2019-04-01")
lag_test_2019<-subset(serie_lag,Fecha>"2019-04-01"& Fecha<="2020-04-01")

sob_val_2019<-time_series_cv(lag_train_2019,Fecha,51,12)

rf_mod <-  rand_forest(min_n = tune(),trees=tune()) %>% 
           set_engine("randomForest") %>% set_mode("regression")

rf_workflow <- workflow() %>% add_formula(V1~.)%>% add_model(rf_mod)  
set.seed(0)
grid_rf<-grid_random(min_n(), trees(), size = 100)

rf_res <- rf_workflow %>% tune_grid(sob_val_2019,grid = grid_rf,
                                   metrics = metric_set(rmse,mape))
rf_res%>%show_best(metric="mape")
params_rf<-select_best(rf_res,metric="mape")
modelo_rf<-finalize_model(rf_mod,params_rf)

#XGBoost
xg_mod<- boost_tree(tree_depth=tune(),trees=tune(),learn_rate=tune(),
                    min_n=tune(),loss_reduction=tune())%>%
                    set_engine("xgboost")%>%set_mode("regression")

xg_workflow <- workflow() %>% add_formula(V1~.)%>%add_model(xg_mod)
set.seed(0)
grid_xg<-grid_random(tree_depth(), trees(),learn_rate(),
                     loss_reduction(),min_n(), size=100)
xg_res <- xg_workflow %>% tune_grid(sob_val_2019,grid = grid_xg,
                                   metrics = metric_set(rmse,mape))
xg_res%>%show_best(metric="mape")
params_xg<-select_best(xg_res,metric="mape")
modelo_xg<-finalize_model(xg_mod,params_xg)

random_forest<-modelo_rf%>%fit(V1~., data=lag_train_2019)

xgboost<-modelo_xg%>%fit(V1~., data=lag_train_2019)

model_tree<-modeltime_table(random_forest,xgboost)
model_cal<-model_tree%>%modeltime_calibrate(lag_test_2019)
model_cal%>%modeltime_accuracy()

#Prophet
Fecha_pr<-sob_train_2019$Fecha
valor_pr<-sob_train_2019$Overflight
df_pr<-data.frame("ds"=Fecha_pr,"y"=valor_pr)
pr<-prophet::prophet(yearly.seasonality = 12)
pr<-prophet::fit.prophet(pr,df_pr)

futuro<-prophet::make_future_dataframe(pr,12,freq="month")
prediccion<-predict(pr,futuro)

df_met<-data.frame("Real"=sob_test_2019,"Estimación"=prediccion$yhat[76:87])
metricas(df_met,Real.Overflight,Estimación)


#BSTS
ss <- AddLocalLevel(list(), sob_train_2019.ts)
ss <- AddSeasonal(ss, sob_train_2019.ts, nseasons = 12)
ss<-AddSemilocalLinearTrend(ss,sob_train_2019.ts)
model_bsts <- bsts(sob_train_2019.ts, state.specification = ss, niter = 500)
pred<- predict.bsts(model_bsts, horizon = 12,burn=100)

df_met<-data.frame("Real"=sob_test_2019,"Estimación"=pred$mean)
metricas(df_met,Real.Overflight,Estimación)


##Comportamiento en la pandemia

sob_pre<-subset(Sobrevuelos,Fecha<"2020-04-01")
sob_pand<-subset(Sobrevuelos,"2020-04-01"< Fecha)

#ARIMA y ETS
forecast1<-modelos%>%modeltime_forecast(h="13 months",actual_data=sob_pre)

df_met<-data.frame("Real"=sob_pand,"Estimación"=forecast1$.value[88:100])
metricas(df_met,Real.Overflight,Estimación)

suavi<-data.frame("Real"=sob_pand,"Estimación"=forecast1$.value[101:113])
metricas(suavi,Real.Overflight,Estimación)

#Prophet
futuro<-prophet::make_future_dataframe(pr,25,freq="month")
prediccion<-predict(pr,futuro)

df_met<-data.frame("Real"=sob_pand,"Estimación"=prediccion$yhat[88:100])
metricas(df_met,Real.Overflight,Estimación)

#BSTS
pred<- predict.bsts(model_bsts, horizon = 25,burn=100)

df_met<-data.frame("Real"=sob_pand,"Estimación"=pred$mean[13:25])
metricas(df_met,Real.Overflight,Estimación)

#RF y XGBoost
sob_extended <- sob_pre %>%
  future_frame(
    .length_out = 13,
    .bind_data  = TRUE
  ) 

lag_transformer_grouped <- function(data){
  data %>%
    tk_augment_lags(Overflight, .lags = 1:12) 
}

sob_lags <- sob_extended %>%
  lag_transformer_grouped()


future_data <-sob_lags %>%
  filter(is.na(Overflight))

train_data <- sob_lags %>%
  drop_na()

model_xg<- modelo_xg %>%
  fit(Overflight ~ ., data = train_data) %>%
  
  recursive(
    transform = lag_transformer_grouped,
    train_tail = tail(train_data, 12)
  )

model_rf<- modelo_rf %>%
  fit(Overflight ~ ., data = train_data) %>%
  
  recursive(
    transform = lag_transformer_grouped,
    train_tail = tail(train_data, 12)
  )

for_tree<-modeltime_table(model_rf,model_xg) %>%
  modeltime_forecast(
    new_data    = future_data,
    actual_data = sob_lags,
  )


pred_tree<-for_tree%>%filter(.key=="prediction")
for_rf=pred_tree$.value[1:13]
for_xg=pred_tree$.value[14:26]
Fecha=pred_tree$.index[1:13]

df_met_rf<-data.frame("Real"=sob_pand,"Estimación"=for_rf)
metricas(df_met_rf,Real.Overflight,Estimación)

df_met_xg<-data.frame("Real"=sob_pand,"Estimación"=for_xg)
metricas(df_met_xg,Real.Overflight,Estimación)


##Modelo de tendencia

des<-stl(sob_serie,s.window = "periodic")
tende<-as.data.frame(des$time.series)
tend<-tende$trend

sob_sintend<-sob_serie-tend
serie_sintend<-ts(sob_sintend,start=c(2013,1),end=c(2021,4),frequency=12)
alfa<-tail(tend,1)



ajuste_sintend<-arima(serie_sintend,order=c(2,1,0),seasonal=list(order=c(1,0,0),frequency=12))
pred_sintend<-sarima.for(serie_sintend,12,2,1,0,1,0,0,12)$pred

#Escenario pesimista
n<-length(tend)
t_pes<-seq(1:n)
reg_pes<-lm(tend~t_pes)
beta_pes<-reg_pes$coefficients[2]

val_pes=c()
for(t in 1:12){
  x=alfa+beta_pes*t
  val_pes=append(val_pes,x)
}

pred_pes<-pred_sintend+val_pes

#Escenario base
sob_base<-tend[1:82]
n<-length(sob_base)
t_base<-seq(1:n)
reg_base<-lm(sob_base~t_base)
beta_base<-reg_base$coefficients[2]

val_base=c()
for(t in 1:12){
  x=alfa+beta_base*t
  val_base=append(val_base,x)
}

pred_base<-pred_sintend+val_base

#Escenario optimista
sob_op<-tend[94:100]
n<-length(sob_op)
t_op<-seq(1:n)
reg_op<-lm(sob_op~t_op)
beta_op<-reg_op$coefficients[2]

val_op=c()
for(t in 1:12){
  x=alfa+beta_op*t
  val_op=append(val_op,x)
}

pred_op<-pred_sintend+val_op

#Escenario vacunas
beta_v<-0.8479
beta2_v<-0.5284

val_v=c()
for(t in 1:12){
  x=alfa+beta_v*exp(beta2_v*t)
  val_v=append(val_v,x)
}

pred_v<-pred_sintend+val_v

#Escenario simétrico

sim<-read_excel("Tendencia.xlsx",sheet="Simetrica sobrevuelos")
val_sim<-sim$Valor[8:19]

pred_sim<-pred_sintend+val_sim

#Predicciones
pes<-pred_pes
op<-pred_op
base<-pred_base
vac<-pred_v
sime<-pred_sim
Fecha<-pred_sob$.index[1:12]

pred_tend<-data.frame("Fecha"=Fecha,"Pesimista"=pes,"Optimista"=op,"Base"=base, "Vacuna"=vac,"Simétrico"=sime)

Fecha<-rep(Fecha,5)
Escenario<-c(rep("Pesimista",12),rep("Optimista",12),rep("Base",12),rep("Vacuna",12),rep("SimÃ©trico",12))
Vuelos<-c(pes,op,base,vac,sime)

datos <- data.frame(Fecha, Escenario,Vuelos)

ggplot(datos, aes(x=Fecha, y=Vuelos, group = Escenario, colour =Escenario )) + 
  geom_line() +theme(legend.position = "bottom")

#Corte con 2019

opt<-ts(append(Sobrevuelos$Overflight,pred_op),start=c(2013,1),end=c(2022,4),frequency=12)
bas<-ts(append(Sobrevuelos$Overflight,pred_base),start=c(2013,1),end=c(2022,4),frequency=12)
pesi<-ts(append(Sobrevuelos$Overflight,pred_pes),start=c(2013,1),end=c(2022,4),frequency=12)

nivel_prev<-Sobrevuelos$Overflight[77:86]
nivel_prev<-append(nivel_prev,Sobrevuelos$Overflight[75:76])



opt1<-sarima.for(opt,12,0,1,1,2,0,0,12)$pred
bas1<-sarima.for(bas,12,0,1,1,2,0,0,12)$pred
pesi1<-sarima.for(pesi,12,0,1,1,2,0,0,12)$pred

opt2<-ts(append(opt,opt1),start=c(2013,1),end=c(2023,4),frequency=12)
bas2<-ts(append(bas,bas1),start=c(2013,1),end=c(2023,4),frequency=12)
pesi2<-ts(append(pesi,pesi1),start=c(2013,1),end=c(2023,4),frequency=12)


opt3<-sarima.for(opt2,12,0,1,1,2,0,0,12)$pred
bas3<-sarima.for(bas2,12,0,1,1,2,0,0,12)$pred
pesi3<-sarima.for(pesi2,12,0,1,1,2,0,0,12)$pred

opti<-append(pred_op,append(opt1,opt3))
base<-append(pred_base,append(bas1,bas3))
pesim<-append(pred_pes,append(pesi1,pesi3))



fecha<-sim$Fecha[8:43]
Fecha<-rep(fecha,4)
Escenario<-c(rep("Pesimista",36),rep("Optimista",36),rep("Base",36), rep("Vuelos 2019",36))
Vuelos<-c(pesim,opti,base, rep(nivel_prev,3))

datos <- data.frame(Fecha, Escenario,Vuelos)

ggplot(datos, aes(x=Fecha, y=Vuelos, group = Escenario, colour =Escenario )) + 
  geom_line() +theme(legend.position = "bottom")+ ylab("Número de vuelos")

#Extensión modelo ARIMA
pred_sintend<-sarima.for(serie_sintend,36,2,1,0,1,0,0,12)$pred


val_pes=c()
for(t in 1:36){
  x=alfa+beta_pes*t
  val_pes=append(val_pes,x)
}

pred_pes<-pred_sintend+val_pes

val_op=c()
for(t in 1:36){
  x=alfa+beta_op*t
  val_op=append(val_op,x)
}

pred_op<-pred_sintend+val_op



val_base=c()
for(t in 1:36){
  x=alfa+beta_base*t
  val_base=append(val_base,x)
}

pred_base<-pred_sintend+val_base

fecha<-sim$Fecha[8:43]
Fecha<-rep(fecha,4)
Escenario<-c(rep("Pesimista",36),rep("Optimista",36),rep("Base",36),rep("Vuelos 2019",36))
Vuelos<-c(pred_pes,pred_op,pred_base, rep(nivel_prev,3))

datos <- data.frame(Fecha, Escenario,Vuelos)

ggplot(datos, aes(x=Fecha, y=Vuelos, group = Escenario, colour =Escenario )) + 
  geom_line() +theme(legend.position = "bottom")