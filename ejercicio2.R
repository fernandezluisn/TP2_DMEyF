#cargo las librerias que necesito
# payrroll sobre edad
# ver envíos
require("data.table")
require("lightgbm")
require("beepr")
require(dplyr)

#monte carlo cross validation

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/TP2_dmEyF")  #Establezco el Working Directory
remove(list=ls())
gc()
#cargo el dataset
dataset <- fread("../datasets/competencia2_2022.csv.gz")
pruebasv3 <- read.delim("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/TP2_dmEyF/exp/HT7231/HT7231.txt")
pruebasv2 <- read.delim("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/TP2_dmEyF/exp/pruebas v2/HT7231.txt")

pruebasv1 <- read.delim("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/TP2_dmEyF/exp/pruebas v1/HT7231.txt")
semillas <- c(100621,
              102149,
              202061,
              257093,
              584723)



dtrain  <- dataset[ foto_mes==202103 ]  #defino donde voy a entrenar
dtest  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar

dapply  <- dataset[ foto_mes==202105 ]  
remove(dataset)
### feature enginering ####
aplicarCambios<-function(base, apply=FALSE, ranking=FALSE){
  
  library(readxl)
  DiccionarioDatos_version_1_xlsb <- read_excel("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/DiccionarioDatos (version 1).xlsb.xls")
  DiccionarioDatos_version_1_xlsb<-as.data.table(DiccionarioDatos_version_1_xlsb)
  DiccionarioDatos_version_1_xlsb[unidad=="pesos"]->mpesos
  DiccionarioDatos_version_1_xlsb[unidad=="dias"]->mdias
  remove(DiccionarioDatos_version_1_xlsb)
  
  base[  , ctrx_quarter_normalizado := ctrx_quarter ]
  base[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  base[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  base[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]
  
  require("beepr")
  library(dplyr)
  #base$foto_mes<-NULL
  if(apply==FALSE){
    base$numero_de_cliente<-NULL
    
  }
  base$nulos<-apply(X = is.na(base), MARGIN = 1, FUN = sum)
  
  #base$log_ctrx_quarter<-log(base$ctrx_quarter)
  
  base$moroso2<-paste0(base$Visa_delinquency, base$Master_delinquency)
  #base$cierre2<-paste0(base$Visa_status, base$Master_status)
  
  #base$cierres<-ifelse(base$Visa_status %in% c(6,7,9) & base$Master_status %in% c(6,7,9),1,0)
  
  
  variables_factor2 <-c(  "moroso2"
                          #,"cierre2"
                          #,"cierres"
  )
  
  
  
  base[ , campo1 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales <2 ) ]
  base[ , campo2 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales>=2 ) ]
  
  base[ , campo3 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro <2601.1 ) ]
  base[ , campo4 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro>=2601.1 ) ]
  
  base[ , campo5 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status>=8 | is.na(Master_status) ) ) ]
  base[ , campo6 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status <8 & !is.na(Master_status) ) ) ]
  
  base[ , campo7 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
  base[ , campo8 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]
  
  
  base[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  base[ , mv_status02       := Master_status +  Visa_status ]
  base[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  base[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  base[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]
  
  base[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]
  
  base[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]
  
  
  #combino MasterCard y Visa
  base[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
  
  base[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  base[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  base[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  base[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  base[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  base[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  base[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  base[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  base[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  base[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  base[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  base[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  base[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  base[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  base[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  base[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  base[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  base[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  base[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
  
  #a partir de aqui juego con la suma de Mastercard y Visa
  base[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  base[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  base[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  base[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  base[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  base[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  base[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  base[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  base[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  base[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  base[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  base[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  base[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  base[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  base[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  base[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]
  
  
  
  #did_recode_columns(base, variables_factor, type = "as.factor")
  #did_recode_columns(base, variables_factor2, type = "as.factor")
  
  
  
  
  #rankeo todas
  if(ranking){
    
    prefix <- "r_"
    library(readxl)
    
    mis_variables<-mpesos$campo
    
    for (var in mis_variables) {
      if(is.numeric(base[,get(var)])){
        base[, (paste(prefix, var, sep = "")) := ntile(get(var), 50)]
        
      }
    }
    
    mis_variables<-mdias$campo
    
    for (var in mis_variables) {
      if(is.numeric(base[,get(var)])){
        base[, (paste(prefix, var, sep = "")) := ntile(get(var), 15)]
        
      }
    }
    
    base[,mpesos$campo]<-NULL
    base[,mdias$campo]<-NULL
    
    mis_variables_2 <- c("ctrx_quarter",
                         "ccaja_ahorro",
                         "cdescubierto_preacordado",
                         "ctarjeta_visa",
                         "ctarjeta_debito",
                         "nulos",
                         "r_mcuenta_corriente",
                         "r_mcuentas_saldo",
                         "r_mcaja_ahorro",
                         "r_mprestamos_personales",
                         "cprestamos_personales",
                         "ccomisiones_otras",
                         "r_mcomisiones_mantenimiento",
                         "ccomisiones_mantenimiento",
                         "r_Visa_fechaalta",
                         "r_Visa_fultimo_cierre"
    ) 
    
  }else{
    mis_variables_2 <- c("ctrx_quarter",
                         "ccaja_ahorro",
                         "cdescubierto_preacordado",
                         "ctarjeta_visa",
                         "ctarjeta_debito",
                         "nulos",
                         "mcuenta_corriente",
                         "mcuentas_saldo",
                         "mcaja_ahorro",
                         "mprestamos_personales",
                         "cprestamos_personales",
                         "ccomisiones_otras",
                         "mcomisiones_mantenimiento",
                         "ccomisiones_mantenimiento",
                         "Visa_fechaalta",
                         "Visa_fultimo_cierre"
    ) 
  }
  
  
  
  
  
  
  
  for (n in 1:(length(mis_variables_2)-1)) {
    print(n)
    
    for (m in (n+1):length(mis_variables_2)) {
      nueva <- paste(mis_variables_2[n], mis_variables_2[m], sep = "___")
      base[, (nueva) := get(mis_variables_2[n]) * get(mis_variables_2[m])]
      
    }
  }
  
  mysum <- function(x){sum(x, na.rm=TRUE)}
  if(ranking){
    mpesos$campo<-paste0("r_", mpesos$campo)
    
  }
  base[, r_tot := rowSums(.SD, na.rm = TRUE), .SDcols = mpesos$campo]  
  
  beep(sound = 1, expr = NULL)
  return(base)
}

dtrain_m<-aplicarCambios(dtrain)
remove(dtrain)

gc()

#### canaritos ####

for (i in 1:20)  {
  dtrain_m[, paste0("canarito", i) := runif(nrow(dtrain_m))]
}

#### train lgbm ####
# Clase BAJA+1 y BAJA+2 juntas
clase_binaria <- ifelse(dtrain_m$clase_ternaria == "CONTINUA", 0, 1)
clase_real <- dtrain_m$clase_ternaria
dtrain_m$clase_ternaria <- NULL

dtrain  <- lgb.Dataset(data   = data.matrix(dtrain_m),
                       label  = clase_binaria,
                       # Truco jedi!
                       weight = ifelse(clase_real == "BAJA+2", 1.0000001, 1.0))

# Veremos en detalle esta funciÃ³n un poco mÃ¡s adelante
ganancia_lgbm  <- function(probs, datos) {
  ## Ingresar su estrategia! acÃ¡ vamos a ir simplemente buscando la mÃ¡xima gan de la curva.
  gan <- data.table("pred" = probs,
                    # truco para separar las clases
                    "gan" = ifelse(getinfo(datos, "label") == 1 & getinfo(datos, "weight") > 1, 78000, -2000))
  setorder(gan, -pred)
  gan[, gan_acum :=  cumsum(gan)]
  return(list("name" = "ganancia",
              "value" = gan[, max(gan_acum)] / 0.2,
              "higher_better" = TRUE))
}


set.seed(semillas[1])
# LightGBM, al igual que XGB traen su implementación del CV
# Los parámetros los iremos viendo en profundidad la siguiente clase.

pruebasv1[pruebasv1$ganancia==max(pruebasv1$ganancia),]->maxgan

model_lgbm_cv <- lgb.cv(data = dtrain,
                        eval = ganancia_lgbm,
                        stratified = TRUE,
                        nfold = 5,
                        param = list(objective = "binary",
                                     max_depth=(-1),
                                     max_bin = 31,
                                     min_data_in_leaf = maxgan$min_data_in_leaf,
                                     learning_rate = maxgan$learning_rate,
                                     feature_fraction=maxgan$feature_fraction,
                                     num_leaves=maxgan$num_leaves,
                                     num_iterations = round(maxgan$num_iterations*1.2,0) #ver esto
                        )
)

# Mejor iteración
model_lgbm_cv$best_iter

# Ganancia de la mejor iteración
unlist(model_lgbm_cv$record_evals$valid$ganancia$eval)[model_lgbm_cv$best_iter]


# Una vez que elegimos los parámetros tenemos que entrenar con todos.
model_lgm <- lightgbm(data = dtrain,
                      nrounds = model_lgbm_cv$best_iter, # <--- OJO! Double Descent alert
                      params = list(objective = "binary",
                                    max_depth=(-1),
                                    max_bin = 31,
                                    min_data_in_leaf = maxgan$min_data_in_leaf,
                                    learning_rate = maxgan$learning_rate,
                                    feature_fraction=maxgan$feature_fraction,
                                    num_leaves=maxgan$num_leaves),
                      verbose = -1)

# También tiene su importancia de variables
lgb.importance(model_lgm, percentage = TRUE)->importancia

list_canaritos <- grepl("canarito", importancia$Feature)

# Cuantos canaritos aparecieron?
length(importancia[list_canaritos])

hist(dtrain_m$ctrx_quarter)
hist(dapply$ctrx_quarter)

hist(dtrain_m$ctrx_quarter___ccomisiones_otras)
hist(dapply_m$ctrx_quarter___ccomisiones_otras)

hist(dtrain_m$cpayroll_trx)
hist(dapply_m$cpayroll_trx)

hist(dtrain_m$ctrx_quarter___cdescubierto_preacordado)
hist(dapply_m$ctrx_quarter___cdescubierto_preacordado)

hist(dtrain_m$ctrx_quarter___ctarjeta_visa)
hist(dapply_m$ctrx_quarter___ctarjeta_visa)

hist(dtrain_m$mcuentas_saldo)
hist(dapply_m$mcuentas_saldo)

hist(dtrain_m$mpayroll)
hist(dapply_m$mpayroll)

# En que posiciones
# idx <- seq(length(list_canaritos))
# idx[list_canaritos]

# importancia[1:84,]->columnas_finales
# columnas_finales[-66,]->columnas_finales
# 
# dtrain_m %>% select(columnas_finales$Feature)->base_pruebas
# 
# base_pruebas <- read.csv("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/datasets/base_pruebas.csv")
# base_pruebas$clase_ternaria<-dtrain$clase_ternaria
# 
# write.csv(base_pruebas,"../datasets/base_pruebas.csv", row.names = FALSE)

#### test ####
dtest_m<-aplicarCambios(dtest)
remove(dtest)
dtest_m$pred <- predict(model_lgm, data.matrix(dtest_m[, -c("clase_ternaria")]))

dtest_m[order(dtest_m$pred, # Sequence of vectors of the same length
              decreasing = TRUE),]->dtest_m

dtest_m[ 1:9100, Predicted := 1 ]
dtest_m[ 9101:nrow(dtest_m), Predicted := 0 ]

# TOTAL
sum((dtest_m$pred > 0.052) *
      ifelse(dtest_m$clase_ternaria == "BAJA+2", 78000, -2000))

sum((dtest_m$Predicted ==1) *
      ifelse(dtest_m$clase_ternaria == "BAJA+2", 78000, -2000))

#### apply ####

dapply_m<-aplicarCambios(dapply, TRUE)
remove(dapply)

dapply_m$pred <- predict(model_lgm, data.matrix(dapply_m[, -c("clase_ternaria", "numero_de_cliente")]))

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply_m[ , Predicted := as.numeric( pred > 0.052 ) ]

#dir.create( "./exp/resultados" )

fwrite( dapply_m[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/resultados/K101_04.csv",
        sep=  "," )

