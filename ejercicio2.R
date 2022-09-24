#cargo las librerias que necesito
require("data.table")
require("lightgbm")
require("beepr")
require(dplyr)

#monte carlo cross validation

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/DM-EyF")  #Establezco el Working Directory
remove(list=ls())
gc()
#cargo el dataset
dataset <- fread("./datasets/competencia2_2022.csv.gz")

semillas <- c(100621,
              102149,
              202061,
              257093,
              584723)



dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dtest  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo
dapply  <- dataset[ foto_mes==202105 ]  
remove(dataset)
### feature enginering ####
aplicarCambios<-function(base, apply=FALSE){
  
  library(readxl)
  DiccionarioDatos_version_1_xlsb <- read_excel("../DiccionarioDatos (version 1).xlsb.xls")
  DiccionarioDatos_version_1_xlsb<-as.data.table(DiccionarioDatos_version_1_xlsb)
  DiccionarioDatos_version_1_xlsb[unidad=="pesos"]->mpesos
  DiccionarioDatos_version_1_xlsb[unidad=="dias"]->mdias
  remove(DiccionarioDatos_version_1_xlsb)
  
  require("beepr")
  library(dplyr)
  base$foto_mes<-NULL
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
  
  
  
  
  
  
  #did_recode_columns(base, variables_factor, type = "as.factor")
  #did_recode_columns(base, variables_factor2, type = "as.factor")
  
  
  
  
  #rankeo todas
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
  
  
  for (n in 1:(length(mis_variables_2)-1)) {
    print(n)
    
    for (m in (n+1):length(mis_variables_2)) {
      nueva <- paste(mis_variables_2[n], mis_variables_2[m], sep = "___")
      base[, (nueva) := get(mis_variables_2[n]) * get(mis_variables_2[m])]
      
    }
  }
  
  mysum <- function(x){sum(x, na.rm=TRUE)}
  mpesos$campo<-paste0("r_", mpesos$campo)
  base[, r_tot := rowSums(.SD, na.rm = TRUE), .SDcols = mpesos$campo]  
  
  beep(sound = 1, expr = NULL)
  return(base)
}

dtrain_m<-aplicarCambios(dtrain)
remove(dtrain)


gc()
#### uso lgbm ####
clase_binaria <- ifelse(dtrain_m$clase_ternaria != "CONTINUA", 1, 0)

dtrain  <- lgb.Dataset(data = data.matrix(dtrain_m[, -c("clase_ternaria")]), label = clase_binaria)

ganancia_lgb <- function(probs, datos) {
  return(list("name" = "ganancia",
              "value" =  sum((probs > 0.05) * ifelse(getinfo(datos, "label") == 1, 78000, -2000))/ 0.2,
              "higher_better" = TRUE))
}


set.seed(semillas[1])
# LightGBM, al igual que XGB traen su implementación del CV
# Los parámetros los iremos viendo en profundidad la siguiente clase.
model_lgbm_cv <- lgb.cv(data = dtrain,
                        eval = ganancia_lgb,
                        stratified = TRUE,
                        nfold = 5,
                        param = list(objective = "binary",
                                     max_bin = 15,
                                     min_data_in_leaf = 4000,
                                     learning_rate = 0.05
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
                                    max_bin = 15,
                                    min_data_in_leaf = 4000,
                                    learning_rate = 0.05),
                      verbose = -1)

# También tiene su importancia de variables
lgb.importance(model_lgm, percentage = TRUE)

#### test ####
dtest_m<-aplicarCambios(dtest)
remove(dtest)
dtest_m$pred <- predict(model_lgm, data.matrix(dtest_m[, -c("clase_ternaria")]))

# TOTAL
sum((dtest_m$pred > 0.051) *
      ifelse(dtest_m$clase_ternaria == "BAJA+2", 78000, -2000))

#### apply ####

dapply_m<-aplicarCambios(dapply, TRUE)
remove(dapply)

dapply_m$pred <- predict(model_lgm, data.matrix(dapply_m[, -c("clase_ternaria", "numero_de_cliente")]))

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply_m[ , Predicted := as.numeric( pred > 0.05 ) ]

#dir.create( "./exp/KA3001" )

fwrite( dapply_m[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA3001/K101_01.csv",
        sep=  "," )
