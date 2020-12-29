setwd("UCM/Mineria de Datos y Modelizacion Predictiva/Practica 1/")
source("FuncionesRosa.R")


mostrar.estadisticas <- function(modelo, data_train, data_test, tipo = "lm", varObj) {
  if (tipo != "lm") {
    cat("Train: ", pseudoR2(modelo,data_train,varObj), "; Test: ", pseudoR2(modelo,data_test,varObj), "; ")
    cat("Dif. (Train-Test): ", pseudoR2(modelo,data_train,varObj) - pseudoR2(modelo,data_test,varObj), "; ")
  } else {
    cat("Train: ", Rsq(modelo,varObj,data_train), "; Test: ", Rsq(modelo,varObj,data_test), "; ")
    cat("Dif. (Train-Test): ", Rsq(modelo,varObj,data_train) - Rsq(modelo,varObj,data_test), "; ")
  }
  cat("AIC:" , AIC(modelo), "; SBC: ", BIC(modelo), "\n")
  cat("Numero de variables: ", length(coef(modelo)), "\n")
}

# ---------------------------- FASE 2: REGRESION LINEAL ------------------------------
input_cont<-readRDS("datosElectorales_Cont")

formInt<-formulaInteracciones(input_cont,1)

# Obtengo la particion
set.seed(12345678)
RNGkind(sample.kind = "Rejection")

trainIndex <- createDataPartition(input_cont$varObjCont, p=0.8, list=FALSE)
data_train <- input_cont[trainIndex,]
data_test <- input_cont[-trainIndex,]

# MODELO 1. Construyo un modelo preliminar con todas las variables
modelo1<-lm(formInt,data=data_train)
mostrar.estadisticas(modelo1, data_train, data_test, "lm", "varObjCont")

input_cont_copia <- input_cont
input_cont_copia$CCAA <- recode(input_cont_copia$CCAA, "c('MA_CA_RI_CE_ME_MU_GA', 'AR_CM') = 'MA_CA_RI_CE_ME_MU_GA_AR_CM';")
data_train_copia <- input_cont_copia[trainIndex,]
data_test_copia <- input_cont_copia[-trainIndex,]

# MODELO 1. Construyo un modelo preliminar con todas las variables
modelo1.copia<-lm(formInt,data=data_train_copia)
mostrar.estadisticas(modelo1.copia, data_train_copia, data_test_copia, "lm", "varObjCont")


# MODELO 1.2
# No obstante, aun podemos mejorar el modelo. Seguimos teniendo demasiadas variables
variacion.r2 <- modelEffectSizes(modelo1)
summary(variacion.r2$Effects[, 4]) # El 75 % de las variables suponen una importancia de 0.0003239 en el R2 o menos
variables.mas.imp <- names(boxplot(variacion.r2$Effects[, 4], plot = FALSE)$out)# rownames(variacion.r2$Effects[variacion.r2$Effects[, 4] > 0.0003239, ])[c(-1)] # Nos quedamos con el 25 % restante

formInt <- paste0("varObjCont~",paste0(colnames(input_cont[-1]), collapse = "+"),"+",paste0(variables.mas.imp, collapse = "+"))
modelo1.2<-lm(as.formula(formInt),data=data_train)
# Mejora el R^2 test, aunque baja ligeramente R^2 train (lo cual reduce la diferencia entre ambos R^2)
# El numero de variables tambien se ve reducido, pasando de 224 a 93 parametros
mostrar.estadisticas(modelo1.2, data_train, data_test, "lm", "varObjCont")

# NOTA: Con el modelo 1.2 partimos en la seleccion de variables
# MODELO 2. Seleccion de variables clasica
# Creamos una funcion para la seleccion clasica, que nos sirva tanto para la regresion lineal como logistica
seleccion.clasica <- function(formInt, data_train, data_test, tipo) {
  if(tipo == "lm"){
    null<-lm(varObjCont~1, data=data_train) #Modelo minimo
    full<-lm(formInt, data=data_train) #Modelo maximo
  }else {
    null<-glm(varObjBin~1, data=data_train, family = binomial)
    full<-glm(formInt, data=data_train, family= binomial)
  }
  estadisticas.modelos <- list();
  for (direccion in c("both", "forward")) {
    modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction=direccion)
    estadisticas.modelos[paste0("AIC-",direccion)] <- list(modeloStepAIC)
    
    modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction=direccion,k=log(nrow(data_train)))
    estadisticas.modelos[paste0("SBC-",direccion)] <- list(modeloStepBIC)
  }

  modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward")
  estadisticas.modelos["AIC-backward"] <- list(modeloBackAIC)
  modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",k=log(nrow(data_train)))
  estadisticas.modelos["SBC-backward"] <- list(modeloBackBIC)
  return(estadisticas.modelos)
}
estadisticas.modelos <- seleccion.clasica(formInt, data_train, data_test, "lm")
# A primera vista, los modelos 2,4 y 6 presentan una menor diferencia train-test ¿Posibles modelos candidatos?
data.frame("R^2.train" = sapply(estadisticas.modelos, function(x) Rsq(x, "varObjCont", data_train)), 
           "R^2.test" = sapply(estadisticas.modelos, function(x) Rsq(x, "varObjCont", data_test)), 
           "Diferencia" = sapply(estadisticas.modelos, function(x) Rsq(x, "varObjCont", data_train) - Rsq(x, "varObjCont", data_test)),
           "AIC" = sapply(estadisticas.modelos, function(x) AIC(x)), "SBC" = sapply(estadisticas.modelos, function(x) BIC(x)),
           "N.Parametros" = sapply(estadisticas.modelos, function(x) length(coef(x))))

# Una vez obtenidos los modelos en la seleccion clasica, mediante una funcion realizamos una validacion cruzada en 5 subdivisiones
# de 20 repeticiones cada uno
validacion.cruzada <- function(modelos, metodo, data_train) {
  total<-c();
  for (i in 1:length(modelos)){
    set.seed(1712)
    vcr<-train(as.formula(modelos[[i]]), data = data_train,
               method = metodo,
               trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                        returnResamp="all")
    )
    total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                           nrow(vcr$resample))))
  }
  bx <- boxplot(Rsquared~modelo,data=total,main="R-Square")
  print(bx$stats)
  data.frame(setNames(aggregate(Rsquared~modelo, data = total, mean), c("modelo", "media")), sd = aggregate(Rsquared~modelo, data = total, sd)[, 2])
}

# De cara a la comparacion final, nos quedamos con el sexto modelo, por varios motivos: en primer lugar, aunque no sean el
# modelo con menos parametros presenta una diferencia baja entre los valores obtenidos entre los datos de entrenamiento y prueba. Ademas, si
# lo comparamos con los modelos 2 y 4 presenta un AIC y SBC mas bajo, ademas de una desviacion tipica menor en su R2 (mejor bonda media). 
# Dado que son 44 parametros, en caso de resultar ser el modelo ganador podemos eliminar aquellas variables con menor significancia
estadisticas.modelos.final <- validacion.cruzada(c(estadisticas.modelos), "lm", data_train)
estadisticas.modelos.final

# MODELO 3. Seleccion de variables aleatoria
seleccion.aleatoria <- function(formInt, data_train, tipo){
  rep<-100
  prop<-0.7 # Se realiza con el 70% de los datos de entrenamiento por velocidad
  modelosGenerados<-c()
  for (i in 1:rep){
    set.seed(12345+i)
    subsample<-data_train[sample(1:nrow(data_train),prop*nrow(data_train),replace = T),]
    if (tipo == "lm") {
      full<-lm(formInt,data=subsample)
      null<-lm(varObjCont~1,data=subsample)
    }else {
      full<-glm(formInt,data=subsample,family=binomial)
      null<-glm(varObjBin~1,data=subsample,family=binomial)
    }
    modeloAux<-step(null,scope=list(lower=null,upper=full),direction="both",trace=0,k=log(nrow(subsample)))
    modelosGenerados<-c(modelosGenerados,paste(sort(unlist(strsplit(as.character(formula(modeloAux))[3]," [+] "))),collapse = "+"))
  }
  head(freq(modelosGenerados,sort="dec"), 3) # Elegimos los 3 mejores modelos aleatorios
}
formInt <- formulaInteracciones(input_cont,1)
modelos.aleatorios <- seleccion.aleatoria(formInt, data_train, "lm")

# Obtenemos las estadisticas de los modelos aleatorios (R2 train - R2 test - diferencia train y test - AIC - SBC - Numero de parametros)
for (x in rownames(modelos.aleatorios)) { mostrar.estadisticas(lm(paste0('varObjCont~', x), data_train), data_train, data_test, "lm", "varObjCont") }

estadisticas.modelos.final.2 <- validacion.cruzada(c(estadisticas.modelos[c(6)], paste("varObjCont~", rownames(modelos.aleatorios))), "lm", data_train)
estadisticas.modelos.final.2

# Modelo ganador: como modelo de regresion lineal elegimos el sexto modelo obtenido en la seleccion clasica, no solo por los menores valores de AIC y SBC
# obtenidos en este modelo, sino que ademas la desviacion estandar en los valores de R2 es mas pequena

#  MODELO FINAL
formula.final <-  'varObjCont ~ CCAA + Age_19_65_pct + SameComAutonPtge + 
    prop_missings + logxForeignersPtge + logxIndustryUnemploymentPtge + 
    logxServicesUnemploymentPtge + logxtotalEmpresas + CCAA:SameComAutonPtge + 
    CCAA:logxForeignersPtge'

modelo.final <- lm(as.formula(formula.final), data = data_train)

# Se reduce la desviacion tipica (de 0.01342411 a 0.0133745), aunque el valor medio de R2 disminuye ligeramente (de 0.7248011 a 0.7234081)
# Evaluacion del modelo ganador
validacion.cruzada(c(formula.final), "lm", data_train)
mostrar.estadisticas(modelo.final, data_train, data_test, "lm", "varObjCont")
summary(modelo.final)
modelEffectSizes(modelo.final)
