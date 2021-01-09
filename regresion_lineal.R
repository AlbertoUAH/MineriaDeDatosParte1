# Autor: Fernandez Hernandez, Alberto
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

# ¿Y si agrupamos MA_CA_RI_CE_ME_MU_GA con AR_CM?
input_cont_copia <- input_cont
input_cont_copia$CCAA <- recode(input_cont_copia$CCAA, "c('MA_CA_RI_CE_ME_MU_GA', 'AR_CM') = 'MA_CA_RI_CE_ME_MU_GA_AR_CM';")
data_train_copia <- input_cont_copia[trainIndex,]
data_test_copia <- input_cont_copia[-trainIndex,]

# MODELO 1 (COPIA). Aumenta la diferencia entre el R2 train y test, ademas del AIC, por lo que dejamos las categorias sin agrupar
modelo1.copia<-lm(formInt,data=data_train_copia)
mostrar.estadisticas(modelo1.copia, data_train_copia, data_test_copia, "lm", "varObjCont")

# MODELO 1.2
# No obstante, aun podemos mejorar el modelo. Seguimos teniendo demasiadas variables
variacion.r2 <- modelEffectSizes(modelo1, Print = FALSE)
summary(variacion.r2$Effects[, 4]) # El 75 % de las variables suponen una importancia de 0.0003355 en el R2 o menos
# Hay mucha diferencia entre el tercer cuartil y el valor maximo, por lo que nos centramos en las interacciones "atipicas" (outliers) que mayor importancia aporten al R2
variables.mas.imp <- names(boxplot(variacion.r2$Effects[, 4], plot = FALSE)$out)

formInt <- paste0("varObjCont~",paste0(colnames(input_cont[-1]), collapse = "+"),"+",paste0(variables.mas.imp[c(-1)], collapse = "+"))
modelo1.2<-lm(as.formula(formInt),data=data_train)
# Mejora el R^2 test, aunque baja ligeramente R^2 train (lo cual reduce la diferencia entre ambos R^2). Aunque el AIC aumenta en algo mas de 100 puntos, el SBC mejora en mas de 1000
# El numero de variables tambien se ve reducido, pasando de 234 a 45 parametros
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

# De cara a la comparacion final, nos quedamos con el sexto modelo
estadisticas.modelos.final <- validacion.cruzada(c(estadisticas.modelos), "lm", data_train)
estadisticas.modelos.final

# ¿Que diferencia al modelo 6 del modelo 1?
cat("¿Que tiene el modelo 2 que no tenga el modelo 6?", Reduce(setdiff, strsplit(c(as.character(estadisticas.modelos[2]$`SBC-both`$call)[2], 
                                                                                   as.character(estadisticas.modelos[6]$`SBC-backward`$call)[2]), split = " ")), "\n")
cat("¿Que tiene el modelo 6 que no tenga el modelo 2?", Reduce(setdiff, strsplit(c(as.character(estadisticas.modelos[6]$`SBC-backward`$call)[2], 
                                                                                   as.character(estadisticas.modelos[2]$`SBC-both`$call)[2]), split = " ")), "\n")
summary(summary(lm(estadisticas.modelos[2]$`SBC-both`, data_train))$coefficients[,4])
summary(summary(lm(estadisticas.modelos[6]$`SBC-backward`, data_train))$coefficients[,4])

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
cor(Filter(is.numeric, input_cont)[c(3,4,6,7,9,10,12,22,26,27)], use="pairwise", method="pearson")
modelEffectSizes(estadisticas.modelos[6]$`SBC-backward`)
formula.final <-  'varObjCont ~ CCAA + Age_over65_pct + 
    SameComAutonPtge + IndustryUnemploymentPtge + 
    ServicesUnemploymentPtge + logxConstructionUnemploymentPtge + CCAA:SameComAutonPtge'
modelo.final <- lm(as.formula(formula.final), data = data_train)
nueva.desv <- validacion.cruzada(c(formula.final), "lm", data_train)
mostrar.estadisticas(modelo.final, data_train, data_test, "lm", "varObjCont")
summary(modelo.final)
modelEffectSizes(modelo.final)

# Conseguimos una maxima correlacion positiva de 0.32
max(unique(c(cor(Filter(is.numeric, input_cont)[,c(4,6,7,9,10,26)], use="pairwise", method="pearson")))[-1])
# Ademas de una correlacion minima de -0.38
min(unique(c(cor(Filter(is.numeric, input_cont)[,c(4,6,7,9,10,26)], use="pairwise", method="pearson")))[-1])

# ¿Y si volvemos a agrupar MA_CA_RI_CE_ME_MU_GA con AR_CM? Parece que empeora
modelo.final.copia2 <- lm(as.formula(formula.final), data = data_train_copia)
nueva.desv.copia <- validacion.cruzada(c(formula.final), "lm", data_train_copia)
mostrar.estadisticas(modelo.final.copia2, data_train_copia, data_test_copia, "lm", "varObjCont")





