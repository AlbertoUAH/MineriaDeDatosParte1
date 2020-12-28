setwd("UCM/Mineria de Datos y Modelizacion Predictiva/Practica 1/")
source("FuncionesRosa.R")

# ---------------------------- FASE 3: REGRESION LOGISTICA ------------------------------
input_bin<-readRDS("datosElectorales_Bin")

# Antes de evaluar el modelo ¿Hay alguna categoria que podamos agrupar? Para ello, recurrimos a la libreria scorecard, empleando la funcion
# woebin que nos permite saber si hay alguna posible agrupacion que mejore el IV (Information Value)
input_bin$CCAA <- recode(input_bin$CCAA, "c('CV_EX_AS_BA_CA','MA_CA_RI_CE_ME_MU_GA_CM') = 'CV_EX_AS_BA_CA_MA_CA_RI_CE_ME_MU_GA_CM'")

formInt.bin<-formulaInteracciones(input_bin, 1)

# Obtengo la particion
set.seed(12345678)
RNGkind(sample.kind = "Rejection")

trainIndex.bin <- createDataPartition(input_bin$varObjBin, p=0.8, list=FALSE)

data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]
formInt.bin<-formulaInteracciones(input_bin, 1)

# MODELO 1. Construyo un modelo preliminar con todas las variables
modelo1.bin<-glm(formInt.bin,data=data_train.bin, family = binomial)
mostrar.estadisticas(modelo1.bin, data_train.bin, data_test.bin, "glm", "varObjBin") # Aun podemos mejorarlo muchisimo...

# NOTA: Con el modelo 1.2 partimos en la seleccion de variables
# MODELO 1.2. ¿Y si eliminamos las variables con menor importancia?
importancia.var <- impVariablesLog(modelo1.bin, "varObjBin", data_train.bin)

# Dado que crear un modelo de regresion logistica es "computacionalmente" mas costoso que un modelo de regresion lineal, debemos eliminar en la medida
# de lo posibles aquellas variables que menos nos aporten a nuestro modelo, en especial de cara a la seleccion clasica. Por ello, de cara a las interacciones
# nos quedaremos con aquellos cuya importancia sea "sobresaliente" con respecto al resto, es decir, los outliers obtenidos a partir del boxplot de impVariablesLog
variables.mas.imp <- importancia.var[which(importancia.var$V5 > 0.000683), 1]
term.independientes <- unique(unlist(strsplit(variables.mas.imp, split = ":")))
formInt.bin <- paste0("varObjBin~",paste0(colnames(input_bin)[-1], collapse = "+"),"+",paste0(unlist(variables.mas.imp), collapse = "+"))

modelo1.2.bin<-glm(formInt.bin,data=data_train.bin, family = binomial)
# Aunque el AIC aumenta, el modelo se ve reducido en el numero de parametros, facilitando "computacionalmente" el proceso de seleccion clasica
mostrar.estadisticas(modelo1.2.bin, data_train.bin, data_test.bin, "glm", "varObjBin") 

# SELECCION CLASICA
estadisticas.modelos.bin <- seleccion.clasica(formInt.bin, data_train.bin, data_test.bin, "glm")
data.frame("R^2.train" = sapply(estadisticas.modelos.bin, function(x) pseudoR2(x, data_train.bin, "varObjBin")), 
           "R^2.test" = sapply(estadisticas.modelos.bin, function(x) pseudoR2(x, data_test.bin, "varObjBin")), 
           "Diferencia" = sapply(estadisticas.modelos.bin, function(x) pseudoR2(x, data_train.bin, "varObjBin") - pseudoR2(x, data_test.bin, "varObjBin")),
           "AIC" = sapply(estadisticas.modelos.bin, function(x) AIC(x)), "SBC" = sapply(estadisticas.modelos.bin, function(x) BIC(x)),
           "N.Parametros" = sapply(estadisticas.modelos.bin, function(x) x$rank))

# Mediante la validacion cruzada comparamos los modelos obtenidos en la seleccion clasica
auxVarObj<-input_bin$varObjBin
input_bin$varObjBin<-make.names(input_bin$varObjBin)
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]

validacion.cruzada.bin <- function(modelos, metodo, data_train) {
  total<-c();
  for (i in 1:length(modelos)){
    set.seed(1712)
    vcr<-train(as.formula(modelos[[i]]), data = data_train,
               method = metodo, family="binomial",metric = "ROC",
               trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                        summaryFunction=twoClassSummary,
                                        classProbs=TRUE,returnResamp="all")
    )
    total<-rbind(total,data.frame(roc=vcr$resample[,1],modelo=rep(paste("Modelo",i),
                                                                  nrow(vcr$resample))))
  }
  bx <- boxplot(roc~modelo,data=total,main="R-Square")
  print(bx$stats)
  data.frame(setNames(aggregate(roc~modelo, data = total, mean), c("modelo", "media")), sd = aggregate(roc~modelo, data = total, sd)[, 2])
}
estadisticas.modelos.final.bin <- validacion.cruzada.bin(c(estadisticas.modelos.bin), "glm", data_train.bin)
# De cara a la seleccion aleatoria nos quedaremos tanto con el primer como con el cuarto modelo, dado que la diferencia entre el train y el test en el 
# resto de modelos es negativa, lo cual es un indicativo de que pueden sobrar parametros en el modelo. Por el contrario, en el primer y cuarto modelo la
# la diferencia es positiva
estadisticas.modelos.final.bin

input_bin$varObjBin<-auxVarObj
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]

# SELECCION ALEATORIA
# Para el modelo aleatorio, aunque pueda conllevar mas tiempo, lo ejecutamos con la formula original, es decir, incluyendo todas
# las posibles variables y transformaciones
formInt.bin <- formulaInteracciones(input_bin,1)
modelos.aleatorios <- seleccion.aleatoria(formInt.bin, data_train.bin, "glm")

# Estadisticas de los modelos aleatorios
for (x in rownames(modelos.aleatorios)) { mostrar.estadisticas(glm(paste0('varObjBin~', x), data_train.bin, family = binomial), data_train.bin, data_test.bin, "glm", "varObjBin") }

auxVarObj<-input_bin$varObjBin
input_bin$varObjBin<-make.names(input_bin$varObjBin)
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]
estadisticas.modelos.final.2.bin <- validacion.cruzada.bin(c(estadisticas.modelos.bin[c(6)], paste("varObjBin~", rownames(modelos.aleatorios))), "glm", data_train.bin)
estadisticas.modelos.final.2.bin
input_bin$varObjBin<-auxVarObj
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]

# ¿Que modelo es el mejor? ¿El primero o el cuarto?
# Con el primer modelo podemos realizar un analisis ANOVA para eliminar aquellas variables que suponga menor perdida en la varianza

# MODELO FINAL
formula.final.bin <- 'varObjBin ~ CCAA + ForeignersPtge + UnemployLess25_Ptge + AgricultureUnemploymentPtge + 
    IndustryUnemploymentPtge + totalEmpresas + ActividadPpal + 
    SUPERFICIE + PobChange_pct + prop_missings + CCAA:IndustryUnemploymentPtge + 
    ForeignersPtge:ActividadPpal + CCAA:Age_over65_pct + CCAA:SameComAutonPtge'
modelo.final.bin <- glm(formula.final.bin, data_train.bin, family = binomial)

rownames(anova(modelo.final.bin, test = "Chisq"))[anova(modelo.final.bin, test = "Chisq")$`Pr(>Chi)` > 0.01][-1]

modelo.final.aleatorio.bin <- glm(paste0('varObjBin~', paste0(rownames(modelos.aleatorios)[3], collapse = "+")), data_train.bin, family = binomial)

# Modelo 1 (ANOVA)
mostrar.estadisticas(modelo.final.bin, data_train.bin, data_test.bin, "glm", "varObjBin")
# Modelo 5
mostrar.estadisticas(modelo.final.aleatorio.bin, data_train.bin, data_test.bin, "glm", "varObjBin")

# ¿Y las desviaciones tipicas?
auxVarObj<-input_bin$varObjBin
input_bin$varObjBin<-make.names(input_bin$varObjBin)
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]
estadisticas.modelos.final.2.bin <- validacion.cruzada.bin(c(formula.final.bin, paste0('varObjBin~', paste0(rownames(modelos.aleatorios)[2], collapse = "+"))), "glm", data_train.bin)
estadisticas.modelos.final.2.bin
input_bin$varObjBin<-auxVarObj
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]

# MODELO GANADOR: Modelo 5
formula.final.bin <- paste0('varObjBin~', paste0(rownames(modelos.aleatorios)[3], collapse = "+"))

# Aparentemente, todo apunta a que el modelo 1 parece ser la mejor opcion (tanto en sd como en media, diferencia entre train y test, asi como en AIC).
# Sin embargo, si echamos un vistazo a la importancia de sus variables...
# Modelo 1
summary(impVariablesLog(modelo.final.bin, "varObjBin", data_train.bin)$V5)
# Modelo 5
summary(impVariablesLog(modelo.final.aleatorio.bin, "varObjBin", data_train.bin)$V5)
# Tanto la media como la mediana y el resto de cuartiles nos permite comprobar que la importancia de las variables en el modelo 5 es mayor al del modelo 1
# lo cual puede deberse, entre otros motivos, al menor numero de parametros. Ademas, el modelo 1 esta prediciendo en todo momento probabilidades absolutas (0,1)
# mientras que el modelo 5 no. Esto no tiene porque ser "algo malo", ya que esto supondria que la variable objetivo esta siendo clasificada "perfectamente" con
# un pequeno conjunto de variables, aunque esto no tiene porque ser asi de cara a un nuevo conjunto de datos. Por tanto, elegimos como modelo ganador el modelo 5

# BUSCAMOS EL MEJOR PUNTO DE CORTE
## Generamos una rejilla con los posibles puntos de corte
posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,function(x) sensEspCorte(modelo.final.aleatorio.bin,data_test.bin,"varObjBin",x,"1")))))
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1
plot(rejilla$posiblesCortes,rejilla$Youden)
plot(rejilla$posiblesCortes,rejilla$Accuracy)
cat("Max. Indice Youden: ", rejilla$posiblesCortes[which.max(rejilla$Youden)], "\n")
cat("Max. Precision/Accuracy: ", rejilla$posiblesCortes[which.max(rejilla$Accuracy)], "\n")

# Comparamos ambos puntos de corte
# Indice Youden
sensEspCorte(modelo.final.aleatorio.bin,data_test.bin,"varObjBin",0.64,"1")

# Indice Precision/Accuracy
sensEspCorte(modelo.final.aleatorio.bin,data_test.bin,"varObjBin",0.53,"1")

# Evaluacion del modelo ganador, empleando el indice de Youden
roc(data_train.bin$varObjBin, predict(modelo.final.aleatorio.bin,data_train.bin,type = "response"))
roc(data_test.bin$varObjBin, predict(modelo.final.aleatorio.bin,data_test.bin,type = "response"))
mostrar.estadisticas(modelo.final.aleatorio.bin, data_train.bin, data_test.bin, "glm", "varObjBin")
summary(modelo.final.aleatorio.bin)

sensEspCorte(modelo.final.aleatorio.bin,data_train.bin,"varObjBin",0.64,"1")
sensEspCorte(modelo.final.aleatorio.bin,data_test.bin,"varObjBin",0.64,"1")




