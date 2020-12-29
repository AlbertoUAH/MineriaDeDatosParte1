setwd("UCM/Mineria de Datos y Modelizacion Predictiva/Practica 1/")
source("FuncionesRosa.R")

# ---------------------------- FASE 3: REGRESION LOGISTICA ------------------------------
input_bin<-readRDS("datosElectorales_Bin")

formInt.bin<-formulaInteracciones(input_bin, 1)

# Obtengo la particion
set.seed(12345678)
RNGkind(sample.kind = "Rejection")

trainIndex.bin <- createDataPartition(input_bin$varObjBin, p=0.8, list=FALSE)

data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]

# MODELO 1. Construyo un modelo preliminar con todas las variables
modelo1.bin<-glm(formInt.bin,data=data_train.bin, family = binomial)
mostrar.estadisticas(modelo1.bin, data_train.bin, data_test.bin, "glm", "varObjBin") # Aun podemos mejorarlo muchisimo...

input_bin_copia <- input_bin
# Antes de evaluar el modelo ¿Hay alguna categoria que podamos agrupar? Para ello, recurrimos a la libreria scorecard, empleando la funcion
# woebin que nos permite saber si hay alguna posible agrupacion que mejore el IV (Information Value)
library(scorecard)
salida.woe.ccaa <- woebin(data.frame(input_bin[, c("varObjBin", "CCAA")]), "varObjBin", print_step = 0)
salida.woe.ccaa$CCAA$breaks
input_bin_copia$CCAA <- recode(input_bin_copia$CCAA, "c('CV_EX_AS_BA_CA','MA_CA_RI_CE_ME_MU_GA') = 'CV_EX_AS_BA_CA_MA_CA_RI_CE_ME_MU_GA_CM'")
data_train_copia.bin <- input_bin_copia[trainIndex.bin,]
data_test_copia.bin <- input_bin_copia[-trainIndex.bin,]

# MODELO 1 COPIA. Construyo un modelo preliminar con todas las variables
modelo1.bin.copia<-glm(formInt.bin,data=data_train_copia.bin, family = binomial)
mostrar.estadisticas(modelo1.bin.copia, data_train_copia.bin, data_test_copia.bin, "glm", "varObjBin")

# NOTA: Con el modelo 1.2 partimos en la seleccion de variables
# MODELO 1.2. ¿Y si eliminamos las variables con menor importancia?
importancia.var <- impVariablesLog(modelo1.bin, "varObjBin", data_train.bin)

# Dado que crear un modelo de regresion logistica es "computacionalmente" mas costoso que un modelo de regresion lineal, debemos eliminar en la medida
# de lo posibles aquellas variables que menos nos aporten a nuestro modelo, en especial de cara a la seleccion clasica. Por ello, de cara a las interacciones
# nos quedaremos con aquellos cuya importancia sea "sobresaliente" con respecto al resto, es decir, los outliers obtenidos a partir del boxplot de impVariablesLog
summary(importancia.var$V5)
variables.mas.imp <- importancia.var[which(importancia.var$V5 > 8.550e-04), "V2"] # 9.168e-04
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


auxVarObj<-input_bin$varObjBin
input_bin$varObjBin<-make.names(input_bin$varObjBin)
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]
estadisticas.modelos.final.bin <- validacion.cruzada.bin(c(estadisticas.modelos.bin[c(1,2,6)]), "glm", data_train.bin)
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
estadisticas.modelos.final.2.bin <- validacion.cruzada.bin(c(estadisticas.modelos.bin[c(2,6)], paste("varObjBin~", rownames(modelos.aleatorios))), "glm", data_train.bin)
estadisticas.modelos.final.2.bin
input_bin$varObjBin<-auxVarObj
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]

# ¿Que modelo es el mejor? ¿El primero o el cuarto?
# Con el primer modelo podemos realizar un analisis ANOVA para eliminar aquellas variables que suponga menor perdida en la varianza

# MODELO FINAL
formula.final.bin <- 'varObjBin ~ CCAA + ForeignersPtge + AgricultureUnemploymentPtge + 
    prop_missings + Age_over65_pct + PobChange_pct + SUPERFICIE + 
    Population + SameComAutonDiffProvPtge + CCAA:Population'
modelo.final.bin <- glm(formula.final.bin, data_train.bin, family = binomial)

impVariablesLog(estadisticas.modelos.bin[6]$`SBC-backward`, "varObjBin", data_train.bin)[which(impVariablesLog(estadisticas.modelos.bin[6]$`SBC-backward`, "varObjBin", data_train.bin)$V5 < 0.002), ]

formula.final.bin.2 <- 'varObjBin ~ CCAA + 
    ForeignersPtge + SameComAutonPtge + SameComAutonDiffProvPtge + ActividadPpal + 
    PersonasInmueble + prop_missings + CCAA:PersonasInmueble + 
    CCAA:SameComAutonPtge + ForeignersPtge:ActividadPpal + CCAA:prop_missings + 
    CCAA:SameComAutonDiffProvPtge'

modelo.final.bin.2 <- glm(formula.final.bin.2, data_train.bin, family = binomial)
mostrar.estadisticas(modelo.final.bin, data_train.bin, data_test.bin, "glm", "varObjBin")
mostrar.estadisticas(modelo.final.bin.2, data_train.bin, data_test.bin, "glm", "varObjBin")

summary(summary(modelo.final.bin)$coefficients[, 4])
summary(summary(modelo.final.bin.2)$coefficients[, 4])

# ¿Y las desviaciones tipicas?
auxVarObj<-input_bin$varObjBin
input_bin$varObjBin<-make.names(input_bin$varObjBin)
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]
estadisticas.modelos.final.3.bin <- validacion.cruzada.bin(c(formula.final.bin, formula.final.bin.2), "glm", data_train.bin)
estadisticas.modelos.final.3.bin
input_bin$varObjBin<-auxVarObj
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]

# MODELO GANADOR: Modelo 2
# BUSCAMOS EL MEJOR PUNTO DE CORTE
## Generamos una rejilla con los posibles puntos de corte
posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,function(x) sensEspCorte(modelo.final.bin,data_test.bin,"varObjBin",x,"1")))))
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1
plot(rejilla$posiblesCortes,rejilla$Accuracy, col="red")
points(rejilla$posiblesCortes,rejilla$Youden,col="blue")
legend("topleft",legend = c("Youden", "Accuracy"), col = c("red", "blue"), pch = c(16, 16), cex = 0.5)
cat("Max. Indice Youden: ", rejilla$posiblesCortes[which.max(rejilla$Youden)], "\n")
cat("Max. Precision/Accuracy: ", rejilla$posiblesCortes[which.max(rejilla$Accuracy)], "\n")

# Comparamos ambos puntos de corte
# Indice Youden
sensEspCorte(modelo.final.bin,data_test.bin,"varObjBin",0.57,"1")

# Indice Precision/Accuracy
sensEspCorte(modelo.final.bin,data_test.bin,"varObjBin",0.5,"1")

# Evaluacion del modelo ganador, empleando el indice de Youden
sensEspCorte(modelo.final.bin,data_train.bin,"varObjBin",0.5,"1")
sensEspCorte(modelo.final.bin,data_test.bin,"varObjBin",0.5,"1")

mostrar.estadisticas(modelo.final.bin, data_train.bin, data_test.bin, "glm", "varObjBin")
summary(modelo.final.bin)
roc(data_train.bin$varObjBin, predict(modelo.final.bin,data_train.bin,type = "response"))
roc(data_test.bin$varObjBin, predict(modelo.final.bin,data_test.bin,type = "response"))
impVariablesLog(modelo.final.bin, "varObjBin", data_train.bin)
