# Autor: Fernandez Hernandez, Alberto
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
# ¿Hay alguna categoria que podamos agrupar? Para ello, recurrimos a la libreria scorecard, empleando la funcion
# woebin que nos permite saber si hay alguna posible agrupacion que mejore el IV (Information Value)
library(scorecard)
salida.woe.ccaa <- woebin(data.frame(input_bin[, c("varObjBin", "CCAA")]), "varObjBin", print_step = 0)
salida.woe.ccaa$CCAA$breaks
input_bin_copia$CCAA <- recode(input_bin_copia$CCAA, "c('CV_EX_AS_BA_CA','MA_CA_RI_CE_ME_MU_GA') = 'CV_EX_AS_BA_CA_MA_CA_RI_CE_ME_MU_GA_CM'")
data_train_copia.bin <- input_bin_copia[trainIndex.bin,]
data_test_copia.bin <- input_bin_copia[-trainIndex.bin,]

# NOTA: Con el modelo 1.2 partimos en la seleccion de variables
# MODELO 1.2. ¿Y si eliminamos las variables con menor importancia?
importancia.var <- impVariablesLog(modelo1.bin, "varObjBin", data_train.bin)

# Dado que crear un modelo de regresion logistica es "computacionalmente" mas costoso que un modelo de regresion lineal, debemos eliminar en la medida
# de lo posibles aquellas variables que menos nos aporten a nuestro modelo, en especial de cara a la seleccion clasica. Por ello, de cara a las interacciones
# nos quedaremos con aquellos cuya importancia este por encima del tercer cuartil (0.000855)
summary(importancia.var$V5)
variables.mas.imp <- importancia.var[which(importancia.var$V5 > 1.29203), "V2"]
formInt.bin <- paste0("varObjBin~",paste0(colnames(input_bin)[-1], collapse = "+"),"+",paste0(unlist(variables.mas.imp), collapse = "+"))

modelo1.2.bin<-glm(formInt.bin,data=data_train.bin, family = binomial)
# Tanto el AIC como el SBC se ve reducido significativamente. Sin embargo, el modelo no converge (tenemos que eliminar variables)
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
# De cara a la seleccion aleatoria nos quedaremos tanto con el segundo como con el sexto modelo, ya que pese a que el sexto modelo mejora en cuanto a AIC, SBC y desviacion tipica,
# la diferencia entre ambos es muy pequena, ademas de que el segundo modelo presenta un menor numero de parametros (40 frente a 19)
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

# Los modelos de seleccion aleatoria no aportan ninguna mejora al modelo, pese a reducir el numero de variables
auxVarObj<-input_bin$varObjBin
input_bin$varObjBin<-make.names(input_bin$varObjBin)
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]
estadisticas.modelos.final.2.bin <- validacion.cruzada.bin(c(estadisticas.modelos.bin[6], paste("varObjBin~", rownames(modelos.aleatorios))), "glm", data_train.bin)
estadisticas.modelos.final.2.bin
input_bin$varObjBin<-auxVarObj
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]

# ¿Que modelo es el mejor? ¿El primero o el sexto de la seleccion clasica?
# Para decantarnos por uno de los dos modelos comparemos el p-valor de cada uno
# En el modelo 2, el 75 % de sus coeficientes presentan un p-valor de 0.001 o menos (3er cuartil),
# mientras que en el modelo 6 solo el 50 % de las variables esta por debajo de 0.008
# Comparamos ambos modelos
cor(Filter(is.numeric, input_bin)[c(5,7,13,14,24,27)], use="complete.obs", method="pearson")
impVariablesLog(estadisticas.modelos.bin[6]$`SBC-backward`, "varObjBin", data_train.bin)
formula.final.bin.clasica <- 'varObjBin ~ CCAA + Age_over65_pct + ForeignersPtge + AgricultureUnemploymentPtge + IndustryUnemploymentPtge + ActividadPpal + prop_missings + ForeignersPtge:ActividadPpal + CCAA:IndustryUnemploymentPtge'
modelo.final.bin.clasica <- glm(formula.final.bin.clasica, data_train.bin, family = binomial)
mostrar.estadisticas(modelo.final.bin.clasica, data_train.bin, data_test.bin, "glm", "varObjBin")

cor(Filter(is.numeric, input_bin)[c(5,7,13,24)], use="complete.obs", method="pearson")
impVariablesLog(glm(paste0("varObjBin~",rownames(modelos.aleatorios)[1]), data_train.bin, family = binomial), "varObjBin", data_train.bin)
formula.final.bin.aleatorio <- 'varObjBin ~ Age_over65_pct+AgricultureUnemploymentPtge+CCAA+Densidad+ForeignersPtge'
modelo.final.bin.aleatorio <- glm(formula.final.bin.aleatorio, data_train.bin, family = binomial)
mostrar.estadisticas(modelo.final.bin.aleatorio, data_train.bin, data_test.bin, "glm", "varObjBin")

summary(summary(modelo.final.bin.clasica)$coefficients[, 4])
summary(summary(modelo.final.bin.aleatorio)$coefficients[, 4])

# ¿Y las nuevas medias y sd?
auxVarObj<-input_bin$varObjBin
input_bin$varObjBin<-make.names(input_bin$varObjBin)
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]
nuevas.medias.sd <- validacion.cruzada.bin(c(formula.final.bin.clasica, formula.final.bin.aleatorio), "glm", data_train.bin)
nuevas.medias.sd$modelo <- c("Modelo 6 (modificado)", "Modelo 1 aleatorio (modificado)")
nuevas.medias.sd
input_bin$varObjBin<-auxVarObj
data_train.bin <- input_bin[trainIndex.bin,]
data_test.bin <- input_bin[-trainIndex.bin,]

# ¿Podriamos juntar de nuevo la CCAA?
mostrar.estadisticas(glm(formula.final.bin.aleatorio, data_train_copia.bin, family = binomial), data_train_copia.bin, data_test_copia.bin, "glm", "varObjBin")

interacciones <- tail(variables.mas.imp, 5)
validaciones.cruzadas <- c()
df <- data.frame("Interaccion" = c(), "AIC" = c(), "SBC" = c(), "Dif. PseudoR2" = c(), "sd" = c())
for (interaccion in interacciones) {
  formula.final.bin.aux <- paste0(formula.final.bin.aleatorio, "+", interaccion)
  
  modelo.final.bin.aux <- glm(formula.final.bin.aux, data_train.bin, family = binomial)
  df <- rbind(df, c(interaccion, round(AIC(modelo.final.bin.aux), 2), round(BIC(modelo.final.bin.aux), 2), 
                    pseudoR2(modelo.final.bin.aux,data_train.bin,'varObjBin') - pseudoR2(modelo.final.bin.aux,data_test.bin,'varObjBin')))
  auxVarObj<-input_bin$varObjBin
  input_bin$varObjBin<-make.names(input_bin$varObjBin)
  data_train.bin <- input_bin[trainIndex.bin,]
  data_test.bin <- input_bin[-trainIndex.bin,]
  validaciones.cruzadas <- c(validaciones.cruzadas, unlist(validacion.cruzada.bin(formula.final.bin.aux, "glm", data_train.bin)$sd))
  input_bin$varObjBin<-auxVarObj
  data_train.bin <- input_bin[trainIndex.bin,]
  data_test.bin <- input_bin[-trainIndex.bin,]
}
df <- cbind(df, validaciones.cruzadas)
colnames(df) <- c("Interaccion", "AIC", "SBC", "Dif. PseudoR2", "sd")
rm(validaciones.cruzadas); rm(formula.final.bin.aux); rm(modelo.final.bin.aux)
df # No parece ser buena idea anadir interacciones

# Conseguimos una maxima correlacion positiva de 0.11
max(unique(c(cor(Filter(is.numeric, input_bin)[,c(5,7,13)], use="pairwise", method="pearson")))[-1])
# Ademas de una correlacion minima de -0.29
min(unique(c(cor(Filter(is.numeric, input_bin)[,c(5,7,13)], use="pairwise", method="pearson")))[-1])

# MODELO GANADOR: Modelo 2 aleatorio (Siguiendo el principio de parsimonia ademas de la importancia general de los coeficientes)
# BUSCAMOS EL MEJOR PUNTO DE CORTE
## Generamos una rejilla con los posibles puntos de corte
posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,function(x) sensEspCorte(modelo.final.bin.aleatorio,data_test.bin,"varObjBin",x,"1")))))
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1
plot(rejilla$posiblesCortes,rejilla$Accuracy, col="red")
points(rejilla$posiblesCortes,rejilla$Youden,col="blue")
legend("topleft",legend = c("Youden", "Accuracy"), col = c("red", "blue"), pch = c(16, 16), cex = 0.5)
cat("Max. Indice Youden: ", rejilla$posiblesCortes[which.max(rejilla$Youden)], "\n")
cat("Max. Precision/Accuracy: ", rejilla$posiblesCortes[which.max(rejilla$Accuracy)], "\n")

# Comparamos ambos puntos de corte
# Indice Youden
sensEspCorte(modelo.final.bin.aleatorio,data_test.bin,"varObjBin",0.59,"1")

# Indice Precision/Accuracy
sensEspCorte(modelo.final.bin.aleatorio,data_test.bin,"varObjBin",0.54,"1")

# El indice de Youden no solo presenta un nivel de sensitividad alto (91 %) sino que ademas la tasa de especificidad o tasa de verdaderos negativos es mucho mejor
# (70 % frente al 66 % del indice ACCURACY). Por tanto, con el objetivo de maximizar tanto la tasa de sensitividad como la tasa de especifidad elegimos el indice de Youden
sensEspCorte(modelo.final.bin.aleatorio,data_train.bin,"varObjBin",0.59,"1")
sensEspCorte(modelo.final.bin.aleatorio,data_test.bin,"varObjBin",0.59,"1")

# Evaluacion del modelo final
mostrar.estadisticas(modelo.final.bin.aleatorio, data_train.bin, data_test.bin, "glm", "varObjBin")
summary(modelo.final.bin.aleatorio)
roc(data_train.bin$varObjBin, predict(modelo.final.bin.aleatorio,data_train.bin,type = "response"))
roc(data_test.bin$varObjBin, predict(modelo.final.bin.aleatorio,data_test.bin,type = "response"))
impVariablesLog(modelo.final.bin.aleatorio, "varObjBin", data_train.bin)
