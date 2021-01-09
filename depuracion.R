# Autor: Fernandez Hernandez, Alberto
setwd("UCM/Mineria de Datos y Modelizacion Predictiva/Practica 1/")
source("FuncionesRosa.R")
library(readxl)
datos <- read_excel("DatosEleccionesEspaña.xlsx",sheet = 1)

# ---------------------------- FASE 1: DEPURACION DE LOS DATOS ------------------------------
datos <- datos[, -c(6,7,8,10,11)] # Eliminamos el resto de variables objetivo
# 1. No todas las variables categoricas estan como factores: c(2,3,7,29,33) -> (CodigoProvincia, CCAA, Derecha, ActividadPpal, densidad)
datos[,c(2,3,7,29,33)] <- lapply(datos[,c(2,3,7,29,33)], factor)

varObjCont <- datos$Dcha_Pct
varObjBin <- datos$Derecha
datos <- datos[, -c(6:7)]

# 2. El campo "Name" es un campo unicamente Identificativo, por lo que debemos descartarlo
cat("Nombres de municipio unicos: ", length(unique(datos$Name)), " de ", nrow(datos), "filas. Numero columnas: ", ncol(datos) , "\n")
datos <- datos[, -c(1)] # Eliminamos el campo identificador

# ¿Y CodigoProvincia? Problema: toma 52 valores diferentes, por lo que codificarlo como factor puede llegar a "entorpecer" la elaboracion del modelo,
# especialmente con demasiadas categorias. 
sapply(datos[, c("CodigoProvincia")],function(x) Vcramer(x,datos$CCAA)) # Correlacion perfecta (1)
sapply(datos[, c("CodigoProvincia", "CCAA")],function(x) Vcramer(x,varObjCont))
sapply(datos[, c("CCAA", "CodigoProvincia")],function(x) Vcramer(x,varObjBin))

cat("Categorias CodigoProvincia: ", length(levels(datos$CodigoProvincia)), "; CCAA: ", length(levels(datos$CCAA)), "\n")
sum(freq(datos$CodigoProvincia)$`n` < 100)

datos$CodigoProvincia <- as.numeric(datos$CodigoProvincia)
cat("VarObjCont: ", sapply(datos[, c("CodigoProvincia")],function(x) Vcramer(x,varObjCont)), "; VarObjBin: ", sapply(datos[, c("CodigoProvincia")],function(x) Vcramer(x,varObjBin)), "\n")

datos <- datos[, -c(1)]

# Mediante la variable corr.previa nos guardamos la matriz de correlaciones de cara a validar la calidad de la imputacion
corr.previa <- cor(datos[,unlist(lapply(datos, is.numeric))], use="complete.obs", method="pearson")

# 3. Valores no declarados en variables cuantitativas (-1, 99999 etc.)
# Valores fuera de rango. En este caso, recodificamos porcentajes negativos en ForeignersPtge...
datos$ForeignersPtge<-replace(datos$ForeignersPtge, which(datos$ForeignersPtge < 0), NA)

# Porcentajes mayores que 100 (SameComAutonPtge y PobChange_pct)
datos$SameComAutonPtge <-replace(datos$SameComAutonPtge, which(datos$SameComAutonPtge > 100), NA)
datos$PobChange_pct <-replace(datos$PobChange_pct, which(datos$PobChange_pct > 100), NA)

# En este caso, con la columna Explotaciones, con valores a 99999
datos$Explotaciones<-replace(datos$Explotaciones,which(datos$Explotaciones==99999),NA)

# 4. Missings no declarados variables cualitativas
t(freq(datos$Densidad)) # Nos encontramos con una categoria desconocida "?"
datos$Densidad<-recode.na(datos$Densidad,"?")

# 5. Variables con elevada asimetria/kurtosis (Population, TotalCensus, totalEmpresas, ComercTTEHosteleria, Servicios, inmuebles, Pob2010 y SUPERFICIE)
describe(datos[c("Population", "TotalCensus", "totalEmpresas", "ComercTTEHosteleria", 
                 "Servicios", "inmuebles", "Pob2010", "SUPERFICIE")])[, c(4, 11, 12)]
# Como podemos comprobar, la asimetria en las variables anteriores se debe, principalmente, a la grandes ciudades (Madrid, Barcelona) donde hay una mayor poblacion,
# un mayor numero de empresas y, por ello, mayor numero de inmuebles y actividades

# 6. Valores Atipicos
# En primer lugar, contamos el porcentaje de valores atipicos de cada variable. Si son muchos, los eliminamos
original <- sapply(Filter(is.numeric, datos),function(x) atipicosAmissing(x)[[2]]) * 100/nrow(datos)
head(sort(original, decreasing = T), 5)
original[which(original == max(original))]
# Modifico los atipicos como missings (el maximo porcentaje es de un 11.87 % en el campo Servicios)
datos[,(which(sapply(datos, class)=="numeric"))]<-sapply(datos[, c(which(sapply(datos, class)=="numeric"))],function(x) atipicosAmissing(x)[[1]])
cat("Total valores missing: ", sum(is.na(datos)))

# 7. Missings
datos$prop_missings<-apply(is.na(datos[, ]),1,mean) * 100
(prop_missingsVars<-max(apply(is.na(datos[, ]),2,mean) * 100))
data.frame("Por observacion" = max(datos$prop_missings), "Por variable" = round(prop_missingsVars, 2))
# Se aprecia un patron de comportamiento: cuando los datos de la poblacion/censo no aparecen, tampoco se encuentra el numero total de empresas o el numero total
# dedicadas a la industria, construccion, comercio, servicios, el numero de inmuebles o la poblacion del ano 2010. Del mismo modo, si no se registran los datos de 
# un sector, el del resto tampoco suele aparecer (Ejemplo: si no se registra el numero total de empresas, es poco comun que aparezca el numero de empresas de algun sector)
# INDICIO: ¿totalEmpresas se obtiene de la suma de cada sector?
# INDICIO: Si el campo Population no aparece, tampoco suele mostrarse su censo total (realmente tiene sentido, ya que el censo total depende de la cantidad de poblacion en el municipio)
corrplot(cor(is.na(datos[colnames(datos)[colSums(is.na(datos))>0]]))[c(1,2,13:19), c(1,2,13:19)],method = "ellipse",type = "upper", tl.cex = 0.5)

columnas <- c(2,3,8,9,10:12,13,16:19,21:24,26,27,28,30,31,32)
datos[,columnas] <- sapply(datos[, columnas],function(x) ImputacionCuant(x,"aleatorio"))
datos[,columnas] <- sapply(datos[, columnas],function(x) ImputacionCuant(x,"mediana"))

edad.19.65 <- apply(datos,1,function(x) if(is.na(x["Age_19_65_pct"])) x["Age_19_65_pct"] <- 100 - (as.numeric(x["Age_under19_Ptge"]) + as.numeric(x["Age_over65_pct"])))
datos[is.na(datos$Age_19_65_pct), "Age_19_65_pct"] <- unlist(edad.19.65[!sapply(edad.19.65, is.null)])

total.empresas <- apply(datos,1,function(x) if(is.na(x["totalEmpresas"])) x["totalEmpresas"] <- as.numeric(x["Industria"]) + as.numeric(x["Construccion"]) + 
                          as.numeric(x["ComercTTEHosteleria"]) + as.numeric(x["Servicios"]))
datos[is.na(datos$totalEmpresas), "totalEmpresas"] <- unlist(total.empresas[!sapply(total.empresas, is.null)])

modificar.columna <- function(fila) {
  densidad <- ""
  if(is.na(fila["Densidad"])) {
    proporcion <- as.numeric(fila["Population"]) / as.numeric(fila["SUPERFICIE"])
    ifelse(proporcion < 1, densidad <- "MuyBaja", ifelse(proporcion > 1 & proporcion < 5, densidad <- "Baja", densidad <- "Alta"))
  }
  else {
    densidad <- fila["Densidad"]
  }
  as.factor(densidad)
}
datos$Densidad <- apply(datos, 1, modificar.columna)
original.2 <- sapply(Filter(is.numeric, datos),function(x) atipicosAmissing(x)[[2]]) * 100/nrow(datos)
cat(sum(is.na(datos)), " valores missing. Columna con mayor % atipicos: ", original.2[which(original.2 == max(original.2))]  ,"\n")

# Analizamos la nueva matriz de correlacion
corr.posterior <- cor(datos[,unlist(lapply(datos, is.numeric))], use = "complete.obs" , method="pearson")
comparacion.corr <- corr.posterior[-30, -30] - corr.previa
sum(abs(comparacion.corr) < 0.2) * 100 / (dim(comparacion.corr)[1] * dim(comparacion.corr)[2])

# 8. ¿Podemos recategorizar alguna CCAA?
boxplot_targetbinaria(varObjCont,datos$CCAA,"CCAA")
datos$CCAA <- recode(datos$CCAA, "c('Navarra', 'Andalucía') = 'AN_NA'; c('Cataluña', 'PaísVasco') = 'CAT_PV';
c('ComValenciana', 'Extremadura', 'Asturias', 'Baleares', 'Canarias') = 'CV_EX_AS_BA_CA'; 
c('Aragón', 'CastillaMancha') = 'AR_CM'; c('CastillaLeón') = 'AA_CL'; 
c('Galicia', 'Cantabria', 'Madrid', 'Rioja', 'Ceuta', 'Melilla', 'Murcia') = 'MA_CA_RI_CE_ME_MU_GA';")
# ¿Y ActividadPpal?
estadisticas <- boxplot_targetbinaria(varObjCont,datos$ActividadPpal,"Actividad Principal")

summary(estadisticas$data[estadisticas$data$target == "Otro", "variable"])
summary(estadisticas$data[estadisticas$data$target == "Construccion", "variable"])
summary(estadisticas$data[estadisticas$data$target == "Industria", "variable"])

datos$ActividadPpal <- recode(datos$ActividadPpal, "c('Construccion', 'Industria', 'Otro') = 'Construccion_Industria_Otro';")

# 9. ¿Podemos eliminar alguna variable?
corrplot(cor(Filter(is.numeric, datos[c(4,5,6,7,9,10,12,2,3,20:27)]), use="pairwise", method="pearson"), method = "circle",type = "upper", tl.cex = 0.5)
par(mfrow = c(1,2))
graficoVcramer(datos[,c(4,5,6,7,9,10,12,2,3,20:24,26:27)],varObjCont)
graficoVcramer(datos[,c(4,5,6,7,9,10,12,2,3,20:24,26:27)],varObjBin)

input_cont <- datos[, c(-6, -12, -26)]
input_bin <- datos[, c(-6, -12, -22)]

# 10. Transformacion de variables
input_cont<-data.frame(varObjCont,input_cont,Transf_Auto(Filter(is.numeric, input_cont),varObjCont))
input_bin<-data.frame(varObjBin,input_bin,Transf_Auto(Filter(is.numeric, input_bin),varObjBin))

corr.cont <- cor(Filter(is.numeric, input_cont), use="pairwise", method="pearson")[c(2:28), c(29:55)]
vector.cont <- c()
for(pos in seq(1:nrow(corr.cont))) {
  vector.cont <- c(vector.cont, corr.cont[pos, pos])
}
summary(vector.cont)
corr.bin <- cor(Filter(is.numeric, input_bin), use="pairwise", method="pearson")[c(1:27), c(28:54)]
vector.bin <- c()
for(pos in seq(1:nrow(corr.bin))) {
  vector.bin <- c(vector.bin, corr.bin[pos, pos])
}
summary(vector.bin)


correlaciones  <- round(abs(cor(Filter(is.numeric, input_cont), use="pairwise", method="pearson"))[1,29:55] - abs(cor(Filter(is.numeric, input_cont), use="pairwise", method="pearson"))[1,2:28], 2)
summary(correlaciones)

# Filtramos unicamente las transformadas que mejoren en mas de 0.1
input_cont <- input_cont[, !colnames(input_cont) %in% names(correlaciones[correlaciones < 0.1])]
input_cont <- input_cont[, c(-3,-4,-9,-12,-14,-15,-17,-19,-25)]
# Todas las transformadas significativas emplean escalas logaritmicas
names(correlaciones[correlaciones >= 0.1])

library(scorecard)

salida.woe <- woebin(input_bin, "varObjBin", print_step = 0) # library scorecard
summary(sapply(salida.woe[c(31:57)], function(x) x$total_iv[1]) - 
          sapply(salida.woe[c(2:21,23:25,27:30)], function(x) x$total_iv[1]))

input_bin <- input_bin[, -c(32:58)]

# Finalmente, almacenamos el resultado en disco
saveRDS(input_cont,"datosElectorales_Cont")
saveRDS(input_bin,"datosElectorales_Bin")

# Finalmente, guardamos el workspace
save.image(file = "depuracion.RData")

