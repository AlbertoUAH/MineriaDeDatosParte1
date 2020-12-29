setwd("UCM/Mineria de Datos y Modelizacion Predictiva/Practica 1/")
source("FuncionesRosa.R")
library(readxl)
datos <- read_excel("DatosEleccionesEspaña.xlsx",sheet = 1)

# ---------------------------- FASE 1: DEPURACION DE LOS DATOS ------------------------------
# 1. No todas las variables categoricas estan como factores (CCAA, AbstencionAlta, Izquierda, Derecha, ActividadPpal, densidad)
datos[,c(3, 7, 11, 12, 34, 38)] <- lapply(datos[,c(3, 7, 11, 12, 34, 38)], factor)

# 2. El campo "Name" es un campo unicamente Identificativo, por lo que debemos descartarlo
cat(length(unique(datos$Name)), " de ", nrow(datos), "filas\n")
datos <- datos[, -c(1)] # Eliminamos el campo identificador

# ¿Y CodigoProvincia? Problema: toma 52 valores diferentes, por lo que codificarlo como factor puede llegar a "entorpecer" la elaboracion del modelo,
# especialmente con demasiadas categorias. ¿Y si lo tratamos como una variable cuantitativa? Hay que tener mucho cuidado, ya que el hecho de utilizar el codigo de provincia
# como una variable numerica mas "tiene que aportar un sentido" al modelo, es decir, que la media del codigoProvincia sea 26.67 no me aporta nada, ademas de que no tiene sentido
# Columnas 5,7,8 y 9 correspondientes con las variables objetivo cuantitativas
cor(cbind(datos$CodigoProvincia, datos[, c(5,7,8,9)]), use = "complete.obs", method = "pearson")[1,-1]
# Mediante la V Cramer vemos la importancia sobre las variables objetivo cualitativas
salida <- c()
for(col in c("AbstencionAlta", "Izquierda", "Derecha")) {
  salida <- cbind(salida, sapply(datos[ , "CodigoProvincia"],function(x) Vcramer(x,unlist(datos[, col]))))
}
salida
datos <- datos[, -c(1)] # Lo eliminamos

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

# 5. Missings no declarados variables cualitativas
t(freq(datos$Densidad)) # Nos encontramos con una categoria desconocida "?"
datos$Densidad<-recode.na(datos$Densidad,"?")

# 7. Variables con elevada asimetria/kurtosis (Population, TotalCensus, totalEmpresas, ComercTTEHosteleria, Servicios, inmuebles, Pob2010 y SUPERFICIE)
describe(datos[c("Population", "TotalCensus", "totalEmpresas", "ComercTTEHosteleria", 
                 "Servicios", "inmuebles", "Pob2010", "SUPERFICIE")])[, c(4, 11, 12)]
# Como podemos comprobar, la asimetria en las variables anteriores se debe, principalmente, a la grandes ciudades (Madrid, Barcelona) donde hay una mayor poblacion,
# un mayor numero de empresas y, por ello, mayor numero de inmuebles y actividades

# 8. Valores Atipicos
# En primer lugar, contamos el porcentaje de valores atipicos de cada variable. Si son muchos, los eliminamos
original <- sapply(Filter(is.numeric, datos),function(x) atipicosAmissing(x)[[2]]) * 100/nrow(datos)
original[which(original == max(original))]
# Modifico los atipicos como missings (el maximo porcentaje es de un 11.87 % en el campo Servicios)
# Nota: Otros_Pct, al ser una variable objetivo, no vamos a modificar su valores atipicos
datos[,(which(sapply(datos, class)=="numeric")[-6])]<-sapply(datos[, c(which(sapply(datos, class)=="numeric")[-6])],function(x) atipicosAmissing(x)[[1]])

# 9. Missings
# Se aprecia un patron de comportamiento: cuando los datos de la poblacion/censo no aparecen, tampoco se encuentra el numero total de empresas o el numero total
# dedicadas a la industria, construccion, comercio, servicios, el numero de inmuebles o la poblacion del ano 2010. Del mismo modo, si no se registran los datos de 
# un sector, el del resto tampoco suele aparecer (Ejemplo: si no se registra el numero total de empresas, es poco comun que aparezca el numero de empresas de algun sector)
# INDICIO: ¿totalEmpresas se obtiene de la suma de cada sector?
# INDICIO: Si el campo Population no aparece, tampoco suele mostrarse su censo total
mostrar_correlacion_na <- function() {
  corrplot(cor(is.na(datos[colnames(datos)[colSums(is.na(datos))>0]])),method = "ellipse",type = "upper")
}

# A continuacion, obtenemos la proporcion de missings por variable y observacion
# Por observacion: creamos un nuevo campo
datos$prop_missings<-apply(is.na(datos[, c(-4,-5,-6,-7,-8,-9,-10)]),1,mean) * 100
max(datos$prop_missings)
# Max = 37.5 % -> Tengo alguna observacion con un 37.5 % de los datos a missing

# Por variable, obtenemos la media
# La columna con el mayor porcentaje de missings es del 12.64 %
(prop_missingsVars<-max(apply(is.na(datos[, c(-4,-5,-6,-7,-8,-9,-10)]),2,mean) * 100))
# En ambos casos no superan el 50 % (por lo que no los eliminamos)

# ¿Como imputamos los valores?
# Ya me encuentro con un problema. Con una imputacion aleatoria unicamente no es suficiente para completar todos los valores. Esto es debido a que la interpolacion, debido
# a un elevado numero de missings consecutivos, no es posible imputar todos los valores, por lo que muchos de ellos quedan sin valor. Una opcion seria, en caso de que la mediana
# no varie demasiado con respecto al valor original, podemos imputar los valores aleatorios (todos los posibles) y los valores restantes imputarlos con la mediana, dado que es mas
# representativa que la media
columnas <- c(2,3,15,16,17,18,19,20,23,24,25,28,29,30,31,33,34,35,37,38,39)
antes.imputar <- sapply(Filter(is.numeric, datos[, columnas]), function(x) median(x, na.rm = TRUE))
datos[,columnas] <- sapply(datos[, columnas],function(x) ImputacionCuant(x,"aleatorio"))
datos[,columnas] <- sapply(datos[, columnas],function(x) ImputacionCuant(x,"mediana"))
despues.imputar <- sapply(Filter(is.numeric, datos[, columnas]), function(x) median(x, na.rm = TRUE)) - antes.imputar

# Age_19_65_pct: se puede obtener en funcion de Age_under19_Ptge y Age_over65_pct
edad.19.65 <- apply(datos,1,function(x) if(is.na(x["Age_19_65_pct"])) x["Age_19_65_pct"] <- 100 - (as.numeric(x["Age_under19_Ptge"]) + as.numeric(x["Age_over65_pct"])))
datos[is.na(datos$Age_19_65_pct), "Age_19_65_pct"] <- unlist(edad.19.65[!sapply(edad.19.65, is.null)])

# totalEmpresas: se puede obener a partir de la suma de Industria + Construccion + ComercTTEHosteleria + Servicios
total.empresas <- apply(datos,1,function(x) if(is.na(x["totalEmpresas"])) x["totalEmpresas"] <- as.numeric(x["Industria"]) + as.numeric(x["Construccion"]) + 
                          as.numeric(x["ComercTTEHosteleria"]) + as.numeric(x["Servicios"]))
datos[is.na(datos$totalEmpresas), "totalEmpresas"] <- unlist(total.empresas[!sapply(total.empresas, is.null)])

# Por otro lado, no se ha realizado la misma imputacion con Unemployless25_Ptge y PersonasInmueble, dado que al recuperar el porcentaje de valores atipicos
# aumentaba significativamente, por lo que se ha decidido imputar dichos valores de forma aleatoria junto con la mediana

# En la variable "Densidad", solo hay 92 valores missing: podemos seguir los estipulado en el Excel para calcular su valor
summary(datos$Densidad)
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
summary(datos$Densidad)

# ¿Cuanto ha variado el porcentaje de correlacion tras la depuracion?
corr.posterior <- cor(datos[,unlist(lapply(datos, is.numeric))], use = "complete.obs" , method="pearson")
comparacion.corr <- corr.posterior[-33, -33] - corr.previa
sum(abs(comparacion.corr) < 0.2) * 100 / (dim(comparacion.corr)[1] * dim(comparacion.corr)[2])
final <- sapply(Filter(is.numeric, datos),function(x) atipicosAmissing(x)[[2]]) * 100/nrow(datos)
original[original > 5]
final[final > 5]

# ¿Que variable elegimos?
# Como variable objetivo continua elegimos el campo Dcha_Pct, ya que pese a la elevada correlaciomn del campo Otros_Pct, presenta un mayor porcentaje de outliers, 
# lo que dificulta la prediccion de los porcentajes Por el contrario, Dcha_Pct presenta una correlacion lieramente menor, aunque sin presentar valores atipicos
for(variableObj in c(4,6,7,8)) {
  cat(colnames(datos[variableObj]), ". Suma correlaciones: ", sum(abs(cor(datos[,unlist(lapply(datos, is.numeric))], use="complete.obs", method="pearson"))[variableObj, c(-6:-12)]), "\n")
}
# Del mismo modo, como variable objetivo cualitativa escogemos el campo Derecha, pues es el que mayor correlacion presenta
for(variableObj in c(5, 9, 10)) {
  cat(colnames(datos[variableObj]), ". Suma correlaciones: ", sum(abs(cor(datos[,unlist(lapply(datos, is.numeric))], use="complete.obs", method="pearson"))[variableObj, c(-6:-12)]), "\n")
}

# Borramos el resto de variables objetivo, ademas de separar las variables objetivo del resto de variables
varObjCont <- datos$Dcha_Pct
varObjBin <- datos$Derecha
datos <- datos[, -c(4,5,6,7,8,9,10)]

# 10. ¿Podemos agrupar categorias de variables cualitativas?
# Comenzamos con CCAA. Para la varObjCont las agrupamos por similutud en los boxplots
boxplot_targetbinaria(varObjCont,datos$CCAA,"CCAA")
datos$CCAA <- recode(datos$CCAA, "c('Navarra', 'Andalucía') = 'AN_NA'; c('CastillaLeón') = 'AA_CL';
                                  c('ComValenciana', 'Extremadura', 'Asturias', 'Baleares', 'Canarias') = 'CV_EX_AS_BA_CA'; c('Aragón', 'CastillaMancha') = 'AR_CM';
                                  c('Galicia', 'Cantabria', 'Madrid', 'Rioja', 'Ceuta', 'Melilla', 'Murcia') = 'MA_CA_RI_CE_ME_MU_GA'; c('Cataluña', 'PaísVasco') = 'CAT_PV'")

# Construccion-Industria-Otro
# Para este caso nos basamos en el grafico de mosaico. Vemos que aquellos municipios dedicados a la Construccion, Industria u Otros presentan una mayor tendencia de voto a la derecha
boxplot_targetbinaria(varObjCont,datos$ActividadPpal,"Actividad Principal")
datos$ActividadPpal <- recode(datos$ActividadPpal, "c('Construccion', 'Industria', 'Otro') = 'Construccion_Industria_Otro';")

# Busco las mejores transformaciones para las variables numericas con respesto a los dos tipos de variables
input_cont<-data.frame(varObjCont,datos,Transf_Auto(Filter(is.numeric, datos),varObjCont))
input_bin<-data.frame(varObjBin,datos,Transf_Auto(Filter(is.numeric, datos),varObjBin))

correlaciones  <- round(cor(Filter(is.numeric, input_cont), use="pairwise", method="pearson")[1,32:61] - cor(Filter(is.numeric, input_cont), use="pairwise", method="pearson")[1,2:31], 2)
input_cont <- input_cont[, !colnames(input_cont) %in% names(correlaciones[correlaciones > -0.01])]
input_cont <- input_cont[, c(-3,-4,-5,-10,-13,-14,-15,-16,-17:-25,-27,-28,-31)]

# Segun el criterio WOE, no aportan apenas cambio al modelo
salida.woe <- woebin(input_bin, "varObjBin", print_step = 0)
sapply(salida.woe[c(2:24,26:28,30:33)], function(x) x$total_iv[1]) - sapply(salida.woe[c(34:63)], function(x) x$total_iv[1])
input_bin <- input_bin[, -c(35:64)]


# Con la variables independientes nos encontramos con algunos casos como totalEmpresas, Industria, Construccion, ComercTTEHosteleria, Servicios e Inmuebles, donde
# existe una elevada colinealidad entre ambas, por lo que debemos conservar la variable mas importante y descartar el resto
corrplot(cor(Filter(is.numeric, input_cont[c(3,4,5,17,7,19,15,16,27:33)]), use="pairwise", method="pearson"), method = "circle",type = "upper", tl.cex = 0.7)
corrplot(cor(Filter(is.numeric, input_bin[c(5:8,11,13,3,4,21:25,27:28)]), use="pairwise", method="pearson"), method = "circle",type = "upper", tl.cex = 0.7)

# A continuacion, debemos descartar variables muy correlacionadas entre si, con el objetivo de evitar multicolinealidad en los modelos
par(mfrow = c(1,2))
# Para la varObjCont tiene mayor importancia Age_under19_Ptge, Age_19_65_pct, SameComAutonPtge y logxtotalEmpresas
graficoVcramer(input_cont[,c(3,4,5,17,7,19,15,16,27:33)],varObjCont)
# Para la varObjBin tiene mayor importancia Age_over65_pct, SameComAutonPtge y totalEmpresas
graficoVcramer(input_bin[,c(5:8,11,13,3,4,21:25,27:28)],varObjBin)

# Elimino aquellas variables que no presentan una elevada correlacion
input_cont <- input_cont[, c(-5,-19,-16,-17,-28:-33)]
input_bin <- input_bin[, c(-4:-7,-13,-22:-25,-27,-28)]

# Finalmente, almacenamos el resultado en disco
saveRDS(input_cont,"datosElectorales_Cont")
saveRDS(input_bin,"datosElectorales_Bin")

# Finalmente, guardamos el workspace
save.image(file = "depuracion.RData")

