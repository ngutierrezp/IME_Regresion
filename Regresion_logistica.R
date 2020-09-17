###################################

## Apartado para fijar el workspace en Rstudio
dirstudio <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(dirstudio) 





##############################
#  Ayudantia de regresion
##############################

library(ggplot2)
library(lmtest)
library(psych)
library(corrplot)
library(gridExtra)
library(car)
library(dplyr)

# 1 - Teoria de regresión logistica

# 2- Ejercicio practico.


##############################
# Ejemplo de regresión logistica





# Para este ejemplo vamos a utilizar un dataset muy usado en todo el mundo.
# Vamos a crear un modelo que nos permita saber de que depende las enfermedades
# de corazón de una pesona.

if(!exists("getAllData", mode="function")) source("scripts/genereteProccessed.R")
if(!exists("bar.plot.categorical", mode="function")) source("scripts/categoricalPlot.R")
if(!exists("conf.matrix ", mode="function")) source("scripts/confMatrix.R")

# Datos


datos <- getAllData()

datos.all <- datos[,-15]




#     mixed.all.df :  Contiene toda la informaciÃ³n limpia
#                     y legible con con la interpretaciÃ³n
#                     categorica de los numeros.
mixed.all.df <- getMixedData(datos)


#     var.numerical : Es un vector quer indica las columnas
#                     que son numericas.
var.numerical <- c(1,4,5,8,10,12)

#     var.categorial : Es un vector quer indica las columnas
#                     que son categoricas y dicotomicas.

var.categorial <- c(2,3,6,7,9,11,13,14)








# Si vemos como se comporta nuestra variable de respuesta o clase.

respuesta <- table(datos.all$num)





## Analisis primeramente como es el comportamiento de las variables 

## Analicemos la variables categoricas
categorical.plots <- bar.plot.categorical(mixed.all.df)


## Tambien es bueno ver el panorama completo.

resumen <- summary(datos.all)







##### Como ya conocemos como son los datos podemos comenzar a hacer nuestro modelo



modelo <- glm(num ~ ., data = datos.all, family = "binomial")


## Podemos ver que existen variables muy significativas como el ca y thal

# sin embargo no son del todo ciertas, al tener mucha cantidad de NAs se pierde
# un tanto el sentido de que se está midiendo y solo se ajusta a lo que se 
# conoce, por lo que como estas no aportan mucho, es mejor sacarlas del modelo


datos.limpios = datos.all[,-c(12,13)]


# volvamos a hacer el modelo


nuevo.modelo <- glm(num ~ ., data = datos.limpios, family = "binomial")



# Del resultado de esto se puede ver que las variables mas significativas
# son :

#       - sex(2)
#       - cp(3)
#       - oldpeak(10)



# y las variables menos significativas son :

#       - age(1)
#       - fbs(6)
#       - restecg(7)

## igual se puede ver que Slope no aporta mucho al modelo y posee muchos NA´s
#por lo cual igualmente se puede sacar del modelo


datos.sign <- datos.limpios[,-c(1,6,7,11)]



final.modelo <- glm(num ~ ., data = datos.sign, family = "binomial")




## Evaluación del modelo :



## Para esto utilizamos 

eval <-  anova(final.modelo, test = "LRT")

## Viendo el p-valor del modelo completo:

dif_residuos <- final.modelo$null.deviance - final.modelo$deviance

# Grados libertad
df <- final.modelo$df.null - final.modelo$df.residual

# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)



## El p-valor tiende a ser muy significativo.







### Aceptación del modelo 



### Linealidad

## No pude encontrar un metodo que nos permitiera comprobar si existe linealidad
# Los metodos anteriores no sirven debido a que estamos trabajando bajo otra escala

# Si comprobamos los metodos anteriores, nos daremos cuenta que existe un patron
# de comportamiento debido a los ODDs que trabajan de forma exponencial.

lineal <- data.frame(residuos = final.modelo$residuals, valor = final.modelo$fitted.values)

lineal.plot <- ggplot(data = lineal, aes(x = valor, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = valor, yend = 0), alpha = 0.2) +
  labs(title = "DistribuciÃ³n de los residuos", x = "valores",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


## Por eso asumiremos la linealidad.









### Independencia de los datos:

## Que los datos hayan sido escogidos de forma aleatoria












### No colinialidad:


# Para esto utilizamos el metodo ya conocimo de VIF:

vif.result <- vif(final.modelo)

















##### Clasificador de regresión,

# recordemos que para esta parte, utilizaremos un un 80-70% de los datos
# para crear el modelo y un 30% para hacer las pruebas de este. 





set.seed(985426)

ntrain <- nrow(datos.sign) * 0.7

index_train<-sample(1:nrow(datos.sign),size = ntrain)

train.df<-datos.sign[index_train,]

test.df<-datos.sign[-index_train,]




high.reg <- glm(num ~ ., family = "binomial", data = train.df)

high.result <- summary(high.reg)


##### matriz de confucion en r #####



## Establecemos el valor threshold en 50%
matrix <- conf.matrix(high.reg, test.df, 0.5 )

# matriz de confucion
conf.matriz<- matrix$matrix

#datos del mismo modelo
model.data <- matrix$data









