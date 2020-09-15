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

# 1 - Teoria de regresión lineal Simple

# 2- Ejercicio practico.


##############################
# Regresion lineal Multiple


# Para este ejemplo vamos a utilizar un dataset de pokemons


#Primero vamos a cargar el archivo csv de los datos.


datos <- read.csv('pokedex.csv')

datos <- datos[!is.na(datos$weight_kg),]


# Para este estudio se quiere ser si el peso de un pokemon depende de sus
# estadisticas base.

# para ello separamos de todos los datos, las columnas que nos interesan:


datos = datos[c(13,19,20,21,22,23,24)]







# 1. visualización de los datos:
#################################

multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
          main = "")





## 1.1 matriz de correlación
#####

cor.poke <- cor(datos)

gra1 <- corrplot(cor.poke, method='number',type='upper')




## Se puede ver que existe un pequeño indicio que el hp, ataque y defensa 
#pueden estar relacionados al peso de un pokemon.

## tambien es posible ver que existe una relación entre la defensa y 
# defensa especial, al igual que la defensa y defensa especial.







# 2. Generación del modelo
#################################


modelo <- lm(weight_kg ~ . , datos)



# Haciendo un summary del modelo, podemos ver que las variables
# que son más significativas al modelo son hp, ataque y defenza.

# Un predictor que no aporta en nada al modelo es la defensa especial
# por lo que es posible sacarla de este modelo



# Otra forma de ver que variable sacar del modelo es por medio de un 
# analisis  de stepwise

stepwise <- step(object = modelo, direction = "both", trace = 1)


# Cuidado al usar esta función que existen fundamentos matematicos
# por detras que no estamos viendo. En general vamos a verlo al ojo. 
#########################################################

nuevo.modelo <- lm(weight_kg ~ hp + attack + defense + sp_attack + speed , datos)


## Aca podemos ver que que ataque especial no es una variable significativa
# y en general se vuelve a repetir que hp, ataque y defensa son muy significativas
# para nuestro modelo. 

# Entonces? podemos dejar solo estas variables que expliquen el modelo?

prueba.modelo <- lm(weight_kg ~ hp + attack + defense , datos)



## Como se puede ver, se pierde fuerza del modelo ya que R^2 disminuye su valor
# es decir, al quitar estas variables, nuestro modelo va a explicar menos.




### Finalmente el mejor modelo encontrado es 

# Y =  -167.64 + 1.43*hp +  0.66*attack + 1.27*defense + 
#                 0.17*sp_attack + -0.31*speed











# 2.1 Linealidad del modelo
####


# Recordemos que la linealidad del modelo se ve cuando los residuos
# tienen una distribución aleatoria respecto al origen o 0.

# En el caso de regresión multiple esta se ve para cada predictor.

datos$residuos <- nuevo.modelo$residuals
datos$ajuste <- nuevo.modelo$fitted.values


hp.plot <- ggplot(data = datos, aes(x = hp, y = residuos)) +
        geom_point(aes(color = residuos)) +
        scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
        geom_hline(yintercept = 0) +
        geom_segment(aes(xend = hp, yend = 0), alpha = 0.2) +
        labs(title = "Distribución de los residuos", x = "hp",
             y = "residuo") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


attack.plot <- ggplot(data = datos, aes(x = attack, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = attack, yend = 0), alpha = 0.2) +
  labs(title = "Distribución de los residuos", x = "ataque",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


defense.plot <- ggplot(data = datos, aes(x = defense, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = defense, yend = 0), alpha = 0.2) +
  labs(title = "Distribución de los residuos", x = "defenza",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


speed.plot <- ggplot(data = datos, aes(x = speed, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = speed, yend = 0), alpha = 0.2) +
  labs(title = "Distribución de los residuos", x = "velocidad",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


sp_attack.plot <- ggplot(data = datos, aes(x = sp_attack, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = sp_attack, yend = 0), alpha = 0.2) +
  labs(title = "Distribución de los residuos", x = "ataque especial",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

lineal.plot <- grid.arrange(hp.plot, attack.plot, defense.plot, sp_attack.plot, speed.plot)


# Con esto podemos que todas las variables se comportan de forma aleatoria, por 
# lo que podemos super linealidad.












# 2.2 Distribución normal de los residuos
####




## Metodo grafico
qqnorm(datos$residuos)
qqline(datos$residuos)


## Metodo por test


normal.residuals <- shapiro.test(datos$residuos)




## Se puede ver que no se cumple con la normalidad de los residuos.













# 2.3 Homocedasticidad de residuos
####


# Al igual que el caso anterior, pero ahora para los datos ajustados


## Metodo grafico.


homo.plot  <- ggplot(data = datos, aes(x = ajuste, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_segment(aes(xend = ajuste, yend = 0), alpha = 0.2) +
  geom_smooth(se = FALSE, color = "firebrick") +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")



## Claramente el metodo grafico nos indica que no existe una varianza cte.



# Metodo de test.

test.bp <- bptest(nuevo.modelo)



# El p valor de este test es sumamente pequeño por lo que no se puede
# aceptar la homocedasticidad. 








# 2.4 No colinialidad
####

# Para este caso podemos volver a ver la matriz de correlación y saber 
# si existen variables correlacionadas


# otra forma es analizando VIF. Como R es mágico tiene una paquete que
# implementa esta función.


vif.poke <- vif(nuevo.modelo)







# Estos valores nos indican que existe una pequña colinalidad entre las variables
# pero puede ser despreciable. Por lo cual se puede aceptar la no colinialidad.



# 2.5 Independencia de los datos
####


## Recordemos que la independencia esta dada por la aleatoriedad. Solo podemos
# suponerla.
















# 2.6 Valores atipicos
####


# por medio de la función influence.measures()


# podremos ver que valores atipicos son influyentes en la medición.


atipi.poke <- influence.measures(nuevo.modelo)


#Al hacer summary, podemos ver que existen muchos valores atipicos que si son 
# influyentes en el modelo.







# 3. Conclusión
########################


# El modelo resulto nefasto, primeramente, el coeficiente de determinación
# daba información que solo se podria explicar un 34% del peso de un pokemon
# respecto a las estadisticas bases.

# Tampoco se cumplieron los supuestos de normalidad, y homocedasticidad 
# para el modelo, adempas este posee muchos datos atipicos lo que afectan al 
# modelo de sobremanera. 

# En sinstesis, este modelo no funciona para nada.











################
# Ejercicio propuesto: 


# dado del datset incluido en R -> state.x77

# En este caso vamos a estudiar si existe alguna relación entre la 
# esperanza de vida de una persona respecto a otras variables, 


datos <- as.data.frame(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)
datos <- mutate(.data = datos, densidad_pobl = habitantes * 1000 / area)



