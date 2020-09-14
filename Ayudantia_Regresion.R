##############################
#  Ayudantia de regresion
##############################

library(ggplot2)
library(lmtest)

# 1 - Teoria de regresión lineal Simple

# 2- Ejercicio practico.


##############################
# Regresion lineal simple.


# los datos con los cuales vamos a trabajar es el dataset de IRIS
# el cual esta disponible en R como 'iris'

datos <- iris[1:4]


# Para la regresión lineal simple estamos buscando la relación que
# existe entre dos variables.

# Para este ejemplo nos iteresa saber si existe una relación entre

# el ancho de un petalo y el largo de un petalo. 


# 1. visualización de los datos:
#################################

gra1 <- ggplot(data = datos, mapping = aes(x = Petal.Length, y = Petal.Width)) +
          geom_point(color = "firebrick", size = 2) +
          labs(title  =  'Diagrama de dispersión', x  =  ' Largo de petalo ', 
               y = 'Ancho de petalo') +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))








# Claramente existe una relación entre el Largo y Ancho de un petalo. 
# para ver que tan fuerte es este, podemos hacer una analisis de correlación






    # 1.1 correlación
    ####

# Primeramente asumamos que los datos se distribuyen de forma normal.

cor.petal <- cor.test(x=datos$Petal.Length, 
                      y=datos$Petal.Width, method = 'pearson')








# Existe una relación casi perfecta entre el largo y el ancho de un petalo
# dado que la correlación es 0.9628654 y el test es muy significativo
# p-value < 2.2e-16












# 2. Generación del modelo
#################################

# Como ya sabemos que existe una correlación entre las variables podemos aplicar
# un modelo de regresión lineal simple:




modelo <- lm(formula = Petal.Width ~ Petal.Length, datos )






gra2 <- ggplot(data = datos, mapping = aes(x = Petal.Length, y = Petal.Width)) +
          geom_point(color = "firebrick", size = 2) +
          labs(title  =  'Ancho ~ Largo') +
          geom_smooth(method = "lm", se = FALSE, color = "black") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))











# Este modelo es casi perfecto en cuanto a los valores de f.test y r^2 
# Pero aun no podemos aceptarlo debido a que hay condiciones que se deben
# cumplir:





  # 2.1 Independencia
  ####

# Para el caso de la independencia , tenemos que asumir que las muestras
# vienen de forma aleatoria.




  # 2.2 Linealidad
  ####

# Para ver esto primeramente se necesitan obtener los residuos.

lineal <- data.frame(valor=modelo$fitted.values,residuos=modelo$residuals)


gra3 <- ggplot(data = lineal, aes(x = valor, y = residuos)) +
            geom_point(aes(color = residuos)) +
            scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
            geom_hline(yintercept = 0) +
            geom_segment(aes(xend = valor, yend = 0), alpha = 0.2) +
            labs(title = "Distribución de los residuos", x = "predicción modelo",
                 y = "residuo") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5), legend.position = "none")









# Los residuos en cierta parte tienen a tener otra distribución que no sea
# Aleatorea, sin embargo al ver el panorama completo, se puede ver que no
# existe un patron. Por lo cual se puede aceptar la linealidad 







  # 2.3 Residuos normales
  ####


# Para estudiar esto, solo hace falta analizar como se comporta la distribución
# de los residuos.

# para esto se puede hacer un test de shapiro o un analisis grafico.


## Metodo grafico
qqnorm(lineal$residuos)
qqline(lineal$residuos)


## Metodo por test


normal.residuals <- shapiro.test(lineal$residuos)











# El grafico Q-Q nos da info que los residuos se comportan aproximadamente
# normal. Sin embargo el test de shapiro sobre linealidad, no dice que se puede
# aceptar pero tambien esta en el limite por lo cual, se podria aceptar que los 
# residuos se comporten de forma normal pero con precausión.









# 2.4 Homocedasticidad de los residuos 
####




# Al igual que la normalidad esto se puede ver de forma grafica, donde si 
# se pone una linea que muestre que tanto varian los residuos, la linea tendria
# que ser lo más cercano a una recta.


gra4 <- ggplot(data = lineal, aes(x = valor, y = residuos)) +
    geom_point(aes(color = residuos)) +
    scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
    geom_segment(aes(xend = valor, yend = 0), alpha = 0.2) +
    geom_smooth(se = FALSE, color = "firebrick") +
    labs(title = "Distribución de los residuos", x = "predicción modelo",
         y = "residuo") +
    geom_hline(yintercept = 0) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")




## Dado que la linea no es del todo una recta, se podria hacer el test de 
# Breush-Pagan para saber realmente si se tiene homocedasticidad o no.


test.bp <- bptest(modelo)




# Tanto el grafico como el test nos indican que no existe una homocedasticidad
# entre los residuos por lo que indica que al haber mucha varianza el modelo
# no sirve para un caso general por lo cual de aceptarse  se tendria que tener
# precausión con los datos entregados.

# Una forma de arreglar estos datos es la eliminación de los valores
# de gran impacto al modelo, como los valores atipicos.



# recordemos que esta parte de analisis es una iteración constante hasta
# que se tenga un buen modelo.














# Conclusión:


# Si bien el modelo se logra ajustar muchos a los datos, no es posible aplicar
# este modelo de forma general ya que no cumple con el supuesto de Homocedasticidad
# Dicho de otro modo, es posible que el modelo se ajuste dado que existe
# mucha varianza entre los datos. Una forma de arreglar esto, es la eliminación
# de outliers y volver a armar el modelo.









### Ahora ejercicio para ustedes:


## Vamos a ver si existe una relación entre los kilimetros recorridos por
# un transporte publico y la cantidad de fallos que se tienen



kilometros <- c(5659,  5710, 5563, 5672, 5532, 5600, 5518, 5447, 5544, 5598,
                   5585, 5436, 5549, 5612, 5513, 5579, 5502, 5509, 5421, 5559,
                   5487, 5508, 5421, 5452, 5436, 5528, 5441, 5486, 5417, 5421)
fallos <- c(855, 875, 787, 730, 762, 718, 867, 721, 735, 615, 708, 644, 654, 735,
          667, 713, 654, 704, 731, 743, 619, 625, 610, 645, 707, 641, 624, 570,
          593, 556)
datos.ejercicio <- data.frame(kilometros,fallos)







