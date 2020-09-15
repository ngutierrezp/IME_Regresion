
library(ggplot2)
library(lmtest)
library(psych)
library(corrplot)
library(gridExtra)
library(car)
library(dplyr)



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



## Este ejercicio fue propuesto por Joaquín Amat Rodrigo j.amatrodrigo@gmail.com
# rescatado de su pagina  cienciadedatos.net


## Si tienen tiempo puede echar un ojo al trabajo de Joaquin ya que tiene
# temas bastantes interesantes respecto al data science.


