
###################################
#     Paquetes utilizados:        #


#install.packages("tidyverse")
#install.packages("ggpubr")
#install.packages("plyr")
#install.packages("lsr")
#install.packages("corrplot")


library(tidyverse)
library(ggpubr);
library(plyr);
library(lsr)
library(corrplot)




###################################



# Estructura del main

# Es el archivo principal para el llamado de funciones de todo el c√≥digo
# primeramente se carga todo el dataset limpio para el tabajo de analisis


# La Estadistica descriptiva que se hace se ha separado en dos partes:

#       1-  Descriptiva para las variables numericas

#       2-  Descriptiva para las variables categoricas


# Luego de esto se hace estadistica inferencial de la forma:

#       1-  Docimas de contraste para las variable numericas

#       2-  Regresi√≥n lineal para generar el modelo de
#           personas sanas o enfermas




dirstudio <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(dirstudio) 

if(!exists("getAllData", mode="function")) source("scripts/genereteProccessed.R")
if(!exists("showCorplot", mode="function")) source("scripts/corAnalysis.R")
if(!exists("normal.test.2df", mode="function")) source("scripts/statisticalTest.R")
if(!exists("getAnalysis", mode="function")) source("scripts/getAnalysis.R")
if(!exists("bar.plot.categorical", mode="function")) source("scripts/categoricalPlot.R")
if(!exists("showBoxplot", mode="function")) source("scripts/generateBoxplot.R")
if(!exists("conf.matrix ", mode="function")) source("scripts/confMatrix.R")



#################
# DATOS LIMPIOS #
#################


# Los datos limpios son una serie de dataframes
# que contienen la informaci√≥n ya limpia de todo
# el data set. Tambien contienen informaci√≥n
# diferenciada segun se especifique:



#     all.df :  Contiene toda la informaci√≥n del dataset
#               incluida la informaci√≥n del lugar de donde
#               fueron obtenidos (cleve,swit,hung,va)
all.df <- getAllData()

#     mixed.all.df :  Contiene toda la informaci√≥n limpia
#                     y legible con con la interpretaci√≥n
#                     categorica de los numeros.
mixed.all.df <- getMixedData(all.df)


#     var.numerical : Es un vector quer indica las columnas
#                     que son numericas.
var.numerical <- c(1,4,5,8,10,12)

#     var.categorial : Es un vector quer indica las columnas
#                     que son categoricas y dicotomicas.

var.categorial <- c(2,3,6,7,9,11,13,14)


#     numerical.df :  Dataframe general pero solo con
#                     variables numericas
numerical.df <- all.df[var.numerical]


#     healthy : Es un dataframe que proviene de all.df y
#               contiene toda la informaci√≥n de las personas
#               sanas del coraz√≥n
healthy <- all.df[all.df$num == 0,] 


#     sicky :   Es un dataframe que proviene de all.df y
#               contiene toda la informaci√≥n de las personas
#               enfermas del coraz√≥n
sicky <- all.df[all.df$num > 0,] 


#     numerical.health :  Dataframe de personas sanas pero
#                         solo con variables numericas
numerical.health <- healthy[var.numerical]  


#     numerical.sick :    Dataframe de personas enfermas 
#                         pero solo con variables numericas
numerical.sick <- sicky[var.numerical]


#     categorical.health :  Dataframe de personas sanas pero
#                           solo con variables categoricas
categorical.health <- healthy[var.categorial]  


#     categorical.sick :    Dataframe de personas enfermas 
#                           pero solo con variables categoricas
categorical.sick <- sicky[var.categorial]


####################################################
#                                                  #  
#         I.    ESTADISTICA DESCRIPTIVA            #
#                                                  #
####################################################

#   1- Variables numericas
################################


var.numerical.loc <- c(var.numerical, 15) # nueva var numerica con locacion

numerical.loc.df <- all.df[var.numerical.loc] 

all.analysis <- getAnalysis(numerical.df) # analisis completo


##Analisis de enfermos y sanos
health.analysis <- getAnalysis(numerical.health)

sick.analysis <- getAnalysis(numerical.sick)

##Generacion de boxplot para los datos numericos 

numerical.boxplot <- showBoxplot(numerical.health,numerical.sick)

## Matriz de correlaci√≥n

showCorplot(all.df,var.numerical)

# como se puede ver la relaci√≥n que se ve en la matriz
# es muy debil lo cual significa que no existen relaciones
# del tipo lineal entre las variables, o tambien que
# las variables tienen valores muy extremos que afectaton
# la medicion de la correlaci√≥n.


#   1- Variables categoricas
################################



#tabla de frecuencias para variables categoricas

frecuency.table <- count(mixed.all.df[var.categorial])

# sin embargo esta tabla resulta tener mucha informaciÛn
# debido a la cantidad de variables presentes. Pero como se
# estan estudiando las clases (sanos y enfermos), se puede
# mostrar informaciÛn relacionada a cada clase

# Entonces ¬ø Qu√© frecuencia tienen las variables segun sanos
# y enfermos ?


categorical.plots <- bar.plot.categorical(mixed.all.df)




####################################################
#                                                  #  
#         II.    ESTADISTICA INFERENCIAL           #
#                                                  #
####################################################



# Test de medias para dos grupos -> Sanos y enfermos
###############################################################

# para poder realizar este tes como corresponde, es necesario
# separar las variables numericas a trabajar.

# Primeramente hay que verificar que las variables a contrastar se
# distribuyan de una forma normal

# lista de 2 df que contiene los p-valor para cada una de las variables
# numericas de los dos grupos a analizar

df.normal.result <- normal.test.2df(numerical.health,numerical.sick)


alpha <- 0.05

normal.dist.health <- df.normal.result[[1]]$names[df.normal.result[[1]]$p.value > alpha]

normal.dist.sick <- df.normal.result[[2]]$names[df.normal.result[[2]]$p.value > alpha]

# Como se puede ver ningun grupo comparte la normalidad en sus variables
# por lo que es necesario utilizar una prueba no parametrica

## test no parametrico ##



# Entonces una alternativa no parametrica para prueba de medias seria Mann-Whitney
result.maan <- mann.whitney.2df.test(numerical.health,numerical.sick) 

# dados los resutados de Mann-Whitney, se puede ver que las caracteristicas
# de los dos grupos difieren, por lo que se podria pensar que 
# estas caracteristicas podrian ser significativas al momento de 
# discriminar a un enfermo de coraz√≥n

# En sistesis en test realizado dice que no existe una igualdad en la distribuciÛn
# de las entre las variables de sanos y enfermos








######### Matriz de correlaciÛn variables categoricas #########
# La matriz fue generada con test Chi^2 y cramer V

# !!!!! Las funciones acontinuacion no fueron posible
# separarlas del main ya que ocupan valores de este mismo
# que no pueden ser pasador por argumento

# function de mapeo de los test chi t cramV
test.chi.cram <- function(x,y) {
  tbl = mixed.all.df[var.categorial] %>% select(x,y) %>% table()
  chisq_pval = round(chisq.test(tbl)$p.value, 4)
  cramV = round(cramersV(tbl), 4) 
  data.frame(x, y, chisq_pval, cramV) }



categorical.cor <- function(df.categorical){
  
  df_comb = data.frame(t(combn(sort(names(df.categorical)), 2)), stringsAsFactors = F)
  
  # apply function to each variable combination
  df_res = map2_df(df_comb$X1, df_comb$X2, test.chi.cram)
  
  # plot results
  result <-df_res %>%
    ggplot(aes(x,y,fill=cramV))+
    geom_tile()+
    geom_text(aes(x,y,label=cramV))+
    scale_fill_gradient(low="red", high="yellow")+
    theme_classic()
  
  return(result)
  
  
}

cor.categ <- categorical.cor(mixed.all.df[var.categorial])

# en este caso si se puede ver una relaciÛn entre las variables
# aun que no sean tan fuertes, si exite y se puede concluir con estos 
# datos

# El test de Chi^2 no arrojo mayor informaciÛn , esto debido a
# que los datos tienen mucha presencia de NA

# como el test CHi^2 no da mayor informaciÛn, se ha decidido ingnorarlo




############### regresion logistica ###############

# Para obtener un modelo de regresion legistica acorde al estudio
# es necesario aplicar una regresiÛn logistica multiple para decir
# la probabilidad de que una persona sea sana o enferma.


# para hacer el modelo es necesario tomar todo el dataset
# sin incluir la separaciÛn por locaciÛn ya que puede
# afectar al modelo. Entonces:

# se compara todo el dataset contra num ( resultado de sano o enfemo )


## Primeraremnte se har· uso de todo el data set para estudiar
# la significaciÛn de las variables:

reg.log.multi <- glm(num ~ ., family = "binomial", data = all.df[,-c(12,13,15)])


result.reg <- summary(reg.log.multi)

# Del resultado de esto se puede ver que las variables mas significativas
# son :

#       - sex(2)
#       - cp(3)
#       - oldpeak(10)



# y las variables menos significativas son :

#       - age(1)
#       - fbs(6)
#       - restecg(7)



# como estas variables no son significativas para el modelo
# por lo que se pueden omitir y volver a realizar el modelo


# Como ya se sabe que variables son las significativas 
# podemos volver a crear un modelo pero esta vez
# se separar· en dos grupos, uno de entrenamiento y 
# de prueba


sig.all.df <- all.df[,-c(1,6,7,12,13,15)] # data set significativo


set.seed(985426)

ntrain <- nrow(sig.all.df) * 0.7

index_train<-sample(1:nrow(sig.all.df),size = ntrain)

train.df<-sig.all.df[index_train,]

test.df<-sig.all.df[-index_train,]




high.reg <- glm(num ~ ., family = "binomial", data = train.df)

high.result <- summary(high.reg)


##### matriz de confucion en r #####

matrix <- conf.matrix(high.reg, test.df, 0.5)

# matriz de confucion
conf.matriz<- matrix$matrix

#datos del mismo modelo
model.data <- matrix$data




