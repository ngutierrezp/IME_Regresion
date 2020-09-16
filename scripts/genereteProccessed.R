### Script para creación de una gran dataset



getAllData <- function() { 
  
  processed.headers = c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
  
  
  
  ## Dataset Cleveland 
  
  cleveland <- read.table("data/processed.cleveland.data",header = F, sep = ",", col.names = processed.headers)
  
  cleveland$loc <- rep(1, nrow(cleveland))
  
  ## Dataset hungarian 
  
  hungarian <- read.table("data/processed.hungarian.data",header = F, sep = ",", col.names = processed.headers)
  
  hungarian$loc <- rep(3, nrow(hungarian))
  
  ## Dataset switzerland
  
  
  switzerland <- read.table("data/processed.switzerland.data",header = F, sep = ",", col.names = processed.headers)
  
  
  switzerland$loc <- rep(2, nrow(switzerland))
  
  ## Dataset va
  
  
  va <- read.table("data/processed.va.data",header = F, sep = ",", col.names = processed.headers)
  
  va$loc <- rep(4, nrow(va))
  
  all <-rbind(cleveland,hungarian,switzerland,va)
  
  
  all$age = as.integer(all$age)
  all$sex = as.integer(all$sex)
  all$cp = as.integer(all$cp)
  all$trestbps = as.integer(all$trestbps)
  all$chol = as.integer(all$chol)
  all$fbs = as.integer(all$fbs)
  all$restecg = as.integer(all$restecg)
  all$thalach = as.integer(all$thalach)
  all$exang = as.integer(all$exang)
  all$oldpeak = as.double(all$oldpeak)
  all$slope = as.integer(all$slope)
  all$ca = as.integer(all$ca)
  all$thal = as.integer(all$thal)
  all$num = as.integer(all$num)
  
  
  all$trestbps <- replace(all$trestbps,all$trestbps==0,NA)
  
  
  
  
  all$chol <- replace(all$chol,all$chol==0,NA)
  
  all$num = ifelse(all$num == 0, 0, 1)
  
  
  
  
  return(all)
  
}


getMixedData <- function(df){
  
  age <- df$age
  
  sex <- ifelse(df$sex == 1, "Male","Female")
  
  cp <- df$cp
  cp <- replace(cp,cp==1,"typical angina")
  cp <- replace(cp,cp==2,"atypical angina")
  cp <- replace(cp,cp==3,"non-anginal pain")
  cp <- replace(cp,cp==4,"asymptomatic")
  
  trestbps <- df$trestbps
  
  chol <- df$chol
  
  fbs <- ifelse(df$fbs == 1, "Yes","No")
  
  restecg <-df$restecg
  restecg <- replace(restecg,restecg==0,"normal")
  restecg <- replace(restecg,restecg==1," wave abnormality")
  restecg <- replace(restecg,restecg==2," ventricular hypertrophy")
  
  thalach <-df$thalach
  
  exang <- ifelse(df$exang == 1, "Yes","No")
  
  oldpeak <- df$oldpeak
  
  slope <- df$slope
  slope <- replace(slope,slope==1,"upsloping")
  slope <- replace(slope,slope==2,"flat")
  slope <- replace(slope,slope==3,"downsloping")
  
  ca <- df$ca
  
  thal <- df$thal
  thal <- replace(thal,thal==1,NA)
  thal <- replace(thal,thal==3,"normal")
  thal <- replace(thal,thal==6,"fixed defect")
  thal <- replace(thal,thal==7,"reversable defect")
  
  disease <- ifelse(df$num == 1, "Yes","No")
  
  loc <- df$loc
  loc <- replace(loc,loc==1,"cleve")
  loc <- replace(loc,loc==2,"switz")
  loc <- replace(loc,loc==3,"hung")
  loc <- replace(loc,loc==4,"va")
  
  
  mix <- data.frame(age,sex,cp,trestbps,chol,fbs,restecg,thalach,exang,oldpeak,slope,ca,thal,disease,loc)
  mix$sex <-as.character(mix$sex)
  mix$cp <-as.character(mix$cp)
  mix$fbs <-as.character(mix$fbs)
  mix$restecg <-as.character(mix$restecg)
  mix$exang <-as.character(mix$exang)
  mix$slope <-as.character(mix$slope)
  mix$thal <-as.character(mix$thal)
  mix$loc <-as.character(mix$loc)
  
  
  return(mix)
}


