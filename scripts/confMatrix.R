conf.matrix <- function(regresion,test,cutoff) {


    predicted_value <- predict.glm(regresion,test,type = "response")
    
    predicted_class <- ifelse(predicted_value > cutoff, 1,0)
    
    performance_data<-data.frame(observed=test$num,
                                 predicted= predicted_class)
    
    
    positive <- sum(performance_data$observed==1,na.rm = TRUE)
    
    negative <- sum(performance_data$observed==0, na.rm = TRUE)
    
    predicted_positive <- sum(performance_data$predicted==1,na.rm = TRUE)
    
    predicted_negative <- sum(performance_data$predicted==0,na.rm = TRUE)
    
    matriz.conf.totals <- data.frame(positive, negative,predicted_positive,predicted_negative)
    
    
    
    
    tp<-sum(performance_data$observed==1 & performance_data$predicted==1,na.rm = TRUE)
    tn<-sum(performance_data$observed==0 & performance_data$predicted==0,na.rm = TRUE)
    fp<-sum(performance_data$observed==0 & performance_data$predicted==1,na.rm = TRUE)
    fn<-sum(performance_data$observed==1 & performance_data$predicted==0,na.rm = TRUE)
    
    predict.enfermos <- c(tp,fp,sum(tp,fp))
    predict.sanos <- c(fn,tn,sum(fn,tn))
    totales <- c(sum(tp,fn),sum(fp,tn),sum(sum(tp,fn),sum(fp,tn)))
    
    total <- sum(sum(tp,fn),sum(fp,tn))
    
    
    matriz.conf <- data.frame(predict.enfermos,predict.sanos,totales)
    row.names(matriz.conf) <- c("real.enfermos","real.sanos","total")
    
    accuracy <- (tp+tn)/total
    error_rate <- (fp+fn)/total
    sensitivity <- tp/positive
    especificity <- tn/negative
    precision <- tp/predicted_positive
    npv <- tn / predicted_negative
    
    data <- data.frame(accuracy,error_rate,sensitivity,especificity,precision,npv)
    
    
    return(list(matrix=matriz.conf,data=data))

}