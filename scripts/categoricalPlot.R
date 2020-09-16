


bar.plot.categorical <- function(my_df){
  
  
  
  ######### sex plot #########
  
  sex.df <- count(data.frame(sex=my_df$sex,disease=my_df$disease))
  
  sex.plot <- ggplot(data=sex.df, aes(x=sex, y=freq, fill=disease)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=freq), vjust=1, color="black",
              position = position_dodge(0.9), size=3) +
              ggtitle("Frecuency Plot for sex")
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

  
  
  ######### cp plot #########
  
  cp.df <- count(data.frame(cp=my_df$cp,disease=my_df$disease))
  
  cp.plot <- ggplot(data=cp.df, aes(x=cp, y=freq, fill=disease)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=freq), vjust=1, color="black",
              position = position_dodge(0.9), size=3) +
              ggtitle("Frecuency Plot for cp")
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

  
  
  ######### fbs plot #########
  
  fbs.df <- count(data.frame(fbs=my_df$fbs,disease=my_df$disease))
  
  fbs.plot <- ggplot(data=fbs.df, aes(x=fbs, y=freq, fill=disease)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=freq), vjust=1, color="black",
              position = position_dodge(0.9), size=3) +
              ggtitle("Frecuency Plot for fbs")
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

  
  ######### restecg plot #########
  
  restecg.df <- count(data.frame(restecg=my_df$restecg,disease=my_df$disease))
  
  restecg.plot <- ggplot(data=restecg.df, aes(x=restecg, y=freq, fill=disease)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=freq), vjust=1, color="black",
              position = position_dodge(0.9), size=3) +
              ggtitle("Frecuency Plot for restecg")
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
 
  
  ######### exang plot #########
  
  exang.df <- count(data.frame(exang=my_df$exang,disease=my_df$disease))
  
  exang.plot <- ggplot(data=exang.df, aes(x=exang, y=freq, fill=disease)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=freq), vjust=1, color="black",
              position = position_dodge(0.9), size=3) +
              ggtitle("Frecuency Plot for exang")
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
                           
  
  
  ######### slope plot #########
  
  slope.df <- count(data.frame(slope=my_df$slope,disease=my_df$disease))
  
  slope.plot <- ggplot(data=slope.df, aes(x=slope, y=freq, fill=disease)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=freq), vjust=1, color="black",
              position = position_dodge(0.9), size=3) +
              ggtitle("Frecuency Plot for slope")
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

  
  
  ######### thal plot #########
  
  thal.df <- count(data.frame(thal=my_df$thal,disease=my_df$disease))
  
  thal.plot <- ggplot(data=thal.df, aes(x=thal, y=freq, fill=disease)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=freq), vjust=1, color="black",
              position = position_dodge(0.9), size=3) +
              ggtitle("Frecuency Plot for thal")
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
              
  
  ## multiplot!!
  
  all.plot <- ggarrange(sex.plot,cp.plot,fbs.plot,restecg.plot,exang.plot,slope.plot,thal.plot)
  
  return(all.plot)
  
}