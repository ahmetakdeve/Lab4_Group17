library(ggplot2)
#tmp<-linreg(lm(Petal.Length~Species),data=iris)

#The print() Method
print.linreg<-function(object){
  colnames(object[[1]])<-"Coefficients"
  return(object[[1]])
}
print(tmp)

#The plot() methods

plot.linreg<-function(object){
  my_df<-data.frame("resids"=object[["resids"]],"yhat"=object[["yhat"]])
  the_res_fit_plot<-ggplot(my_df,aes(y=resids,x=yhat))+geom_point()+theme_bw()+
    xlab("Fitted values")+ylab("Residuals")+ggtitle("Residuals vs Fitted ")+
    theme(plot.title = element_text(hjust=0.5))+geom_smooth(method = "auto",se = F,col="red")
  
  
  sqrt_stand_res<-sqrt((object[["resids"]]-mean(object[["resids"]]))/sd(object[["resids"]]))
  my_df2<-data.frame("std_res"<-sqrt_stand_res,"yhat"=object[["yhat"]])
  scale_location_plot<-ggplot(my_df2,aes(y=std_res,x=yhat))+geom_point()+theme_bw()+
    xlab("Fitted values")+ylab("SQRT(Standardized residuals)")+ggtitle("Scale-Location")+
    theme(plot.title = element_text(hjust=0.5))+geom_smooth(method = "auto",se = F,col="red")
  
  print(the_res_fit_plot)
  print(scale_location_plot)
}

plot(tmp)


#Resid Method
resid<-function(object)UseMethod("resid")

resid.linreg<-function(object){
  return(object[[3]])
}
resid(tmp)


#Pred Method
pred<-function(object)UseMethod("pred")
pred.linreg<-function(object){
  return(object[[2]])
}
pred(tmp)

#Coef Method

#Summary method
summary.linreg<-function(object){
first<-cbind(object[[1]],as.matrix(sqrt(object[["var_reg_coef"]])),object[[7]],object[[8]])
colnames(first)<-c("Coefficients","St Errors","T-values","P-values")
sigma<-as.numeric(sqrt(object[["res_var"]]))
degrees<-object[["df"]]
return(list("Model"=first,"Sigma"=sigma,"df"=degrees))
}
summary(tmp)


methods(class="linreg")
