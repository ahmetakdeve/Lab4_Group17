#' Creating linear model
#'
#' Linreg is a function which is fitting linear models
#'
#' @param formula The equation of the linear model
#' @param data The dataset used when creating linear models
#'
#' @return The function generates several components of the linear model, including regression coefficients, fitted values and the residual variance
#' 
#' 
#' @importFrom stats model.matrix pt 
#' @import utils
#' 
#' @export

linreg<-function(formula,data){
  data_name<-deparse(substitute(data))
  utils::str(myframe <- model.frame(formula, data))
  X <- model.matrix(formula,myframe)
  y<-myframe[,1]
  formula<-formula
  #Reg coefs
  reg_coef<-solve(t(X)%*%X)%*%t(X)%*%y
  
  #Fitted Values
  yhat<-X%*%reg_coef
  
  #The residuals
  resids<-y-X%*%reg_coef
  
  #Degrees of freedom
  df<-nrow(X)-ncol(X)
  
  #The residual variance
  res_var<-(t(resids)%*%resids)/df
  
  #The variance of reg coefs
  var_reg_coef<-diag(as.numeric(res_var)*solve(t(X)%*%X))
  
  #T-values of each coef
  t_values<-reg_coef/sqrt(var_reg_coef)
  
  #P-values of each coef
  p_values<-2*pt(-abs(t_values),df)
  
  stats_list<-list("reg_coef"=reg_coef,"yhat"=yhat,"resids"=resids,"df"=df,
                   "res_var"=res_var,"var_reg_coef"=var_reg_coef,
                   "t_values"=t_values,"p_values"=p_values,"formula"=formula,"data"=data_name)
  
  class(stats_list)<-"linreg"
  
  return(stats_list)
}
linreg_mod <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)


#' Print plots
#' 
#' Predicted values vs residuals and the Scale-location plots can be created by this function 
#' 
#' @param x A object of class "linreg"
#' @param ... Arguments belonging to the generic function
#' 
#' @import ggplot2
#' @export

plot.linreg<-function(x,...){
  liu_theme<-theme_bw()+theme(panel.border = element_rect(color = "#00b9e7", size = 2),
                              panel.background = element_rect(fill="#00b9e7"))
  
  my_df<-data.frame("resids"=x[["resids"]],"yhat"=x[["yhat"]])
  the_res_fit_plot<-ggplot(my_df,aes(y=resids,x=yhat))+geom_point()+liu_theme+
    xlab("Fitted values")+ylab("Residuals")+ggtitle("Residuals vs Fitted ")+
    theme(plot.title = element_text(hjust=0.5))+geom_smooth(method = "lm",se = F,col="red")
  
  
  sqrt_stand_res<-sqrt((x[["resids"]]-mean(x[["resids"]]))/sd(x[["resids"]]))
  my_df2<-data.frame("std_res"<-sqrt_stand_res,"yhat"=x[["yhat"]])
  scale_location_plot<-ggplot(my_df2,aes(y=std_res,x=yhat))+geom_point()+liu_theme+
    xlab("Fitted values")+ylab("SQRT(Standardized residuals)")+ggtitle("Scale-Location")+
    theme(plot.title = element_text(hjust=0.5))+geom_smooth(method = "lm",se = F,col="red")
  
  print(the_res_fit_plot)
  print(scale_location_plot)
}


#' Prints the regression coefficients
#' 
#' More details
#' 
#' @param x A object of class "linreg"
#' @param ... Arguments belonging to the generic function can be used here
#' 
#' @export

print.linreg<-function(x,...){
  theformula<-as.character(x[["formula"]])  
  mycall<-paste0("linreg(","formula = ",paste(theformula[2],"~ "),theformula[3],
                 paste(", data =",paste0(x[["data"]],")")))
  print(mycall)
  coeffs<-as.vector(x[[1]])
  names(coeffs)<-rownames(x[[1]])
  print(coeffs)
}
print(linreg_mod)

resid<-function(object)UseMethod("resid")


#' Prints out the residuals of the model
#' 
#' Prints the residuals 
#' 
#' @param object A object of class "linreg"
#' 
#' @export

resid.linreg<-function(object){
  return(object[[3]])
}



pred<-function(object)UseMethod("pred")

#' Print values
#' 
#' Output the predicted values of the dependent variable
#' 
#' @param object A object of class "linreg"
#' 
#' @export


pred.linreg<-function(object){
  return(object[[2]])
}


#' Prints out the summary of the model
#' 
#' Prints the summary 
#' 
#' @param object A object of class "linreg"
#' @param ... Arguments belonging to the generic function
#' 
#' @export


summary.linreg<-function(object,...){
  first<-cbind(object[[1]],as.matrix(sqrt(object[["var_reg_coef"]])),object[[7]],object[[8]])
  first<-as.data.frame(first)
  first$stars<-"***"
  colnames(first)<-c("Coefficients","St Errors","T-values","P-values","")
  print(first)
  sigma<-as.numeric(sqrt(object[["res_var"]]))
  degrees<-object[["df"]]
  lastline<-paste("Residual standard error:",round(sigma,5),"on",degrees,"degrees of freedom")
  print(lastline)
}


#' Regression coefficients
#' 
#' Prints the regression coefficients as a named vector
#' 
#' @param object A object of class "linreg"
#' @param ... Arguments belonging to the generic function can be used here
#' 
#' @export

coef.linreg<-function(object,...){
  coef_vector<-as.vector(object[[1]])
  names(coef_vector)<-rownames(object[[1]])
  return(coef_vector)
}

