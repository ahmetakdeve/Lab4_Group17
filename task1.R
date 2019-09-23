#' Creating linear model
#'
#' Linreg is a function which is fitting linear models
#'
#' @param formula The equation of the linear model
#' @param data The dataset used when creating linear models
#'
#' @return The function generates several components of the linear model, including regression coefficients, fitted values and the residual variance.
#'
#' @export


linreg<-function(formula,data){
  attach(data)
  X<-model.matrix(formula)
  index_dep<-all.vars(formula$call)[1]
  y<-data[,index_dep]
  
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
                   "res_var"=res_var,"var_reg_coef"=var_reg_coef,"t_values"=t_values,"p_values"=p_values)
  
  class(stats_list)<-"linreg"
  return(stats_list)
  detach(data)
}

