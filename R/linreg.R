################################################################################
#
#name = "Salvador Marti Roman & Eleftheria Chatzitheodoridou"
#liuid = "salma742 & elech646"
#
# 732A94 Advanced R Programming
# Computer lab 4#
#lab_path = "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab4.yml"
# package.skeleton(name="lab4")
# Deadline: 22 September 23:59
################################################################################


#' Linreg Class
#' 
#' This class performs linear regression and provides different methods to display the data. It also prints the plots for Residual vs. Fitted Values and Scale-Location
#' 
#' The algorithm used for this function can be found on 
#' \url{https://en.wikipedia.org/wiki/Linear_regression}
#' 
#' @field balance matrix. Where is balance bruh.
#' @field coefficients matrix. Regression Coefficients.
#' @field fitted matrix. Fitted Values.
#' @field residuals matrix. Residuals.
#' @field degrees_of_freedom numeric. Degrees of freedom.
#' @field resvar matrix. Residual Variance.
#' @field coefficients_variance matrix. Variance of the Regression Coefficients.
#' @field formula formula. The Formula for the Linear Regression.
#' @field pvalues vector. P-values.
#' @field tvalues vector. T-values for each Coefficient.
#' @field data_name character. The Given Data.
#' @field data data.frame. The given data frame.
#' @field call_params character. A pairlist of all active calls.
#' @field outliers vector. Outliers.
#' 
#' @return Nothing.
#' @export linreg
#' @exportClass linreg

linreg <- setRefClass("linreg",
                       fields = list(balance = "numeric",
                                     formula = "formula",
                                     data = "data.frame",
                                     data_name = "character",
                                     pvalues = "vector",
                                     tvalues = "vector",
                                     coefficients = "matrix",
                                     coefficients_variance = "matrix",
                                     call_params = "character",
                                     residuals = "matrix",
                                     fitted = "matrix",
                                     resvar = "matrix",
                                     outliers = "vector",
                                     degrees_of_freedom = "numeric"
                                     ),
                      
                      # Methods
                      methods = list(
                        fit = function(){
                          
                          #Independent variables
                          X = model.matrix(formula, data)
                          
                          variables = all.vars(formula)
                          #Pick the variable in the other side of the formula
                          y = data[all.vars(formula)[1]]
                          
                          X = as.matrix(X)
                          y = as.matrix(y)
                          
                          #Regression coefficients
                          beta_hat = as.matrix(solve(t(X) %*% X) %*% t(X) %*% y)
                          coefficients <<- beta_hat
                          
                          #fitted values
                          y_hat = X %*% beta_hat                                
                          fitted <<- y_hat
                          
                          #Residuals
                          e = y - y_hat                                         
                          residuals <<- e
                          
                          #Degrees of freedom (observations - parameters)
                          df = nrow(X) - ncol(X)
                          degrees_of_freedom <<- df
                          
                          #residual variance
                          s_hat = (t(e)%*%e)/df                      
                          resvar <<- s_hat
                          
                          #variance of regression coefficients
                          var_beta_hat = as.numeric(s_hat) * solve(t(X) %*% X) 
                          coefficients_variance <<- var_beta_hat
                          
                          #T-Values
                          t_val = as.vector(beta_hat) / sqrt(pmax(0,var_beta_hat))
                          tvalues <<- t_val
                          
                          #P-Values
                          p_val = 2*pt(-abs(t_val), df)
                          pvalues <<- p_val
                          
                        },
                        
                        quantile_outlier = function(x){
                          q1 <- quantile(x, 0.25)
                          q3 <- quantile(x, 0.75)
                          is_outlier = function(x){
                            # Checks for outliers
                            if((x > (q3 + 1.5*(q3-q1))) || (x < (q1 - 1.5*(q3-q1)))) 
                              return(TRUE)
                            else 
                              return(FALSE)
                          }
                          return(sapply(residuals, is_outlier))
                          
                          },
                        
                        plot = function(x){ 
                          # Plots Residuals vs. Fitted Values for p1 and Residuals vs. Standardized Residuals for p3
                          
                          library(ggplot2)
                          p1 <- ggplot(iris, aes(.self$fitted, .self$residuals)) + 
                            geom_point()+
                            #geom_point(data = .self$data[.self$outliers,], aes(.self$outliers, 0), shape = 1)+
                            stat_summary(fun=mean, fun.args = list(trim=0.25), colour="red", geom="line",group=1)+
                            labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
                            theme(axis.title.y = element_text(vjust = 0.5, size = 13, face = "bold")) +
                            theme(axis.title.x = element_text(vjust = 0.5, size = 13, face = "bold")) +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
                            theme_bw()
                          
                          p3 <- ggplot(iris, aes(.self$fitted, sqrt(abs(.self$residuals/sqrt(abs(.self$resvar)))))) + 
                            geom_point()+
                            #geom_point(data = .self$data[.self$outliers,], aes(.self$outliers, 0), shape = 1)+
                            stat_summary(fun=mean, fun.args = list(trim=0.25), colour="red", geom="line",group=1)+
                            labs(title = "Scale~Location", x = "Fitted values", y = expression(sqrt(Standardized~residuals))) +
                            theme(axis.title.y = element_text(vjust = 0.5, size = 13, face = "bold")) +
                            theme(axis.title.x = element_text(vjust = 0.5, size = 13, face = "bold")) +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
                            theme_bw() 
                          return(list(p1,p3))
                          },
                        # resid function, returns residuals 
                        resid = function(){return(.self$residuals)},
                        
                        # pred function, returns fitted values
                        pred = function(){return(.self$fitted)},
                        
                        # coef function, returns coefficients
                        coef = function(){return(.self$coefficients)},
                        
                        summary = function(){

                          for(i in 1:length(rownames(.self$coefficients))){
                            cat(paste(rownames(.self$coefficients)[i],.self$coefficients[i],
                                      sqrt(diag(.self$coefficients_variance)[i]),
                                      diag(matrix(.self$tvalues, nrow= sqrt(length(.self$tvalues)), ncol= sqrt(length(.self$tvalues))))[i],
                                      "***\n"))
                          }
                          cat(paste("Residual standard error:",
                                    sqrt(.self$resvar),
                                    "on",
                                    .self$degrees_of_freedom,
                                    "degrees of freedom"))
                          },
                        
                        summary2 = function(){
                          cat("Coefficients \n")
                          print(cbind(coefficients = .self$coefficients, Standard_Errors = diag(.self$coefficients_variance)))
                          print("T values and P values")
                          print(cbind(T_values=.self$tvalues, P_values=.self$pvalues))
                          cat(" Degrees of Freedom \n")
                          cat(.self$degrees_of_freedom)
                          },
                        
                        initialize = function(formula, data){
                          # Creating object. Arguments are the formula and corresponding data frame.
                          
                          stopifnot(inherits(formula,"formula"))
                          stopifnot(inherits(data,"data.frame"))
                          call_params <<- as.character(sys.calls()[1])  # returns pairlist of all the active calls
                          formula <<- formula
                          data <<- data
                          data_name <<- deparse(substitute(data))
                          .self$fit()
                          .self$outliers <<- .self$quantile_outlier(.self$residuals)
                        },
                        
                        print = function(){ 
                          cat("Call: \n")
                          cat(paste("linreg(formula = "), format(.self$formula), ", data = ", .self$data_name, ")\n", sep = "")
                          cat("Coefficients: \n")
                          cat(paste(rownames(.self$coefficients),sep = "\t\t\t"))
                          cat("\n")
                          cat(paste(round(.self$coefficients,8),sep = "\t\t\t"))
                        }
                      )
          )

#custom_reg = Linreg(formula = Petal.Length ~ Species, data = iris)

#custom_reg$fit()
#print(custom_reg)

#library(ggplot2)
#d <- iris
#fit <- Linreg(formula = Petal.Length ~ Species, data = d)
#linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#linreg_mod$print()
#linreg_mod$summary()
#linreg_mod$plot()
#linreg_mod$resid()
#linreg_mod$pred()
#linreg_mod$coeff()