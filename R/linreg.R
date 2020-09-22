data("iris")
# ' @export
Linreg <- setRefClass("Linreg",
                       fields = list(balance = "numeric",
                                     formula = "formula",
                                     data = "data.frame",
                                     pvalues = "vector",
                                     tvalues = "vector",
                                     coefficients = "matrix",
                                     call_params = "character",
                                     residuals = "matrix",
                                     fitted = "matrix",
                                     resvar = "vector",
                                     outliers = "vector",
                                     degrees_of_freedom = "numeric"
                                     ),
                      
                      methods = list(
                        test1 = function() print(data),
                        test2 = function() print("RC test 2"),
                        fit = function(){
                          
                          #Independent variables
                          X = model.matrix(formula, data)
                          
                          variables = all.vars(formula)
                          #Pick the variable in the other side of the formula
                          y = data[variables[1:length(variables)-1]]
                          
                          X = as.matrix(X)
                          y = as.matrix(y)
                          
                          #Regression coefficients
                          beta_hat = (solve(t(X) %*% X) %*% t(X) %*% y)
                          coefficients <<- beta_hat
                          
                          y_hat = X %*% beta_hat                                #fitted values
                          fitted <<- y_hat
                          
                          e = y - y_hat                                         #residuals
                          residuals <<- e

                          df = nrow(X) - ncol(X)
                          degrees_of_freedom <<- df
                          
                          s_hat = as.vector((t(e)%*%e)/df)                      #residual variance
                          resvar <<- s_hat
                          
                          var_beta_hat = s_hat * solve(t(X) %*% X)              #variance of regression coefficients
                          #print(var_beta_hat)
                          t_val = as.vector(beta_hat) / sqrt(pmax(0,var_beta_hat))                   #t-values
                          tvalues <<- t_val
                          p_val = 2*pt(-abs(t_val), df)
                          pvalues <<- p_val
                          
                        },
                        
                        quantile_outlier = function(x){
                          q1 <- quantile(x, 0.25)
                          q3 <- quantile(x, 0.75)
                          is_outlier = function(x){
                            if((x > (q3 + 1.5*(q3-q1))) || (x < (q1 - 1.5*(q3-q1)))) 
                              return(TRUE)
                            else 
                              return(FALSE)
                          }
                          return(sapply(residuals, is_outlier))
                          
                          },
                        plot = function(x){ 
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
                        resid = function(){return(.self$residuals)},
                        pred = function(){return(.self$fitted)},
                        coef = function(){return(.self$coefficients)},
                        summary = function(){
                          cat("Coefficients \n")
                          print(.self$coefficients)
                          cat("Standard Errors \n")
                          print("How do I get them?")
                          print(cbind(T_values=.self$tvalues, P_values=.self$pvalues))
                          cat(" Degrees of Freedom \n")
                          cat(.self$degrees_of_freedom)
                          },
                        
                        initialize = function(formula, data){
                          call_params <<- as.character(sys.calls()[1])  # returns pairlist of all the active calls
                          formula <<- formula
                          data <<- data
                          .self$fit()
                          .self$outliers <<- .self$quantile_outlier(.self$residuals)
                        },
                        
                        show = function(){ 
                          cat("Call: \n")
                          cat(.self$call_params)
                          cat('\n')
                          cat("Coefficients: \n")
                          print(t(coefficients))
                        }
                      )
          )

#custom_reg = Linreg(formula = Petal.Length ~ Species, data = iris)

#custom_reg$fit()
#print(custom_reg)

#library(ggplot2)

#d <- iris
#fit <- Linreg(formula = Petal.Length ~ Species, data = d)
