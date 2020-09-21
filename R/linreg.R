data("iris")
# ' @export
Linreg <- setRefClass("Linreg",
                       fields = list(balance = "numeric",
                                     formula = "formula",
                                     data = "data.frame",
                                     pvalues = "vector",
                                     coefficients = "matrix",
                                     call_params = "character"
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
                          
                          y_hat = X %*% beta_hat                                ##fitted values
                          
                          e = y - y_hat                                         ##residuals

                          df = nrow(X) - ncol(X)
                          
                          s_hat = (t(e)%*%e)/df                                 ##residual variance
                          var_beta_hat = as.vector(s_hat) * solve(t(X) %*% X)   #variance of regression coefficients
                          #print(var_beta_hat)
                          t_val = as.vector(beta_hat) / sqrt(pmax(0,var_beta_hat))                   ##t-values
                          p_val = 2*pt(-abs(t_val), df)
                          pvalues <<- p_val
                        },
                        
                        initialize = function(formula, data){
                          call_params <<- as.character(sys.calls()[1])
                          formula <<- formula
                          data <<- data
                          .self$fit()
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
