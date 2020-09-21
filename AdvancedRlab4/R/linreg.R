data("iris")

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

custom_reg = Linreg(formula = Petal.Length ~ Species, data = iris)

#custom_reg$fit()
print(custom_reg)

library(ggplot2)

data("iris")

d1 <- data.frame(x$y_hat, x$e)
names(d1) <- c("Fitted Values", "Residuals")
d2 <- data.frame(x$y_hat, sqrt(abs(x$e/sqrt(x$s_hat))))
names(d2) <- c("Fitted Values", "Standardized Residuals")

iqr <- IQR(d1$Residuals)
d1$outliers <- ifelse((d1$Residuals > as.numeric(quantile(d1$Residuals)[4] + iqr * 1.5)),
                      (d1$Residuals < as.numeric(quantile(d1$Residuals)[2] - iqr * 1.5)), 1, 0)
d2$outliers <- d1$outliers

plot_res <- function(data, title) {
  p <- ggplot(data = data[data$outliers == 0, ], aes_string(names(data)[1], names(data)[2]), shape = 1, size =3) +
    geom_point() +
    stat_summary(
      data = data[data$outliers == 0, ],
      fun = mean,
      aes(group = 1),
      geom = "line",
      color = "red"
    )
  geom_smooth(data = data[data$outliers == 0, ], method = "loess") +
    geom_point(data = data[data$outliers == 1, ], shape = 1, size = 3) +
    geom_text(data = data[data$outliers == 1, ], aes(label = rownames(data[data$outliers == 1,])), hjust = 0.5) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
    ggtitle(title) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  return(p)
}

plot_fun(data = d1, title = "Residuals vs Fitted")
plot_fun(data = d2, title = "Scale-Location")
