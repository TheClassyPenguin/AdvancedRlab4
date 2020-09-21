data("iris")
# ' @export
Linreg <- setRefClass("Linreg",
                       fields = list(balance = "numeric",
                                     formula = "formula",
                                     data = "data.frame",
                                     pvalues = "vector",
                                     coefficients = "matrix",
                                     call_params = "character",
                                     residuals = "matrix",
                                     fitted = "matrix",
                                     resvar = "vector"
                                     
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
                          
                          s_hat = as.vector((t(e)%*%e)/df)                      #residual variance
                          #print(class(s_hat))
                          resvar <<- s_hat
                          
                          var_beta_hat = s_hat * solve(t(X) %*% X)              #variance of regression coefficients
                          #print(var_beta_hat)
                          t_val = as.vector(beta_hat) / sqrt(pmax(0,var_beta_hat))                   #t-values
                          p_val = 2*pt(-abs(t_val), df)
                          pvalues <<- p_val
                          
                        },
                        
                        initialize = function(formula, data){
                          call_params <<- as.character(sys.calls()[1])  # returns pairlist of all the active calls
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
print(custom_reg)

library(ggplot2)

d <- iris
fit <- Linreg(formula = Petal.Length ~ Species, data = d)

# ggplot(iris, aes(x = fit$fitted, y = fit$residuals)) + geom_point()

# Another way
quantile_outlier <- function(d){
q1 <- quantile(d$residuals, 0.25)
q3 <- quantile(d$residuals, 0.75) 
d$outlier <- ifelse(d$residuals > (q3 + 1.5*(q3-q1)), TRUE,
                 ifelse(d$residuals < (q1 - 1.5*(q3-q1)), 1, 0))
return(d$outlier)
}

plot_fun <- function() {
  p1 <- ggplot(iris, aes(fit$fitted, fit$residuals)) + 
    geom_point(data = d[quantile_outlier(d$outlier),], aes(outlier, 0), shape = 1) +
    stat_smooth(method = "loess") + 
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
    theme(axis.title.y = element_text(vjust = 0.5, size = 13, face = "bold")) +
    theme(axis.title.x = element_text(vjust = 0.5, size = 13, face = "bold")) +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
    theme_bw() 
   
  p3 <- ggplot(iris, aes(fit$fitted, sqrt(abs(fit$resvar)))) + 
    geom_point() +
    stat_smooth(method = "loess") + 
    xlab("Fitted Values") +
    ylab(expression(sqrt("|Standardized residuals|"))) +
    ggtitle("Scale-Location") + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
  
  return(list(rvf = p1, fvsd = p3))
}

plot_fun()

# Comments: So I tried to plot residuals vs. fitted and fitted vs residual variance (scale-location). 
# The variables should mainly be assigned on ggplot. Everything else is just aeshetics for R. 
# p1 works after some warnings but I still can't get the outliers in, as showed in the pdf (wrote a function that detects them and tried to implement it within the plot).
# p3 throws an error