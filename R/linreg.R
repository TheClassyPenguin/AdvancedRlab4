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
#' RC print function redifinition workaround.
#' 
#' Test cases call object$print() instead of print(object) so print
#' function must be redefined in inner scope which makes printing of lists
#' and dataframes impossible.
#' 
#' @field Object to print.
#' 
#' @return Nothing.
RC_print_workaround = function(x) print(x)

#' Linreg Class
#' 
#' This class performs linear regression and provides different methods to display the data. It also prints the plots for Residual vs. Fitted Values and Scale-Location
#' 
#' The algorithm used for this function can be found on 
#' \url{https://en.wikipedia.org/wiki/Linear_regression}
#' 
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
                       fields = list(formula = "formula",
                                     data = "data.frame",
                                     data_name = "character",
                                     pvalues = "vector",
                                     tvalues = "vector",
                                     coefficients = "matrix",
                                     coefficients_variance = "matrix",
                                     call_params = "character",
                                     residuals = "vector",
                                     fitted = "vector",
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
                          .self$coefficients <- beta_hat
                          
                          #fitted values
                          y_hat = X %*% beta_hat                                
                          .self$fitted <- as.vector(y_hat)
                          
                          #Residuals
                          e = y - y_hat                                         
                          .self$residuals <- as.vector(e)
                          
                          #Degrees of freedom (observations - parameters)
                          df = nrow(X) - ncol(X)
                          .self$degrees_of_freedom <- df
                          
                          #residual variance
                          s_hat = (t(e)%*%e)/df                      
                          .self$resvar <- s_hat
                          
                          #variance of regression coefficients
                          var_beta_hat = as.numeric(s_hat) * solve(t(X) %*% X) 
                          .self$coefficients_variance <- var_beta_hat
                          
                          #T-Values
                          t_val = as.vector(beta_hat) / sqrt(pmax(0,var_beta_hat))
                          .self$tvalues <- t_val
                          
                          #P-Values
                          p_val = 2*pt(-abs(t_val), df)
                          .self$pvalues <- p_val
                          
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
                          
                          p2 <- ggplot(iris, aes(.self$fitted, sqrt(abs(.self$residuals/sqrt(abs(.self$resvar)))))) + 
                            geom_point()+
                            #geom_point(data = .self$data[.self$outliers,], aes(.self$outliers, 0), shape = 1)+
                            stat_summary(fun=mean, fun.args = list(trim=0.25), colour="red", geom="line",group=1)+
                            labs(title = "Scale~Location", x = "Fitted values", y = expression(sqrt(Standardized~residuals))) +
                            theme(axis.title.y = element_text(vjust = 0.5, size = 13, face = "bold")) +
                            theme(axis.title.x = element_text(vjust = 0.5, size = 13, face = "bold")) +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
                            theme_bw() 
                          RC_print_workaround(p1)
                          RC_print_workaround(p2)
                          },
                        # resid function, returns residuals 
                        resid = function(){return(.self$residuals)},
                        
                        # pred function, returns fitted values
                        pred = function(data=NULL){
                          if(!is.null(data)){
                            .self$initialize(.self$formula, data)
                          }
                          return(.self$fitted)
                          },
                        
                        # coef function, returns coefficients
                        coef = function(){return(.self$coefficients)},
                        
                        summary = function(){
                          
                          signif_display= function(x){
                            switch(x > 0.1, return("*"))
                            switch(x > 0.01, return("**"))
                            switch(x > 0.001, return("***"))
                            return(" ")
                          }
                          
                          printout = data.frame(matrix(nrow = 0, ncol= 5))
                          names(printout) = c(" ","Estimate", "Std. error", "t value", " ")
                          for(i in 1:length(rownames(.self$coefficients))){
                            coeff_name = rownames(.self$coefficients)[i]
                            estimate = .self$coefficients[i]
                            stderror = sqrt(diag(.self$coefficients_variance)[i])
                            tvalue = diag(matrix(.self$tvalues, nrow= sqrt(length(.self$tvalues)), ncol= sqrt(length(.self$tvalues))))[i]
                            signif = signif_display(tvalue)
                            printout[i,] = c(coeff_name, estimate, stderror, tvalue, signif)
                          }
                          
                          
                          residprint = data.frame(matrix(nrow = 0, ncol = 5))
                          names(residprint) = c("Min", "1Q", "Median", "3Q", "Max")
                          quant = quantile(.self$residuals)
                          residprint[1,] = c(min(.self$residuals),
                                             quant[2],
                                             quant[3],
                                             quant[4],
                                             max(.self$residuals)
                                             )
                          
                          cat("Call: \n")
                          cat(paste("linreg(formula = "), format(.self$formula), ", data = ", .self$data_name, ")\n", sep = "")
                          cat("\n")
                          cat("Residuals:\n")
                          RC_print_workaround(residprint)
                          cat("\n")
                          cat("Coefficients:\n")
                          RC_print_workaround(printout)
                          cat("--- \n")
                          cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 \n\n")
                          cat(paste("Residual standard error:",
                                    sqrt(.self$resvar),
                                    "on",
                                    .self$degrees_of_freedom,
                                    "degrees of freedom"))
                          },
                        
                        initialize = function(formula, data){
                          # Creating object. Arguments are the formula and corresponding data frame.
                          
                          stopifnot(inherits(formula,"formula"))
                          stopifnot(inherits(data,"data.frame"))
                          .self$call_params <- as.character(sys.calls()[1])  # returns pairlist of all the active calls
                          .self$formula <- formula
                          .self$data <- data
                          .self$data_name <- deparse(substitute(data))
                          .self$fit()
                          .self$outliers <- .self$quantile_outlier(.self$residuals)
                        },
                        
                        print = function(){ 
                          cat("Call: \n")
                          cat(paste("linreg(formula = "), format(.self$formula), ", data = ", .self$data_name, ")\n", sep = "")
                          cat("Coefficients: \n")
                          RC_print_workaround(t(.self$coefficients))
                        }
                      )
          )

custom_reg = linreg(formula = Petal.Length ~ Species, data = iris)

#custom_reg$fit()
#print(custom_reg)

#library(ggplot2)
#d <- iris
#fit <- Linreg(formula = Petal.Length ~ Species, data = d)
#linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#linreg_mod$print()
#linreg_mod$summary()
#linreg_mod$plot()
#custom_reg$pred() 
#linreg_mod$pred()
#linreg_mod$coeff()