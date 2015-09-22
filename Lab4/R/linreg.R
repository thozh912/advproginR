#' A reference class generator for linear regression
#' 
#' This function creates a class \code{linreg} object which does linear
#' regression upon a specified formula and data.frame. Linear regression is
#' performed by ordinary least squares calculations. Plots are handled by 
#' packages \code{ggplot2}, \code{grid} and \code{gridExtra}.
#' 
#' This RC class generator can create a \code{linreg} object from any formula as
#' long as the response are not factors. The ordinary least squares calculations
#' are all done using matrices, and the residual plots fit using \code{glm.fit}
#' by default, although it switches to \code{loess} for groups <1000 (see
#' \link[ggplot2]{stat_smooth}). The printouts are simple \code{paste} and
#' \code{writeLines} calls for the most part, and are not objects. Every method
#' returns something relevant.
#' 
#' @field formula A formula taken as input, upon which linear regression is
#'   performed.
#'   
#' @field data A data.frame which has names to which the names in the formula
#'   refer.
#'   
#' @field dataname A character string containing the name of the data.
#'   
#' @field coefficients A single column matrix consisting of the calculated
#'   regression coefficients of the linear model.
#'   
#' @field residual A single column matrix containing the residuals of the
#'   calculated linear model.
#'   
#' @field predicted A single column matrix containing the linearly predicted
#'   response.
#'   
#' @field df An integer containing the degrees of freedom of the linear
#'   regression undertaken.
#'   
#' @field residualvar A numeric scalar indicating the residual variance of the
#'   linear model.
#'   
#' @field varregcoefficients A matrix containing the covariance matrix of the
#'   regression coefficients.
#'   
#' @field tvalues A single column matrix consisting of the t-statistics for the
#'   calculated regression coefficients.
#'   
#' @field pvalues A single column matrix consisting of the two-sided p-values
#'   associated with the t-statistics for the calculated regression
#'   coefficients. Is zero for large enough t-statistic.
#'   
#' @references \url{http://en.wikipedia.org/wiki/Linear_regression}
#'   
#' @examples
#' data(faithful)
#' linearobject <- linreg(formula = eruptions ~ waiting, data = faithful)
#' linearobject$summary()
#' @export





linreg <- setRefClass("linreg",
    
      fields = list( formula = "formula",
        data = "data.frame",
        dataname = "character",
        coefficients = "matrix",
        residual = "matrix",
        predicted = "matrix",
        df = "integer",
        residualvar = "numeric",
        varregcoefficients = "matrix",
        tvalues = "matrix",
        pvalues = "matrix"),
      
      methods = list(
        initialize = function(formula, data){
          "From inputs formula and data, generates all the other fields upon object initialization."
          .self$formula <- formula
          .self$data <- data
          .self$dataname <- deparse(substitute(data))
          X <- model.matrix(formula,data)
          formulanames <- all.vars(formula)
          y <- data[,which(names(data) == formulanames[1])]
          .self$coefficients <- solve(t(X) %*% X) %*% t(X) %*% y
          .self$predicted <- X %*% .self$coefficients
          .self$residual <- y -  .self$predicted
          .self$df <- dim(data)[1] - dim(X)[2]
          .self$residualvar <- sum(.self$residual * .self$residual) / .self$df
          .self$varregcoefficients <-.self$residualvar * solve(t(X) %*% X)
          .self$tvalues <- .self$coefficients / sqrt(diag(.self$varregcoefficients))
          .self$pvalues <- 2 * (1 - pt(abs(.self$tvalues),.self$df))    
    },
    print = function(){
      "Gives a printout of the call as well as the calculated regression coefficients."
      blender <- as.character(.self$formula)
      formulastring <- paste(blender[2],blender[1],blender[3])
      readout <- as.vector(.self$coefficients)
      coefnames <- rownames(.self$coefficients)
      names(readout) <- coefnames
      writeLines(c("Call:"))
      calline <- paste("linreg","(","formula = ",formulastring,", data= ",.self$dataname,")",sep="")
      writeLines(c(calline,"","Coefficients:"))
      return( readout )
    },
    
    plot = function(){
      "plots two plots of residuals vs. fitted values, in a grid."
      blender <- as.character(.self$formula)
      formulastring <- paste(blender[2],blender[1],blender[3])
      resifitted <- data.frame(Residuals= .self$residual, fitted = .self$predicted, sqrtstdresid = sqrt(abs(.self$residual / sqrt(.self$residualvar))))
      plot1 <- ggplot(resifitted,aes(fitted,Residuals)) + geom_smooth( se=FALSE, color = "red") + geom_point(shape=1) + xlab(paste("Fitted values: linreg","(","formula = ",formulastring,")",sep="")) + ggtitle("Residuals vs Fitted") + 
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      plot2 <- ggplot(resifitted,aes(fitted, sqrtstdresid)) + geom_smooth(se=FALSE, color= "red") + geom_point(shape=1) + xlab(paste("Fitted values: linreg","(","formula = ",formulastring,")",sep=""))+ ylab("sqrt(abs(Standardized residuals))") + ggtitle("Scale-Location") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      return(grid.arrange(plot1, plot2, nrow = 2))
    },
    coef = function(){
      "Returns a named vector of the regression coefficients in the linear model." 
      readout <- as.vector(.self$coefficients)
      coefnames <- rownames(.self$coefficients)
      names(readout) <- coefnames
      return(readout)
    },
    resid = function(){
      "Returns a vector of the residual values of the linear model."
      return(.self$residual[,1])
    },
    
    pred = function(){
      "Returns the predicted values of the linear model."
      return(.self$predicted[,1])
    },
    summary = function(){
      "Prints out a summary of the linear regression performed."
      blender <- as.character(.self$formula)
      formulastring <- paste(blender[2],blender[1],blender[3])
      readout <- as.vector(.self$coefficients)
      coefnames <- rownames(.self$coefficients)
      writeLines(c("Call:"))
      line1 <- paste("linreg","(","formula = ",formulastring,", data= ",.self$dataname,")",sep="")
      line2 <- paste("Residual standard error: ",as.character(signif(sqrt(.self$residualvar),5))," on ",as.character(.self$df)," degrees of freedom",sep="")
      coefmat <- data.frame(.self$coefficients, sqrt(diag(.self$varregcoefficients)), .self$tvalues , .self$pvalues )
      names(coefmat) <- c("Estimate", "Std. Error", "t Value", "Pr(>|t|)")
      writeLines(c(line1,line2,"Coefficients:"))
      return(coefmat)
    }
  )
)

