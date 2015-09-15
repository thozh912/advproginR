rm(list=ls())
data(iris)
data(faithful)
#install.packages("ggplot2")
library(ggplot2)
library(grid)
#install.packages("gridExtra")
library(gridExtra)



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
      blender <- as.character(.self$formula)
      formulastring <- paste(blender[2],blender[1],blender[3])
      resifitted <- data.frame(Residuals= .self$residual, fitted = .self$predicted, sqrtstdresid = sqrt(abs(.self$residual / sqrt(.self$residualvar))))
     
      plot1 <- ggplot(resifitted,aes(fitted,Residuals)) + geom_smooth( se=FALSE, color = "red") + geom_point() + xlab(paste("Fitted values: linreg","(","formula = ",formulastring,")",sep="")) + ggtitle("Residuals vs Fitted")
      
      plot2 <- ggplot(resifitted,aes(fitted, sqrtstdresid)) + geom_smooth(se=FALSE, color= "red") + geom_point() + xlab(paste("Fitted values: linreg","(","formula = ",formulastring,")",sep=""))+ ylab("sqrt(abs(Standardized residuals))") + ggtitle("Scale-Location")
      return(grid.arrange(plot1, plot2, nrow = 2))
    },
    coef = function(){
      readout <- as.vector(.self$coefficients)
      coefnames <- rownames(.self$coefficients)
      names(readout) <- coefnames
      return(readout)
    },
    resid = function(){
      return(.self$residual[,1])
    },
    
    pred = function(){
      return(.self$predicted[,1])
    },
    summary = function(){
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
