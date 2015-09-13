rm(list=ls())
data(iris)
data(faithful)

linreg <- setRefClass("linreg",
      
      fields = list( formula = "formula",
        data = "data.frame",
        coefficients = "matrix",
        resid = "matrix",
        pred = "matrix",
        df = "integer",
        residvar = "numeric",
        varresidcoefficients = "matrix",
        tvalues = "matrix",
        pvalues = "matrix"),
      
      methods= list(
        initialize = function(formula, data){
          #.self$resid <- 5
          X <- model.matrix(formula,data)
          formulanames <- all.vars(formula)
          y <- data[,which(names(data) == formulanames[1])]
          if(class(y) == "factor"){
            
          }
          .self$coefficients <- solve(t(X) %*% X) %*% t(X) %*% y
          .self$pred <- X %*% .self$coefficients
          .self$resid <- y -  .self$pred
          .self$df <- dim(data)[1] - dim(X)[2]
          .self$residvar <- sum(.self$resid * .self$resid) / .self$df
          .self$varresidcoefficients <-.self$residvar * solve(t(X) %*% X)
          .self$tvalues <- .self$coefficients / sqrt(diag(.self$varresidcoefficients))
          .self$pvalues <- 1 - pt(abs(.self$tvalues),.self$df)
          
      
    },
    print = function(){
      
    },
    
    plot = function(){
      
    },
    coefficients = function(){
      
    },
    resid = function(){
      
    },
    
    pred = function(){
      
    },
    
    summary = function(){
      
    }
  )
)
  
