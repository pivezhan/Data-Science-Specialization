#' building a model with top ten features
#' develops aprediction algorithm based on the top 10 features
#' in 'x' that are most predictive of 'y'
#'
#' @param x a n 8p matrix of n observation and p prediction
#' @param y a vector of length n representing the response
#' @return a vectorof coefficient from the final model with top 10 features
#' @author Roger Peng
#' @details 
#' this function runs a univariate regression of each predictor
#' in x and calculates a p-value indicating the significance of 
#' the assosiation. The final set of 10 predictors is taken from 
#' the 10 smallest p-values.
#' @seealso \code{lm}
#' @export 
#' @importFrom stat lm


topten <- function(x,y){
        p<-ncol(x)
        if(p<10)
                stop("there are less than 10  predictors")
        pvalues<-numeric(p)
        for (i in seq_len(p)){
                fit~lm(y~x[,i])
                summ<-summary(fit)
                pvalues[i]<-summ$coefficients[2,4]
                }
        ord<-order(pvalues)
        ord <- ord[1:10]
        x10<x[,ord]
        fit<-lm(y~x10)
        coef(fit)
}

#' predictionwith top 10 features 
#' 
#' This function takes a set of coefficients produced by the \code{topten}
#' input 'X' matrix
#' 
#' @param x a n *10 matrix of n observation
#' @param y a vector coefficient obtained from the \code{topten} function
#' @return a numeric vector contained predicted value

predict10<-function(X,b){
        X<-cbind(1,X)
        drop(X%*%b)
        }
