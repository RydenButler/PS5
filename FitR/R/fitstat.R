#' Calculate Fit Statistics
#'
#' Finds fit statistics given a matrix of predicted values and either a vector
#' of test data or a vector of naive predictions, or both
#'
#' @param y A vector of test data values
#' @param P A matrix of predicted values
#' @param r A vector of naive estimates
#' @param fits A vector of capitalized fit statistic abbreviations as strings. 
#' Possible values include RMSE, MAD, RMSLE, MAPE, MEAPE, and MRAE (for use with
#' r) 
#'
#' @return An matrix of fit statistic values, where rows are models and columns 
#' are fit statistics
#' @author Ryden W. Butler
#' @rdname fitstats
#' @export
setGeneric(name="fitstat",
           def=function(y=NULL, P, r=NULL, fits=c('RMSE', 'MAD', 'RMSLE', 'MAPE', 'MEAPE'), ...)
           {standardGeneric("fitstat")}
)

#' @export
setMethod(f="fitstat",
          definition=function(y=NULL, P, r=NULL, 
                              fits=c('RMSE', 'MAD', 'RMSLE', 'MAPE', 'MEAPE'), ...){
            return(fitstats(y, P, r, fits, ...))
          }
)

fitstats <- function(y = NULL, P, r = NULL, 
                     fits = c('RMSE', 'MAD', 'RMSLE', 'MAPE', 'MEAPE')) {
  if (is.null(y) & is.null(r)) return()
  if (!is.null(r)) {
    HandleNAs <- na.omit(cbind(P, y, r))
    y <- HandleNAs[ , (ncol(HandleNAs)-1)]
    # because y takes on values of 0, this results in infinity
    # values in the average percent error
    y <- y + .1
    r <- HandleNAs[ , ncol(HandleNAs)]
    # because r takes on values of 0, this results in infinity
    # values in the average percent error
    r <- r + .1
    P <- HandleNAs[ ,-c(ncol(HandleNAs), ncol(HandleNAs)-1)]
  }
  HandleNAs <- na.omit(cbind(P, y))
  y <- HandleNAs[ , ncol(HandleNAs)]
  # because y takes on values of 0, this results in infinity
  # values in the average percent error
  y <- y + .1
  P <- HandleNAs[ ,-c(ncol(HandleNAs), ncol(HandleNAs)-1)]
 
  # calculate individual errors
  e <- abs(P - y)
  # produces some huge values b/c 0s in y
  a <- (e/abs(y))*100
  if (!is.null(r)) b <- abs(r - y)
  
  output <- matrix(NA, nrow = ncol(P), ncol = 1)
  rownames(output) <- colnames(P)
  
  # calculate fit statistics
  if ('RMSE' %in% fits) {
    RMSE <- sqrt(apply(e, 2, function(x) sum(x^2))/nrow(e))
    output <- cbind(output, RMSE)
  }
  if ('MAD' %in% fits) {
    MAD <- apply(e , 2, median)
    output <- cbind(output, MAD)
  }
  if ('RMSLE' %in% fits) {
    RMSLE <- sqrt(apply(P, 2, function(x) sum(log(x + 1) - log(y + 1))^2)/nrow(P))
    output <- cbind(output, RMSLE)
  }
  if ('MAPE' %in% fits) {
    MAPE <- apply(a, 2, sum)/nrow(a)
    output <- cbind(output, MAPE)
  }
  if ('MEAPE' %in% fits) {
    MEAPE <- apply(a, 2, median)
    output <- cbind(output, MEAPE)
  }
  if ('MRAE' %in% fits & !is.null(r)) {
    MRAE <- apply(e, 2, function(x) median(e/x))
    output <- cbind(output, MRAE)
  }
  if ('MRAE' %in% fits & is.null(r)) {
    print('Must pass a vector of naive estimates to r for MRAE.')
  }
  
  return(output[, -1])
}

