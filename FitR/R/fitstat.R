#' Calculate Fit Statistics
#'
#' Finds fit statistics given a matrix of predicted values and a vector
#' of test data. Providing a vector of naive predictions allows for the inclusion
#' of MRAE in fit statistic calculations. Note that NA values are case-wise deleted.
#'
#' @param y A vector of test data values
#' @param P A matrix of predicted values
#' @param r A vector of naive estimates
#' @param HideFits A vector of fit statistics the user wants suppressed from the output. abbreviations as strings. 
#' Possible values include RMSE, MAD, RMSLE, MAPE, MEAPE, and MRAE (hidden by default since 
#' it is for use with the r argument only) 
#'
#' @return A matrix of fit statistic values, where rows are models and columns 
#' are fit statistics
#' @author Ryden W. Butler
#' @rdname fitstats
#' @export
fitstats <- function(y, P, r = NULL, HideFits = c('MRAE')) {
  # Check if inputs are of proper class
  if (!is.vector(y)) stop('y must be a vector')
  if (!is.matrix(P)) stop('P must be a matrix')
  if (!is.null(r) & !is.vector(r)) stop('r must either be NULL (by default) or a vector')
  # Combine test data, predictions, and naive estimates in matrix
  # If r is null, the cbind command will naturally ignore it
  HandleNAs <- cbind(P, y, r)
  # Check which rows have NA values
  NARows <- which(is.na(HandleNAs), arr.ind=T)[ , 1]
  
  # Remove NA rows if necessary
  if(length(NARows) != 0){
    y <- y[-NARows]
    P <- as.matrix(P[-NARows, ])
    r <- r[-NARows]
  }
  # Create matrix to store output, with corresponding model names
  # The leading column is removed in the function return
  output <- matrix(NA, nrow = ncol(P), ncol = 1)
  rownames(output) <- colnames(P)
  
  # Calculate absolute error
  e <- abs(P - y)
  # Calcualte absolute percent error
  a <- (e/abs(y))*100
  
  # Calculate fit statistics
  if (!('RMSE' %in% HideFits)) {
    RMSE <- sqrt(apply(e, 2, function(x) sum(x^2))/nrow(e))
    output <- cbind(output, RMSE)
  }
  if (!('MAD' %in% HideFits)) {
    MAD <- apply(e , 2, median)
    output <- cbind(output, MAD)
  }
  if (!('RMSLE' %in% HideFits)) {
    RMSLE <- sqrt(apply(P, 2, function(x) sum(log(x + 1) - log(y + 1))^2)/nrow(P))
    output <- cbind(output, RMSLE)
  }
  if (!('MAPE' %in% HideFits)) {
    MAPE <- apply(a, 2, sum)/nrow(a)
    if(sum(MAPE > 100) > 0){
      warning(paste('Large MAPE values caused by 0 values in test data.\n  Consider using other fit statistics.'))
    }
    output <- cbind(output, MAPE)
  }
  if (!('MEAPE' %in% HideFits)) {
    MEAPE <- apply(a, 2, median)
    output <- cbind(output, MEAPE)
  }
  # Check whether user has asked to suppress MRAE and if they have provided r
  if (!('MRAE' %in% HideFits) & !is.null(r)) {
    b <- abs(r - y)
    MRAE <- apply(e, 2, function(x) median(x/b))
    output <- cbind(output, MRAE)
  }
  # Prints error message if they want MRAE and r is null
  if (!('MRAE' %in% HideFits) & is.null(r)) {
    stop('Must pass a vector of naive estimates to argument r for MRAE.')
  }
  return(output[, -1])
}