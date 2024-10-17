#' Summary Function for the gSTM objects
#' 
#' Function to report on the contents of gSTM objects
#' 
#' Summary prints a short statement about the model and then runs
#' \code{\link{labelTopics}}.
#' 
#' @aliases summary.gSTM print.gSTM
#' @param object An gSTM object.
#' @param \dots Additional arguments affecting the summary
#' @method summary gSTM
#' @export
summary.gSTM <- function(object,...) {
  toprint <- sprintf("A topic model with %i topics, %i documents and a %i word dictionary.\n", 
                     object$settings$dim$K, 
                     object$settings$dim$N, 
                     object$settings$dim$V)
  cat(toprint)
  labelTopics(object)
}

#' @method print gSTM
#' @export
print.gSTM <- function(x,...) {
  toprint <- sprintf("A topic model with %i topics, %i documents and a %i word dictionary.\n", 
                     x$settings$dim$K, 
                     x$settings$dim$N, 
                     x$settings$dim$V)
  cat(toprint)
}