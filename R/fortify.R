#' @export
#' @importFrom ggplot2 fortify
fortify.DataFrame <- function(model, data, ...) {
  as.data.frame(model)
}
