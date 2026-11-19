

#' @method as.double ingredient
#' @export
as.double.ingredient <- \(x, ...) {
  
  x <- x |>
    mergeH2O()
  
  attributes(x)[slotNames(x = 'ingredient')] |>
    unname() |>
    unlist(use.names = TRUE) # no need to remove len-0 element
  
}
