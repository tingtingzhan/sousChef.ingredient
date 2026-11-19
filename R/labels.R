

#' @method labels ingredient
#' @export
labels.ingredient <- \(object, ...) {
  object |>
    attr(which = 'name', exact = TRUE)
  # see 
  # setAs(from = 'ingredient', to = 'nutrition')
  # setAs(from = 'nutrition', to = 'ingredient')
}

