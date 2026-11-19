

#' @rdname ingredient
#' @param object see **Usage**
#' @export
setMethod(f = show, signature = 'ingredient', definition = \(object) print.ingredient(object))




#' @title [print.ingredient()]
#' 
#' @param x,... ..
#' 
#' @method print ingredient
#' @export print.ingredient
#' @export
print.ingredient <- \(x, ...) {
  
  cat('\n')
  
  if (is(x, 'recipe')) {
    x |> 
      labels() |> # [labels.recipe]
      #col_grey() |> 
      style_bold() |> 
      cat('\n\n')
  } # else do nothing
  
  prt_vol(x@misc)
  prt_vol_H2O(x@water)
  
  cat('\n')
  
  if (sum(waterEvap(x))) {
    sum(x@misc, x@water) |>
      sprintf(fmt = '%.0f grams') |>
      make_ansi_style('deeppink4')() |>
      bg_br_yellow() |>
      style_bold() |>
      sprintf(fmt = 'Raw Total %s\n\n') |>
      cat()
  }
  
  x |>
    as(Class = 'nutrition') |>
    print(prt_label = FALSE) # sousChef.nutrition:::print.nutrition
  
}

