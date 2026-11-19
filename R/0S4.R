
#' @import cli
#' @import methods
#' @import sousChef.utils
#' @import sousChef.nutrition



#' @title Raw Ingredients
#' 
#' @description 
#' \linkS4class{ingredient} allows negative ingredients.
#' 
#' @slot misc \link[base]{numeric} \link[base]{vector}, weight (in grams) of miscellaneous ingredients
#' 
#' @slot water \link[base]{numeric} \link[base]{vector}, weight (in grams) of water
#' 
#' @name ingredient
#' @aliases ingredient-class
#' @export
setClass(
  Class = 'ingredient', 
  # dQuote('raw') has a sealed class definition and cannot be redefined
  slots = c(
    misc = 'numeric',
    water = 'numeric'
  )
)






ingredient <- \(...) {
  list(...) |>
    .cleanSlots_ingredient() |>
    c(list(Class = 'ingredient')) |> 
    do.call(what = 'new', args = _)
}





.cleanSlots_ingredient <- \(x) {
  
  x <- x |>
    mergePc() |> 
    mergeVol() |>
    mergeVol_H2O()
    
  for (i in slotNames(x = 'ingredient')) {
    if (length(x[[i]])) {
      x[[i]] <- x[[i]] |>
        zero.omit() |>
        sort.default(decreasing = TRUE)
    }
  }
  
  return(x)
  
}





setValidity(Class = 'ingredient', method = \(object) {
  
  z <- object |>
    as.double.ingredient()
  
  if (anyDuplicated.default(names(z))) {
    stop('do not allow!!!')
  }
  
})
  
  
  

