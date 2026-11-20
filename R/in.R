
setGeneric(
  name = '%in%', 
  where = asNamespace('sousChef.ingredient'),
  useAsDefault = base::`%in%`
)

setGeneric(
  name = '%notin%', 
  where = asNamespace('sousChef.ingredient'),
  useAsDefault = base::`%notin%`
)



#' @title Subset Methods of \linkS4class{ingredient}
#' 
#' @param x,i,table see **Usage**
#' 
#' @name in_ingredient
#' @aliases %in%,character,ingredient-method
#' @export
setMethod(f = '%in%', signature = c(x = 'character', table = 'ingredient'), definition = \(x, table) {
  
  slotNames('ingredient') |>
    setdiff(y = c('water')) |> 
    lapply(FUN = slot, object = table) |>
    unname() |>
    unlist(use.names = TRUE) |>
    names() |>
    grepl(pattern = x, x = _) |>
    any()
  
})




#' @rdname in_ingredient
#' @aliases %notin%,character,ingredient-method
#' @export
setMethod(f = '%notin%', signature = c(x = 'character', table = 'ingredient'), definition = \(x, table) !(x %in% table))





#' @rdname in_ingredient
#' @aliases [,ingredient,character-method
#' @export
setMethod(f = '[', signature = c(x = 'ingredient', i = 'character'), definition = \(x, i) {
  
  z <- slotNames('ingredient') |>
    setdiff(y = c('water')) |> 
    lapply(FUN = slot, object = x) |>
    unname() |>
    unlist(use.names = TRUE)
  
  id <- z |>
    names() |>
    grepl(pattern = i, x = _)
  
  if (any(id)) return(z[id])
  return(numeric())
  
})
