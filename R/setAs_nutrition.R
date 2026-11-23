
#' @importFrom stats setNames
setAs(from = 'ingredient', to = 'nutrition', def = \(from) {
  
  grams <- from |> 
    as.double.ingredient()
  
  z <- grams |>
    names() |> 
    setNames(nm = _) |>
    #lapply(FUN = eval_, envir = asNamespace('sousChef.nutrition'))
    lapply(FUN = as, Class = 'nutrition')
  
  date <- z |> 
    lapply(FUN = slot, name = 'date') |>
    do.call(what = 'c', args = _) |>
    min(na.rm = FALSE)
  
  info <- z |>
    do.call(what = 'nutritionlist', args = _) |>
    as.matrix(
      incl_usd = TRUE, 
      incl_calorie = TRUE, 
      rel = TRUE
    ) # sousChef.nutrition:::as.matrix.nutritionlist
  
  z <- crossprod(grams, info)[1, , drop = TRUE]
  
  new(
    Class = 'nutrition',
    call = quote(`<UNDEFINED>`), # mark explicitly as an ill-formatted \linkS4class{nutrition}
    name = labels.ingredient(from) %||% character(),
    date = date,
    servingGram = sum(grams),
    usd = z['usd'] |> unname(),
    calorie = z['calorie'] |> unname(),
    carbohydrate = z['carbohydrate'] |> unname(),
    fiber = z['fiber'] |> unname(),
    sugar = z['sugar'] |> unname(),
    addedSugar = max(0, z['addedSugar']) |> unname(),
    alcohol = z['alcohol'] |> unname(),
    sodium = z['sodium'] |> unname(),
    chloride = z['chloride'] |> unname(),
    fat = z['fat'] |> unname(),
    cholesterol = z['cholesterol'] |> unname(),
    protein = z['protein'] |> unname(),
    water = z['water'] |> unname()
  )
  
})







#' @importFrom stats setNames
setAs(from = 'nutrition', to = 'ingredient', def = \(from) {
  
  x <- from; from <- NULL
  
  if (identical(x@call, quote(`<UNDEFINED>`))) {
    # created by one of setAs()
    return(invisible())
  }
  
  z <- if (length(x@pieceGram)) {
    1 |>
      setNames(nm = as.character(x@call)) |>
      ingredient(misc_pc = _)
  } else {
    x@servingGram |>
      setNames(nm = as.character(x@call)) |> 
      ingredient(misc = _)
  }
  
  if (!length(x@suggest)) {
    #return(z) # no bug now :)
    return(invisible())
  }
  
  z <- z + 
    x@suggest
  # setMethod(f = '+', signature = c(e1 = 'ingredient', e2 = 'list'))
  
  # \linkS4class{ingredient} does not have slot `@name`
  attr(z, which = 'name') <- x |>
    labels() |> # sousChef.nutrition:::labels.nutrition
    sprintf(fmt = '\U1f381 %s')
  
  return(z)
  
})



