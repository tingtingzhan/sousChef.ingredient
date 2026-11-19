

#' @title [.clean_ingredient()]
#' 
#' @param x \link[base]{list}
#' 
#' @export
.clean_ingredient <- \(x) {
  
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


