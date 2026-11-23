



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






#' @importFrom equiv4 sum_by_name
mergeVol_H2O <- \(x) {
  
  x$water <- x[['water']] |>
    addname1(nm = 'c22')
  x$water_tsp <- x[['water_tsp']] |>
    addname1(nm = 'c22')
  x$water_Tbsp <- x[['water_Tbsp']] |>
    addname1(nm = 'c22')
  x$water_floz <- x[['water_floz']] |>
    addname1(nm = 'c22')
  x$water_cup <- x[['water_cup']] |>
    addname1(nm = 'c22')
  
  x_tsp <- sum_by_name(
    x[['water_tsp']],
    3 * x[['water_Tbsp']],
    6 * x[['water_floz']],
    48 * x[['water_cup']]
  )
  
  z <- sum_by_name(
    x[['water']],
    4.92892 * x_tsp # tsp -> ml
  )
  
  if (length(z)) x$water <- z
  
  x$water_tsp <- x$water_Tbsp <- x$water_floz <- x$water_cup <- NULL
  
  return(x)
  
}



#' @importFrom equiv4 sum_by_name
mergeVol <- \(x, ...) {
  
  x_tsp <- sum_by_name(
    x[['misc_tsp']],
    3 * x[['misc_Tbsp']],
    6 * x[['misc_floz']],
    48 * x[['misc_cup']],
    3 * 16 * x[['misc_brick']]
  )
  
  z <- sum_by_name(
    x[['misc']], # weight in gram 
    gram_per_tsp(names(x_tsp)) * x_tsp
  )
  
  if (length(z)) x$misc <- z
  
  x$misc_tsp <- x$misc_Tbsp <- x$misc_floz <- x$misc_cup <- x$misc_brick <- NULL
  return(x)
  
}



#' @importFrom equiv4 sum_by_name
mergePc <- \(x, ...) {  
  
  v <- x[['misc_pc']]
  if (!length(v)) return(x)
  
  pieceGram <- names(v) |>
    lapply(FUN = as, Class = 'nutrition') |>
    vapply(FUN = slot, name = 'pieceGram', FUN.VALUE = NA_real_, USE.NAMES = TRUE)
  
  z <- sum_by_name(
    x[['misc']],
    v * pieceGram
  )
  
  if (length(z)) x$misc <- z
  
  x$misc_pc <- NULL
  
  return(x)
}



# combine water at different temperature
#' @importFrom stats setNames
mergeH2O <- \(x) {
  # `x` is \linkS4class{ingredient}
  if (length(x@water)) {
    x@water <- x@water |>
      sum() |>
      setNames(nm = 'Wegmans_water')
  } # else do nothing
  return(x)
}



prt_vol_H2O <- \(x) {
  
  if (!length(x)) return(invisible())
  
  z <- x |>
    fmt_vol_H2O()
  
  col_orchid4 <- make_ansi_style('orchid4')
  
  label <- names(x)
  label[label == 'ice'] <- '\u51b0\u5757' |>
    col_orchid4() |>
    sprintf(fmt = '%s Ice\U1f9ca Cubes')
  label[label == 'c0'] <- '0\u00b0C\u51b0\u6c34' |>
    col_orchid4() |>
    sprintf(fmt = '%s Iced Water \U0001f32132\u00b0F')
  label[label == 'c22'] <- '22\u00b0C\u5e38\u6e29\u6c34' |>
    col_orchid4() |>
    sprintf(fmt = '%s Water \U0001f32172\u00b0F')
  label[label == 'c40'] <- '40\u00b0C\u6e29\u6c34' |>
    col_orchid4() |>
    sprintf(fmt = '%s Warm Water \U0001f321104\u00b0F')
  label[label == 'c70'] <- '70\u00b0C\u70ed\u6c34' |>
    col_orchid4() |>
    sprintf(fmt = '%s Hot Water \U0001f321160\u00b0F')
  label[label == 'c80'] <- '80\u00b0C\u70ed\u6c34' |>
    col_orchid4() |>
    sprintf(fmt = '%s Hot Water \U0001f321175\u00b0F')
  label[label == 'c85'] <- '85\u00b0C\u70ed\u6c34' |>
    col_orchid4() |>
    sprintf(fmt = '%s Hot Water \U0001f321185\u00b0F')
  label[label == 'c90'] <- '90\u00b0C\u70ed\u6c34' |>
    col_orchid4() |>
    sprintf(fmt = '%s Hot Water \U0001f321195\u00b0F')
  label[label == 'c95'] <- '95\u00b0C\u70ed\u6c34' |>
    col_orchid4() |>
    sprintf(fmt = '%s Hot Water \U0001f321203\u00b0F')
  label[label == 'c100'] <- '100\u00b0C\u5f00\u6c34' |>
    col_orchid4() |>
    sprintf(fmt = '%s Boiling Water \U0001f321212\u00b0F')
  
  sprintf(fmt = '%s %.0f grams %s\n', label, x, z) |>
    format_inline() |>
    cat()
  
}





prt_vol <- \(x) {
  
  x <- x |>
    zero.omit()
  
  nx <- length(x)
  if (!nx) return(invisible())
  
  nm <- x |> 
    names()
  
  ingredient <- nm |>
    lapply(FUN = as, Class = 'nutrition')
  
  id <- ingredient |>
    vapply(FUN = \(i) {
      if (!is(i, 'nutrition')) return(FALSE) # could be homemade \linkS4class{recipe}
      length(i@pieceGram) > 0L
    }, FUN.VALUE = NA)
  
  z <- character(length = nx)
  z[id] <- gram2pc(x[id])
  z[!id] <- gram2vol(x[!id])
  
  label <- ingredient |>
    vapply(FUN = labels, FUN.VALUE = '') 
  # [labels.nutrition]; or 
  # [labels.recipe] (when dealing with homemade ingredients)
  
  sprintf(fmt = '%s %.0f grams %s\n', label, x, z) |>
    format_inline() |>
    cat()
  
}


fmt_vol_H2O <- \(x) {
  nx <- length(x)
  if (!nx) return(character())
  (x / 4.92892) |> # ml -> tsp
    cmod(e1 = _, e2 = consec::teaspoon, n = 3L, tol = 1e-6) |>
    col_br_blue() |> 
    style_bold()
}



#' @importFrom consec cmod
gram2vol <- \(x, nm = names(x)) {
  
  nx <- length(x)
  if (!nx) return(character())
  
  x1 <- x / gram_per_tsp(nm)
  
  id <- grepl(pattern = '_butter$|_creamCheese$', x = nm)
  
  z <- character(length = nx)
  z[id] <- x1[id] |>
    cmod(e1 = _, e2 = consec::teaspoon2, n = 3L, tol = 1e-6)
  z[!id] <- x1[!id] |>
    cmod(e1 = _, e2 = consec::teaspoon, n = 3L, tol = 1e-6) 
  
  z |>
    col_br_blue() |> 
    style_bold()
}




gram2pc <- \(x) {
  
  if (!length(x)) return(invisible())
  
  x_nutri <- names(x) |>
    lapply(FUN = as, Class = 'nutrition')
  id_pc <- x_nutri |>
    vapply(FUN = is, class2 = 'nutrition', FUN.VALUE = NA)
  v_pc <- x_nutri[id_pc]
  
  pieceGram <- v_pc |> 
    vapply(FUN = slot, name = 'pieceGram', FUN.VALUE = NA_real_)
  
  fmt <- v_pc |>
    vapply(FUN = slot, name = 'piece_fmt', FUN.VALUE = '')
  
  z <- character(length = length(x))
  z[id_pc] <- (x[id_pc] / pieceGram) |>
    sprintf(fmt = fmt) |> # `fmt` can be vectorized!!
    col_br_magenta() |> 
    style_bold()
  return(z)
  
}




gram_per_tsp <- \(x) {
  
  if (!length(x)) return(double())
  
  if (!is.character(x) || anyNA(x) || !all(nzchar(x))) stop('input degenerated')
  
  x |> 
    vapply(FUN = \(i) {
      v <- i |> 
        as(Class = 'nutrition')
      if (!is(v, 'nutrition')) {
        stop(sQuote(i), ' cannot be converted to `nutrition`')
      }
      if (!length(v@servingTsp)) return(NA_real_)
      v@servingGram / v@servingTsp
    }, FUN.VALUE = NA_real_, USE.NAMES = TRUE)
  
}





paste_names <- \(x, ...) {
  if (!length(x)) return(x)
  names(x) <- paste(names(x), ...)
  return(x)
}

gsub_names <- \(x, ...) {
  names(x) <- gsub(x = names(x), ...)
  return(x)
}




# add name to len-1 vector
addname1 <- \(x, nm = stop('no default!'), ...) {
  
  nx <- length(x)
  if (!nx) return(x) # exception
  
  if (nx == 1L) {
    xnm <- names(x)
    if (!length(xnm) || is.na(xnm) || !nzchar(xnm)) {
      names(x) <- nm
    } # else do nothing
  }
  
  xnm <- names(x) # re-do!
  if (!length(xnm) || anyNA(xnm) || !all(nzchar(xnm))) {
    stop('ill name')
  }
  
  return(x)
  
}









