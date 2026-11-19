

expect_no_error(
  object = nutritionlist(
    sousChef.nutrition:::Carnation_drymilk() |> 
      as('ingredient'),
    sousChef.nutrition:::KingArthur_baker_drymilk() |>
      as('ingredient'),
    sousChef.nutrition:::KingArthur_drywholemilk() |>
      as('ingredient'),
    sousChef.nutrition:::GreatValue_drymilk() |> 
      as('ingredient'),
    sousChef.nutrition:::MembersMark_drymilk() |> 
      as('ingredient'),
    sousChef.nutrition:::BobsRedMill_drymilk() |> 
      as('ingredient'),
    sousChef.nutrition:::Nido_drywholemilk() |> 
      as('ingredient'),
    sousChef.nutrition:::Horizon_drywholemilk() |> 
      as('ingredient')
  ) |> print()
)
  
expect_no_error(
  object = nutritionlist(
    sousChef.nutrition:::Meyenberg_goat_drymilk() |> 
      as('ingredient'),
    sousChef.nutrition:::Meyenberg_goat_drywholemilk() |> 
      as('ingredient')
  ) |> print()
)
