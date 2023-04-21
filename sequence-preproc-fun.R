sequence_preproc <- function(d) {
  
  d |> select(
    E_rotation,
    M_rotation,
    H_rotation,
    E_Sternberg,
    M_Sternberg,
    H_Sternberg,
    E_span,
    M_span,
    H_span
  ) |>
    drop_na() |>
    sapply(function(x) which(x == 1)) -> v 
    
  tibble(name = names(v),
           order = v,
           id = d[["Индивидуальный_код"]][1]) |>
    arrange(order) |>
    separate(name, c("level", "task"), "_") |>
    mutate(
      task = recode(
        task,
        "rotation" = "MR",
        "Sternberg" = "ST",
        "span" = "MS"
      ),
      level = recode(
        level,
        "E" = "easy",
        "M" = "medium",
        "H" = "hard"
      )
    ) -> SEQUENCE
  
  return(SEQUENCE)
  
}
  