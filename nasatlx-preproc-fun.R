nasatlx_preproc <- function(d) {
  d |> select("Индивидуальный_код",
              slider.response,
              head,
              task_type,
              task_level) |>
    filter_at(vars(head, task_type, task_level), all_vars(!is.na(.))) |>
    rename("id" = "Индивидуальный_код",
           "score" = slider.response) |>
    mutate(
      scale = recode(
        head,
        "Умственная нагрузка" = "ME",
        "Физическая нагрузка" = "PH",
        "Давление времени" = "TI",
        "Успешность выполнения" = "PE",
        "Усилия" = "EF",
        "Уровень фрустрации" = "FR"
      ),
      task = recode(
        task_type,
        "mental_rotation" = "MR",
        "sternberg" = "ST",
        "mental_span" = "MS"
      ),
      level = recode(
        task_level,
        "1" = "easy",
        "2" = "medium",
        "3" = "hard"
      )
    ) |>
    select(id, scale, score, task, level) -> NASATLX
  
  return(NASATLX)
}