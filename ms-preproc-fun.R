ms_preproc <- function(d) {
  
  n_count <- function(df) {
    df |> select(matches("^noun")) |> as.matrix() -> s
    df |> select(matches("^resp")) |> as.matrix() -> r
    a <- vector(mode = "numeric", length = 16L)
    for (i in 1:16) {
      a[i] <- sum(r[i, ] %in% s[i, ])
    }
    return(a)
  }
  
  if ("mouse_MSe.time" %in% colnames(d)) {
    
    d |> select(
      "Индивидуальный_код",
      matches("^noun"),
      matches("resp\\d\\.text$"),
      "mouse_MSe.time"
    ) |>
      filter_at(vars(paste0("noun", 1:3)), all_vars(!is.na(.))) |>
      filter_at(vars(paste0("noun", 4:7)), all_vars(is.na(.))) |>
      mutate(task = "MS",
             level = "easy") |>
      rename(
        "resp1" = resp1.text,
        "resp2" = resp2.text,
        "resp3" = resp3.text,
        "id" = "Индивидуальный_код",
        "rt" = "mouse_MSe.time"
      ) |>
      select(-c(paste0("noun", 4:7))) -> MS_easy
    
    d |> select(
      "Индивидуальный_код",
      matches("^noun"),
      matches("MSm.text$"),
      "mouse_MSm.time"
    ) |>
      filter_at(vars(paste0("noun", 4:5)), all_vars(!is.na(.))) |>
      filter_at(vars(paste0("noun", 6:7)), all_vars(is.na(.))) |>
      mutate(task = "MS",
             level = "medium") |> 
      rename("id" = "Индивидуальный_код",
             "rt" = "mouse_MSm.time") |>
      rename_with(.fn = str_replace_all, 
                  pattern = "_MSm\\.text", 
                  replacement = "") |> 
      select(-noun6, -noun7) -> MS_medium
    
    
    d |> select(
      "Индивидуальный_код",
      matches("^noun"),
      matches("MSh.text$"),
      "mouse_MSh.time"
    ) |>
      filter_at(vars(paste0("noun", 1:7)), all_vars(!is.na(.))) |>
      mutate(task = "MS",
             level = "hard") |>
      rename("id" = "Индивидуальный_код",
             "rt" = "mouse_MSh.time") |> 
      rename_with(.fn = str_replace_all,
                  pattern = "_MSh\\.text",
                  replacement = "") -> MS_hard
    
  } else {
    
    d |> select("Индивидуальный_код",
                matches("^noun"),
                matches("resp\\d\\.text$")) |>
      filter_at(vars(paste0("noun", 1:3)), all_vars(!is.na(.))) |>
      filter_at(vars(paste0("noun", 4:7)), all_vars(is.na(.))) |>
      mutate(task = "MS",
             level = "easy",
             rt = NA) |>
      rename(
        "resp1" = resp1.text,
        "resp2" = resp2.text,
        "resp3" = resp3.text,
        "id" = "Индивидуальный_код"
      ) |>
      select(-c(paste0("noun", 4:7))) -> MS_easy
    
    d |> select("Индивидуальный_код",
                matches("^noun"),
                matches("MSm.text$")) |>
      filter_at(vars(paste0("noun", 4:5)), all_vars(!is.na(.))) |>
      filter_at(vars(paste0("noun", 6:7)), all_vars(is.na(.))) |>
      mutate(task = "MS",
             level = "medium",
             rt = NA) |>
      rename("id" = "Индивидуальный_код") |>
      rename_with(.fn = str_replace_all,
                  pattern = "_MSm\\.text",
                  replacement = "") |> 
      select(-noun6, -noun7) -> MS_medium
    
    
    d |> select("Индивидуальный_код",
                matches("^noun"),
                matches("MSh.text$")) |>
      filter_at(vars(paste0("noun", 1:7)), all_vars(!is.na(.))) |>
      mutate(task = "MS",
             level = "hard",
             rt = NA) |>
      rename("id" = "Индивидуальный_код") |> 
      rename_with(.fn = str_replace_all,
                  pattern = "_MSh\\.text", 
                  replacement = "") -> MS_hard
  }
  
  tibble(
    id = MS_easy$id[1],
    trials = 1:16,
    MS_easy_n = n_count(MS_easy),
    MS_easy_rt = MS_easy$rt,
    MS_medium_n = n_count(MS_medium),
    MS_medium_rt = MS_medium$rt,
    MS_hard_n = n_count(MS_hard),
    MS_hard_rt = MS_hard$rt
  ) |>
    pivot_longer(cols = -c("id", "trials"), values_to = "value") |>
    separate(name, c("task", "level", "name")) |>
    pivot_wider(values_from = value, names_from = name) |>
    mutate(acc = ifelse(level == "easy", n / 3,
                        ifelse(
                          level == "medium", n / 5,
                          ifelse(level == "hard", n / 7, NA)
                        ))) -> MS
  
  return(MS)
  
}
