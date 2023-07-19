library(tidyverse)
theme_set(theme_bw())

rm(list=ls())

mr_preproc <- function(d) {
  
  require(tidyverse)
  
  d |> select(
    # select columns we need
    correctAns,
    base_pic,
    rotated_pic,
    resp_MR_easy.keys,
    resp_MR_easy.corr,
    resp_MR_easy.rt
  ) |>
    drop_na() |> # remove technical NAs (recording artefacts, not missing data)
    mutate(task = "MR",
           # add task name (mental rotation)
           level = "easy",
           # add difficulty level
           trial = 1:16) |> # number trials
    rename(
      # rename columns for handy usage
      "key" = resp_MR_easy.keys,
      "is_correct" = resp_MR_easy.corr,
      "rt" = resp_MR_easy.rt
    ) -> MR_easy # ready to use
  
  
  d |> select(
    # select columns we need
    correctAns,
    base_pic,
    rotated_pic,
    resp_MR_medium.keys,
    resp_MR_medium.corr,
    resp_MR_medium.rt
  ) |>
    drop_na() |> # remove technical NAs (recording artefacts, not missing data)
    mutate(task = "MR",
           # add task name (mental rotation)
           level = "medium",
           # add difficulty level
           trial = 1:16) |>  # number trials
    rename(
      # rename columns for handy usage
      "key" = resp_MR_medium.keys,
      "is_correct" = resp_MR_medium.corr,
      "rt" = resp_MR_medium.rt
    ) -> MR_medium # ready to use
  
  
  
  d |> select(
    # select columns we need
    correctAns,
    base_pic,
    rotated_pic,
    resp_MR_hard.keys,
    resp_MR_hard.corr,
    resp_MR_hard.rt
  ) |>
    drop_na() |> # remove technical NAs (recording artefacts, not missing data)
    mutate(task = "MR",
           # add task name (mental rotation)
           level = "hard",
           # add difficulty level
           trial = 1:16) |> # number trials
    rename(
      # rename columns for handy usage
      "key" = resp_MR_hard.keys,
      "is_correct" = resp_MR_hard.corr,
      "rt" = resp_MR_hard.rt
    ) -> MR_hard # ready to use
  
  # bind all conditions of mental rotation task to one tibble
  
  bind_rows(MR_easy, MR_medium, MR_hard) -> MR
  
  return(MR)
  
}

st_preproc <- function(d) {
  
  require(tidyverse)
  
  d |> select(
    # select columns we need
    target_present,
    key_resp_SE.keys,
    key_resp_SE.corr,
    key_resp_SE.rt
  ) |>
    drop_na() |> # remove technical NAs (recording artefacts, not missing data)
    mutate(task = "ST",
           # add task name (Sternberg task)
           level = "easy",
           # add difficulty level
           trial = 1:16) |> # number trials
    rename(
      # rename columns for handy usage
      "key" = key_resp_SE.keys,
      "is_correct" = key_resp_SE.corr,
      "rt" = key_resp_SE.rt
    ) -> ST_easy # ready to use
  
  d |> select(
    # select columns we need
    target_present,
    key_resp_SM.keys,
    key_resp_SM.corr,
    key_resp_SM.rt
  ) |>
    drop_na() |> # remove technical NAs (recording artefacts, not missing data)
    mutate(task = "ST",
           # add task name (Sternberg task)
           level = "medium",
           # add difficulty level
           trial = 1:16) |> # number trials
    rename(
      # rename columns for handy usage
      "key" = key_resp_SM.keys,
      "is_correct" = key_resp_SM.corr,
      "rt" = key_resp_SM.rt
    ) -> ST_medium # ready to use
  
  
  d |> select(
    # select columns we need
    target_present,
    resp_S_H_trials.keys,
    resp_S_H_trials.corr,
    resp_S_H_trials.rt
  ) |>
    drop_na() |> # remove technical NAs (recording artefacts, not missing data)
    mutate(task = "ST",
           # add task name (Sternberg task)
           level = "hard",
           # add difficulty level
           trial = 1:16) |> # number trials
    rename(
      # rename columns for handy usage
      "key" = resp_S_H_trials.keys,
      "is_correct" = resp_S_H_trials.corr,
      "rt" = resp_S_H_trials.rt
    ) -> ST_hard # ready to use
  
  # bind all conditions of sternberg task to one tibble
  bind_rows(ST_easy, ST_hard, ST_medium) -> ST
  
  return(ST)
  
}

ms_preproc <- function(d) {
  
  # Since we our participants could fill the fields in any order, 
  # here is a function which allows us to count correct inputs 
  # our subjects made.
  
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
        "rt" = "mouse_MSe.time"
      ) |>
      select(-c(paste0("noun", 4:7))) -> MS_easy
    
    d |> select(
      matches("^noun"),
      matches("MSm.text$"),
      "mouse_MSm.time"
    ) |>
      filter_at(vars(paste0("noun", 4:5)), all_vars(!is.na(.))) |>
      filter_at(vars(paste0("noun", 6:7)), all_vars(is.na(.))) |>
      mutate(task = "MS",
             level = "medium") |> 
      rename("rt" = "mouse_MSm.time") |>
      rename_with(.fn = str_replace_all, 
                  pattern = "_MSm\\.text", 
                  replacement = "") |> 
      select(-noun6, -noun7) -> MS_medium
    
    
    d |> select(
      matches("^noun"),
      matches("MSh.text$"),
      "mouse_MSh.time"
    ) |>
      filter_at(vars(paste0("noun", 1:7)), all_vars(!is.na(.))) |>
      mutate(task = "MS",
             level = "hard") |>
      rename("rt" = "mouse_MSh.time") |> 
      rename_with(.fn = str_replace_all,
                  pattern = "_MSh\\.text",
                  replacement = "") -> MS_hard
    
  } else {
    
    d |> select(matches("^noun"),
                matches("resp\\d\\.text$")) |>
      filter_at(vars(paste0("noun", 1:3)), all_vars(!is.na(.))) |>
      filter_at(vars(paste0("noun", 4:7)), all_vars(is.na(.))) |>
      mutate(task = "MS",
             level = "easy",
             rt = NA) |>
      rename(
        "resp1" = resp1.text,
        "resp2" = resp2.text,
        "resp3" = resp3.text
      ) |>
      select(-c(paste0("noun", 4:7))) -> MS_easy
    
    d |> select(matches("^noun"),
                matches("MSm.text$")) |>
      filter_at(vars(paste0("noun", 4:5)), all_vars(!is.na(.))) |>
      filter_at(vars(paste0("noun", 6:7)), all_vars(is.na(.))) |>
      mutate(task = "MS",
             level = "medium",
             rt = NA) |>
      rename_with(.fn = str_replace_all,
                  pattern = "_MSm\\.text",
                  replacement = "") |> 
      select(-noun6, -noun7) -> MS_medium
    
    
    d |> select(matches("^noun"),
                matches("MSh.text$")) |>
      filter_at(vars(paste0("noun", 1:7)), all_vars(!is.na(.))) |>
      mutate(task = "MS",
             level = "hard",
             rt = NA) |>
      rename_with(.fn = str_replace_all,
                  pattern = "_MSh\\.text", 
                  replacement = "") -> MS_hard
  }
  
  tibble(
    #id = MS_easy$id[1],
    trials = 1:16,
    MS_easy_n = n_count(MS_easy),
    MS_easy_rt = MS_easy$rt,
    MS_medium_n = n_count(MS_medium),
    MS_medium_rt = MS_medium$rt,
    MS_hard_n = n_count(MS_hard),
    MS_hard_rt = MS_hard$rt
  ) |>
    pivot_longer(cols = -c("trials"), values_to = "value") |>
    separate(name, c("task", "level", "name")) |>
    pivot_wider(values_from = value, names_from = name) |>
    mutate(acc = ifelse(level == "easy", n / 3,
                        ifelse(
                          level == "medium", n / 5,
                          ifelse(level == "hard", n / 7, NA)
                        ))) -> MS
  
  return(MS)
  
}

nasatlx_preproc <- function(d) {
  d |> select(slider.response,
              head,
              task_type,
              task_level) |>
    filter_at(vars(head, task_type, task_level), all_vars(!is.na(.))) |>
    rename("score" = slider.response) |>
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
    select(scale, score, task, level) -> NASATLX
  
  return(NASATLX)
}

# sequence_preproc <- function(d) {
#   
#   d |> select(
#     E_rotation,
#     M_rotation,
#     H_rotation,
#     E_Sternberg,
#     M_Sternberg,
#     H_Sternberg,
#     E_span,
#     M_span,
#     H_span
#   ) |>
#     drop_na() |>
#     sapply(function(x) which(x == 1)) -> v 
#   
#   tibble(name = names(v),
#          order = v,
#          id = d[["Индивидуальный_код"]][1]) |>
#     arrange(order) |>
#     separate(name, c("level", "task"), "_") |>
#     mutate(
#       task = recode(
#         task,
#         "rotation" = "MR",
#         "Sternberg" = "ST",
#         "span" = "MS"
#       ),
#       level = recode(
#         level,
#         "E" = "easy",
#         "M" = "medium",
#         "H" = "hard"
#       )
#     ) -> SEQUENCE
#   
#   return(SEQUENCE)
#   
# }

files <- paste0("data-toloka/", dir("data-toloka"))

MR_data <- tibble()
ST_data <- tibble()
MS_data <- tibble()
NASATLX_data <- tibble()
#SEQUENCE_data <- tibble()

for (i in 1:length(files)) {
  
  print(files[i])
  
  d <- read_csv(files[i], show_col_types = FALSE)
  
  MR_data |> bind_rows(mr_preproc(d) |> mutate(file = files[i])) -> MR_data
  ST_data |> bind_rows(st_preproc(d) |> mutate(file = files[i])) -> ST_data
  MS_data |> bind_rows(ms_preproc(d) |> mutate(file = files[i])) -> MS_data
  NASATLX_data |> bind_rows(nasatlx_preproc(d) |> mutate(file = files[i])) -> NASATLX_data
  #SEQUENCE_data |> bind_rows(sequence_preproc(d)) -> SEQUENCE_data
  
}

MR_data |> View()
ST_data |> View()
MS_data |> View()

# MR_data |> write_csv("mentral_rotation_data_toloka.csv")
# ST_data |> write_csv("sternberg_data_toloka.csv")
# MS_data |> write_csv("mental_span_data_toloka.csv")
# NASATLX_data |> write_csv("nasa_tlx_data_toloka.csv")
# SEQUENCE_data |> write_csv("sequence_data_toloka.csv")



MR_data |> 
  ggplot(aes(rt, fill = level)) +
  geom_density(alpha = .5) +
  facet_wrap(~ file) +
  labs(title = "MR")

MR_data |> 
  ggplot(aes(level, rt)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") +
  facet_wrap(~ file) +
  labs(title = "MR")

MR_data |> 
  ggplot(aes(level, is_correct)) +
  stat_summary(fun = mean, geom = "point") +
  facet_wrap(~ file) +
  labs(title = "MR")


MS_data |> 
  ggplot(aes(rt, fill = level)) +
  geom_density(alpha = .5) +
  facet_wrap(~ file) +
  labs(title = "MS")

MS_data |> 
  ggplot(aes(level, rt)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") +
  facet_wrap(~ file) +
  labs(title = "MS")

MS_data |> 
  ggplot(aes(level, acc)) +
  stat_summary(fun = mean, geom = "point") +
  facet_wrap(~ file) +
  labs(title = "MS")


ST_data |> 
  ggplot(aes(rt, fill = level)) +
  geom_density(alpha = .5) +
  facet_wrap(~ file) +
  labs(title = "ST")

ST_data |> 
  ggplot(aes(level, rt)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") +
  facet_wrap(~ file) +
  labs(title = "ST")

ST_data |> 
  ggplot(aes(level, is_correct)) +
  stat_summary(fun = mean, geom = "point") +
  facet_wrap(~ file) +
  labs(title = "ST")


