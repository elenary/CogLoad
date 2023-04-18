library(tidyverse)

# rm(list=ls())

# FUNCTIONS ----

## MR

mr_preproc <- function(d) {
  
  d %>% select("Индивидуальный_код",
               correctAns,
               base_pic,
               rotated_pic,
               resp_MR_easy.keys,
               resp_MR_easy.corr,
               resp_MR_easy.rt) %>%
    na.omit() %>% 
    mutate(task = "MR",
           level = "easy",
           trial = 1:16) %>% 
    rename("id" = "Индивидуальный_код",
           "key" = resp_MR_easy.keys,
           "is_correct" = resp_MR_easy.corr,
           "rt" = resp_MR_easy.rt) -> MR_easy
  
  
  d %>% select("Индивидуальный_код",
               correctAns,
               base_pic,
               rotated_pic,
               resp_MR_medium.keys,
               resp_MR_medium.corr,
               resp_MR_medium.rt) %>%
    na.omit() %>% 
    mutate(task = "MR",
           level = "medium",
           trial = 1:16) %>% 
    rename("id" = "Индивидуальный_код",
           "key" = resp_MR_medium.keys,
           "is_correct" = resp_MR_medium.corr,
           "rt" = resp_MR_medium.rt) -> MR_medium
  
  
  
  d %>% select("Индивидуальный_код",
               correctAns,
               base_pic,
               rotated_pic,
               resp_MR_hard.keys,
               resp_MR_hard.corr,
               resp_MR_hard.rt) %>%
    na.omit() %>% 
    mutate(task = "MR",
           level = "hard",
           trial = 1:16) %>% 
    rename("id" = "Индивидуальный_код",
           "key" = resp_MR_hard.keys,
           "is_correct" = resp_MR_hard.corr,
           "rt" = resp_MR_hard.rt) -> MR_hard
  
  bind_rows(MR_easy, MR_medium, MR_hard) ->> MR
  
}

## ST

st_preproc <- function(d) {
  d %>% select(
    "Индивидуальный_код",
    target_present,
    key_resp_SE.keys,
    key_resp_SE.corr,
    key_resp_SE.rt
  ) %>%
    na.omit() %>%
    mutate(task = "ST",
           level = "easy",
           trial = 1:16) %>%
    rename(
      "id" = "Индивидуальный_код",
      "key" = key_resp_SE.keys,
      "is_correct" = key_resp_SE.corr,
      "rt" = key_resp_SE.rt
    ) -> ST_easy
  
  d %>% select("Индивидуальный_код",
               target_present,
               key_resp_SM.keys,
               key_resp_SM.corr,
               key_resp_SM.rt) %>% 
    na.omit() %>% 
    mutate(task = "ST",
           level = "medium",
           trial = 1:16) %>% 
    rename("id" = "Индивидуальный_код",
           "key" = key_resp_SM.keys,
           "is_correct" = key_resp_SM.corr,
           "rt" = key_resp_SM.rt) -> ST_medium
  
  
  d %>% select("Индивидуальный_код",
               target_present,
               resp_S_H_trials.keys,
               resp_S_H_trials.corr,
               resp_S_H_trials.rt) %>% 
    na.omit() %>% 
    mutate(task = "ST",
           level = "hard",
           trial = 1:16) %>% 
    rename("id" = "Индивидуальный_код",
           "key" = resp_S_H_trials.keys,
           "is_correct" = resp_S_H_trials.corr,
           "rt" = resp_S_H_trials.rt) -> ST_hard
  
  bind_rows(ST_easy, ST_hard, ST_medium) ->> ST
  
}

## MS

n_count <- function(df) {
  df %>% select(matches("^noun")) %>% as.matrix() -> s
  df %>% select(matches("^resp")) %>% as.matrix() -> r
  a <- vector(mode = "numeric", length = 16L)
  for (i in 1:16) {
    a[i] <- sum(r[i, ] %in% s[i, ])
  }
  return(a)
}

ms_preproc <- function(d) {
  
  d %>% select("Индивидуальный_код",
               matches("^noun"),
               matches("resp\\d\\.text$")) %>%
    filter_at(vars(paste0("noun", 1:3)), all_vars(!is.na(.))) %>%
    filter_at(vars(paste0("noun", 4:7)), all_vars(is.na(.))) %>%
    mutate(task = "MS",
           level = "easy") %>%
    rename(
      "resp1" = resp1.text,
      "resp2" = resp2.text,
      "resp3" = resp3.text,
      "id" = "Индивидуальный_код"
    ) %>%
    select(-c(paste0("noun", 4:7))) -> MS_easy
  
  d %>% select("Индивидуальный_код",
               matches("^noun"),
               matches("MSm.text$")) %>%
    filter_at(vars(paste0("noun", 4:5)), all_vars(!is.na(.))) %>%
    filter_at(vars(paste0("noun", 6:7)), all_vars(is.na(.))) %>%
    mutate(task = "MS",
           level = "medium") %>%
    rename(setNames(names(.), gsub("_MSm\\.text", "", names(.))),
           "id" = "Индивидуальный_код") %>%
    select(-noun6, -noun7) -> MS_medium
  
  
  d %>% select("Индивидуальный_код",
               matches("^noun"),
               matches("MSh.text$")) %>%
    filter_at(vars(paste0("noun", 1:7)), all_vars(!is.na(.))) %>%
    mutate(task = "MS",
           level = "hard") %>%
    rename(setNames(names(.), gsub("_MSh\\.text", "", names(.))),
           "id" = "Индивидуальный_код") -> MS_hard
  
  tibble(id = MS_easy$id[1],
         MS_easy = n_count(MS_easy),
         MS_medium = n_count(MS_medium),
         MS_hard = n_count(MS_hard),
         trial = 1:16) %>% 
    pivot_longer(cols = -c("id", "trial"), values_to = "n") %>% 
    separate(name, c("task", "level")) %>% 
    mutate(acc = ifelse(level == "easy", n/3,
                        ifelse(level == "medium", n/5,
                               ifelse(level == "hard", n/7, NA)))) ->> MS
  
}

## NASA-TLX

nasa_tlx_preproc <- function(d){
  d %>% select("Индивидуальный_код",
               slider.response,
               head,
               task_type,
               task_level) %>%
    filter_at(vars(head, task_type, task_level), all_vars(!is.na(.))) %>%
    rename("id" = "Индивидуальный_код",
           "score" = slider.response) %>% 
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
    ) %>%
    select(id, scale, score, task, level) ->> NASA_TLX
  
}

## SEQUENCE

sequence_preproc <- function(d) {
  d %>% select(
    E_rotation,
    M_rotation,
    H_rotation,
    E_Sternberg,
    M_Sternberg,
    H_Sternberg,
    E_span,
    M_span,
    H_span
  ) %>%
    na.omit() %>%
    sapply(function(x)
      which(x == 1)) %>%
    unlist() %>%
    tibble(name = names(.),
           order = .,
           id = d[["Индивидуальный_код"]][1]) %>%
    arrange(order) %>%
    separate(name, c("level", "task"), "_") %>%
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
    ) ->> SEQUENCE
}


# PREPROCESS

files <- dir("data")

MR_data <- tibble()
ST_data <- tibble()
MS_data <- tibble()
NASA_TLX_data <- tibble()
SEQUENCE_data <- tibble()

for (j in 1:length(files)) {
  d <- read_csv(paste0("data/", files[j]))
  mr_preproc(d)
  st_preproc(d)
  ms_preproc(d)
  nasa_tlx_preproc(d)
  sequence_preproc(d)
  
  MR_data %>% 
    bind_rows(MR) -> MR_data
  ST_data %>% 
    bind_rows(ST) -> ST_data
  MS_data %>% 
    bind_rows(MS) -> MS_data
  NASA_TLX_data %>% 
    bind_rows(NASA_TLX) -> NASA_TLX_data
  SEQUENCE_data %>% 
    bind_rows(SEQUENCE) -> SEQUENCE_data
}

MR_data %>% write_csv("mentral_rotation_data.csv")
ST_data %>% write_csv("sternberg_data.csv")
MS_data %>% write_csv("mental_span_data.csv")
NASA_TLX_data %>% write_csv("nasa_tlx_data.csv")
SEQUENCE_data %>% write_csv("sequence_data.csv")





