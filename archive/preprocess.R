library(tidyverse)

d <- read_csv("data/080SSAF_entire_exp_2022-09-09_18h48.13.239.csv")

#View(d)

# dir("data")


## MR----

d %>% select("Индивидуальный_код",
             correctAns,
             base_pic,
             rotated_pic,
             resp_MR_easy.keys,
             resp_MR_easy.corr,
             resp_MR_easy.rt) %>%
  na.omit() %>% 
  mutate(task = "MR",
         level = "easy") %>% 
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
         level = "medium") %>% 
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
         level = "hard") %>% 
  rename("id" = "Индивидуальный_код",
         "key" = resp_MR_hard.keys,
         "is_correct" = resp_MR_hard.corr,
         "rt" = resp_MR_hard.rt) -> MR_hard

bind_rows(MR_easy, MR_medium, MR_hard) -> MR


## ST----


d %>% select(
  "Индивидуальный_код",
  target_present,
  key_resp_SE.keys,
  key_resp_SE.corr,
  key_resp_SE.rt
) %>%
  na.omit() %>%
  mutate(task = "ST",
         level = "easy") %>%
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
         level = "medium") %>% 
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
         level = "hard") %>% 
  rename("id" = "Индивидуальный_код",
         "key" = resp_S_H_trials.keys,
         "is_correct" = resp_S_H_trials.corr,
         "rt" = resp_S_H_trials.rt) -> ST_hard

bind_rows(ST_easy, ST_hard, ST_medium) -> ST







## MS----

d %>% View

colnames(d) %>% str_detect("space_perehod") %>% colnames(d)[.]
colnames(d) %>% str_detect("mouse_") %>% colnames(d)[.]

if ("mouse_MSe.time" %in% colnames(d)) {
  d %>% select("Индивидуальный_код",
               matches("^noun"),
               matches("resp\\d\\.text$"),
               "mouse_MSe.time") %>%
    filter_at(vars(paste0("noun", 1:3)), all_vars(!is.na(.))) %>% 
    filter_at(vars(paste0("noun", 4:7)), all_vars(is.na(.))) %>% 
    mutate(task = "MS",
           level = "easy",
           trials = 1:16) %>% 
    rename("resp1" = resp1.text,
           "resp2" = resp2.text,
           "resp3" = resp3.text,
           "id" = "Индивидуальный_код",
           "rt" = mouse_MSe.time) %>% 
    select(-c(paste0("noun", 4:7))) -> MS_easy
} else {
  d %>% select("Индивидуальный_код",
             matches("^noun"),
             matches("resp\\d\\.text$")) %>%
  filter_at(vars(paste0("noun", 1:3)), all_vars(!is.na(.))) %>% 
  filter_at(vars(paste0("noun", 4:7)), all_vars(is.na(.))) %>% 
  mutate(task = "MS",
         level = "easy",
         rt = NA,
         trials = 1:16) %>% 
  rename("resp1" = resp1.text,
         "resp2" = resp2.text,
         "resp3" = resp3.text,
         "id" = "Индивидуальный_код") %>% 
  select(-c(paste0("noun", 4:7))) -> MS_easy
}

MS_easy %>% View

d %>% select("Индивидуальный_код",
             matches("^noun"),
             matches("MSm.text$"),
             "mouse_MSm.time") %>%
  filter_at(vars(paste0("noun", 4:5)), all_vars(!is.na(.))) %>%
  filter_at(vars(paste0("noun", 6:7)), all_vars(is.na(.))) %>% 
  mutate(task = "MS",
         level = "medium",
         trials = 1:16) %>% 
  rename(setNames(names(.), gsub("_MSm\\.text", "", names(.))),
         "id" = "Индивидуальный_код",
         "rt" = "mouse_MSm.time") %>% 
  select(-noun6, -noun7) -> MS_medium


d %>% select("Индивидуальный_код",
             matches("^noun"),
             matches("MSh.text$"),
             "mouse_MSh.time") %>%
  filter_at(vars(paste0("noun", 1:7)), all_vars(!is.na(.))) %>% 
  mutate(task = "MS",
         level = "hard",
         trials = 1:16) %>% 
  rename(setNames(names(.), gsub("_MSh\\.text", "", names(.))),
         "id" = "Индивидуальный_код",
         "rt" = "mouse_MSh.time") -> MS_hard

n_count <- function(df) {
  df %>% select(matches("^noun")) %>% as.matrix() -> s
  df %>% select(matches("^resp")) %>% as.matrix() -> r
  a <- vector(mode = "numeric", length = 16L)
  for (i in 1:16) {
    a[i] <- sum(r[i, ] %in% s[i, ])
  }
  return(a)
}

n_count(MS_easy)
n_count(MS_medium)
n_count(MS_hard)


tibble(id = MS_easy$id[1],
       trials = 1:16,
       MS_easy_n = n_count(MS_easy),
       MS_easy_rt = MS_easy$rt,
       MS_medium_n = n_count(MS_medium),
       MS_medium_rt = MS_medium$rt,
       MS_hard_n = n_count(MS_hard),
       MS_hard_rt = MS_hard$rt) %>% 
  pivot_longer(cols = -c("id", "trials"), values_to = "value") %>% 
  separate(name, c("task", "level", "name")) %>% 
  pivot_wider(values_from = value, names_from = name) %>% 
  mutate(acc = ifelse(level == "easy", n/3,
                      ifelse(level == "medium", n/5,
                             ifelse(level == "hard", n/7, NA)))) -> MS



## NASA-TLX ----

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
  select(id, scale, score, task, level) -> nasa_tlx



## SEQUENCE ----


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
  ) -> sequence



sapply(d1, function(x) which(x == 1))




















