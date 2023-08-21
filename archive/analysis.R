library(tidyverse)
library(lavaan)
theme_set(theme_bw())

## rm(list=ls())


## reading data

MR <- read_csv("mentral_rotation_data.csv")
ST <- read_csv("sternberg_data.csv")
MS <- read_csv("mental_span_data.csv")
NASA_TLX <- read_csv("nasa_tlx_data.csv")
SEQ <- read_csv("sequence_data.csv")


# preprocess

NASA_TLX %>% 
  mutate(level = factor(level, levels = c("easy", "medium", "hard"), 
                        ordered = TRUE), # fix factor
         scl = str_to_lower(scale), # modify vars to match with previously created encoding
         tsk = str_to_lower(task),
         lvl = str_sub(level, start = 1, end = 1) %>% str_to_upper()) %>% 
         unite(item, scl, tsk, lvl) -> NASA_TLX # create a new var for CFA





# plotting

NASA_TLX %>%
  ggplot(aes(level, score, fill = scale)) +
  geom_boxplot() +
  facet_grid(task ~ .)



## CFA

NASA_TLX %>% 
  mutate(score_hat = round(score * 5)) %>% 
  pivot_wider(id_cols = id, names_from = item, values_from = score_hat) -> NASA_TLX_w
rm(NASA_TLX_w)

sapply(NASA_TLX_w %>% select(-id), function(x)
  sum(is.na(x))) %>%
  `/`(., nrow(NASA_TLX_w)) %>%
  round(2) %>%
  tibble(prop = .,
         item = names(.)) %>% 
  ggplot(aes(item, prop)) +
  geom_point() +
  coord_flip()


model1 <- "PE =~ pe_st_E + pe_st_M + pe_st_H + pe_mr_E + pe_mr_M + pe_mr_H + pe_ms_E + pe_ms_M + pe_ms_H
ME =~ me_st_E + me_st_M + me_st_H + me_mr_E + me_mr_M + me_mr_H + me_ms_E + me_ms_M + me_ms_H
PH =~ ph_st_E + ph_st_M + ph_st_H + ph_mr_E + ph_mr_M + ph_mr_H + ph_ms_E + ph_ms_M + ph_ms_H
EF =~ ef_st_E + ef_st_M + ef_st_H + ef_mr_E + ef_mr_M + ef_mr_H + ef_ms_E + ef_ms_M + ef_ms_H
TI =~ ti_st_E + ti_st_M + ti_st_H + ti_mr_E + ti_mr_M + ti_mr_H + ti_ms_E + ti_ms_M + ti_ms_H
FR =~ fr_st_E + fr_st_M + fr_st_H + fr_mr_E + fr_mr_M + fr_mr_H + fr_ms_E + fr_ms_M + fr_ms_H
OW =~ PE + ME + PH + EF + TI + FR"

cfa1 <- cfa(model1, NASA_TLX_w)






## RT

MR %>% 
  mutate(level = factor(level, 
                        ordered = TRUE, 
                        levels = c("easy", "medium", "hard"))) %>% 
  ggplot(aes(level, rt)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  stat_summary(fun = mean, geom = "point") +
  labs(x = "Уровень сложности",
       y = "Время реакции",
       title = "Задача мысленного вращения") +
  scale_x_discrete(labels = c(easy = "легкий", medium = "средний", hard = "сложный"))
ggsave("MR_rt.png", dpi = 300)

MR %>% 
  mutate(level = factor(level, 
                        ordered = TRUE, 
                        levels = c("easy", "medium", "hard"))) %>% 
  group_by(id, level) %>% 
  mutate(acc = mean(is_correct)) %>% 
  ggplot(aes(level, acc)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  stat_summary(fun = mean, geom = "point") +
  labs(x = "Уровень сложности",
       y = "Точность",
       title = "Задача мысленного вращения") +
  scale_x_discrete(labels = c(easy = "легкий", medium = "средний", hard = "сложный"))
ggsave("MR_acc.png", dpi = 300)

MS %>% 
  mutate(level = factor(level, 
                        ordered = TRUE, 
                        levels = c("easy", "medium", "hard"))) %>% 
  ggplot(aes(level, rt)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  stat_summary(fun = mean, geom = "point") +
  labs(x = "Уровень сложности",
       y = "Время реакции",
       title = "Задача Mental Span") +
  scale_x_discrete(labels = c(easy = "легкий", medium = "средний", hard = "сложный"))
ggsave("MS_rt.png", dpi = 300)

MS %>% 
  mutate(level = factor(level, 
                        ordered = TRUE, 
                        levels = c("easy", "medium", "hard"))) %>% 
  # group_by(id, level) %>% 
  # mutate(acc = mean(is_correct)) %>% 
  ggplot(aes(level, acc)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  stat_summary(fun = mean, geom = "point") +
  labs(x = "Уровень сложности",
       y = "Точность",
       title = "Задача Mental Span") +
  scale_x_discrete(labels = c(easy = "легкий", medium = "средний", hard = "сложный"))
ggsave("MS_acc.png", dpi = 300)

ST %>% 
  mutate(level = factor(level, 
                        ordered = TRUE, 
                        levels = c("easy", "medium", "hard"))) %>% 
  ggplot(aes(level, rt)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  stat_summary(fun = mean, geom = "point") +
  labs(x = "Уровень сложности",
       y = "Время реакции",
       title = "Задача Стернберга") +
  scale_x_discrete(labels = c(easy = "легкий", medium = "средний", hard = "сложный"))
ggsave("ST_rt.png", dpi = 300)

ST %>% 
  mutate(level = factor(level, 
                        ordered = TRUE, 
                        levels = c("easy", "medium", "hard"))) %>% 
  group_by(id, level) %>% 
  mutate(acc = mean(is_correct)) %>% 
  ggplot(aes(level, acc)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  stat_summary(fun = mean, geom = "point") +
  labs(x = "Уровень сложности",
       y = "Точность",
       title = "Задача Стернберга") +
  scale_x_discrete(labels = c(easy = "легкий", medium = "средний", hard = "сложный"))
ggsave("ST_acc.png", dpi = 300)

