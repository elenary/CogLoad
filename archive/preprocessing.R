library(tidyverse) 

# setwd("/Users/elenary/Dropbox/Datanal/CogLoad/")

# load data, bind conditions in MR, parse angles by axis ---------------------------------------------------------------

#pilots with angels: 
# easy x = 45, z = 0 | x = 0, z = 45, 
# middle x = 45, z = 135 | x = 135, z = 45
# hard x = 135, z = 135

pilot4 <- read_csv("/Users/elenary/Google Drive/! HSE CogLoad/Данные пробация опросника/pilot_4_entire_exp_2022-05-26_11h21.01.394.csv")
pilot5 <- read_csv("/Users/elenary/Google Drive/! HSE CogLoad/Данные пробация опросника/pilot5_entire_exp_2022-05-30_17h54.50.600.csv")
pilot6 <- read_csv("/Users/elenary/Google Drive/! HSE CogLoad/Данные пробация опросника/pilot6_entire_exp_2022-05-30_22h39.30.csv")

#exps with angels: 
# easy z = 45, x = 0 | z = 135, x = 0, 
# middle z = 90, x = 45 | z = 90, x = 135 | z = 270, x = 45 | z = 270, x = 135 
# hard z = 90, x = 90 | z = 90, x = 270 | z = 270, x = 90 | z = 270, x = 270 

exp_raw_001SSMS <- read_csv("/Users/elenary/Google Drive/! HSE CogLoad/Данные пробация опросника/001SSMS_entire_exp_2022-05-31_18h24.17.csv")
exp_raw_004SSDR <- read_csv("/Users/elenary/Google Drive/! HSE CogLoad/Данные пробация опросника/004SSDR_entire_exp_2022-06-02_21h35.09.567.csv")
exp_raw_005SSDR <- read_csv("/Users/elenary/Google Drive/! HSE CogLoad/Данные пробация опросника/005SSDR_entire_exp_2022-05-31_20h43.50.910.csv")
exp_raw_006SSEE <- read_csv("/Users/elenary/Google Drive/! HSE CogLoad/Данные пробация опросника/006SSEE_entire_exp_2022-06-02_22h14.18.092.csv")
exp_raw_007SSJM <- read_csv("/Users/elenary/Google Drive/! HSE CogLoad/Данные пробация опросника/007SSJM_entire_exp_2022-06-01_15h12.27.341.csv")
exp_raw_009SSAP <- read_csv("/Users/elenary/Google Drive/! HSE CogLoad/Данные пробация опросника/009SSAP_entire_exp_2022-06-04_15h48.30.436.csv")
exp_raw_010SSEA <- read_csv("/Users/elenary/Google Drive/! HSE CogLoad/Данные пробация опросника/010SSEA_entire_exp_2022-06-07_24h08.05.064.csv")
exp_raw_011SSAB <- read_csv("/Users/elenary/Google Drive/! HSE CogLoad/Данные пробация опросника/011SSAB_entire_exp_2022-06-01_14h54.48.csv")

MR_conditions_function <- function(exp_raw_001SSMS) {
  
  easy = exp_raw_001SSMS %>% 
    select(base = base_pic, target = rotated_pic, acc = resp_MR_easy.corr, rt = resp_MR_easy.rt, correctAns=correctAns) %>%
    na.omit() %>%
    mutate(level = "easy") %>% 
    mutate()
  #  mutate(x = c(0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,1))
  
  medium = exp_raw_001SSMS %>% 
    select(base = base_pic, target = rotated_pic, acc = resp_MR_medium.corr, rt = resp_MR_medium.rt, correctAns=correctAns) %>%
    na.omit() %>%
    mutate(level = "medium")
  #  mutate(x = c(1,0,0,1,1,1,0,0,0,0,0,1,1,1,0,0))
  
  hard = exp_raw_001SSMS %>% 
    select(base = base_pic, target = rotated_pic, acc = resp_MR_hard.corr, rt = resp_MR_hard.rt, correctAns=correctAns) %>%
    na.omit() %>%
    mutate(level = "hard")
  
  exp001SSMS <- rbind(easy,medium,hard)
  
  exp001SSMS %>% 
    separate(col = target, into = c(NA, NA, NA, "x", NA, "y", NA, "z"), sep = "_") -> exp001SSMS_2
  
  exp001SSMS_2 %>% 
    separate(col = z, into = c("z", NA), sep = "\\.") -> exp001SSMS
  
  return(exp001SSMS)
  
}

# gsub("x_.", "", MR_pilot4$target)

MR_exp001SSMS <- MR_conditions_function(exp_raw_001SSMS)
MR_exp004SSDR <- MR_conditions_function(exp_raw_004SSDR)
MR_exp005SSDR <- MR_conditions_function(exp_raw_005SSDR)
MR_exp006SSEE <- MR_conditions_function(exp_raw_006SSEE)
MR_exp007SSJM <- MR_conditions_function(exp_raw_007SSJM)
MR_exp009SSAP <- MR_conditions_function(exp_raw_009SSAP)
MR_exp010SSEA <- MR_conditions_function(exp_raw_010SSEA)
MR_exp011SSAB <- MR_conditions_function(exp_raw_011SSAB)

View(MR_exp001SSMS)

# summary stats by RT and errors by axis ----------------------------------

MR_summary_conditions <- function(MR_exp001SSMS) {
  
  MR_exp001SSMS %>% 
    group_by(x, z) %>% 
    summarise(mean_rt = mean(rt), overall_probes = length(y), correct_per = round(sum(acc)/length(y),2), .groups = "keep") -> xz_MR_exp001SSMS
  return(xz_MR_exp001SSMS)
}

xz_MR_exp001SSMS <- MR_summary_conditions(MR_exp001SSMS)
xz_MR_exp004SSDR <- MR_summary_conditions(MR_exp004SSDR)
xz_MR_exp005SSDR <- MR_summary_conditions(MR_exp005SSDR)
xz_MR_exp006SSEE <- MR_summary_conditions(MR_exp006SSEE)
xz_MR_exp007SSJM <- MR_summary_conditions(MR_exp007SSJM)
xz_MR_exp009SSAP <- MR_summary_conditions(MR_exp009SSAP)
xz_MR_exp010SSEA <- MR_summary_conditions(MR_exp010SSEA)
xz_MR_exp011SSAB <- MR_summary_conditions(MR_exp011SSAB)

View(xz_MR_exp005SSDR)

# xz_MR_pilot4 %>% 
#   full_join(xz_MR_pilot5, by = c("x", "z")) %>%  
#   full_join(xz_MR_pilot6, by = c("x", "z")) %>% View()
#   
#   write_csv("three_pilots.csv", na = "NA", col_names = TRUE, "/Users/elenary/Dropbox/Datanal/CogLoad/")


library(ez)
library(lmer)

ezANOVA()


# t test ------------------------------------------------------------------

t.test(easy$rt, medium$rt)
t.test(easy$rt, hard$rt)
t.test(medium$rt, hard$rt)

mean(medium$rt[medium$x == 1])
mean(medium$rt[medium$x == 0])

mean(easy$rt[easy$x == 1])
mean(easy$rt[easy$x == 0])

mean(easy$rt)
mean(hard$rt)
