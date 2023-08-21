library(tidyverse)

files <- dir("data2")

d <- read_csv(paste0("data2/", files[39]))

colnames(d)[colnames(d) |> str_detect(".rt")]
colnames(d)[colnames(d) |> str_detect(".time")]

read_csv(paste0("data2/", files[38])) |>
  select(contains(".rt") | contains(".time")) |> 
  sapply(FUN = sum, na.rm = TRUE) |> unlist() |> 
  sum() / 60




vn <- numeric()
va <- numeric()

for (j in 1:length(files)) {
  print(j)
  print(files[j])
  d <- read_csv(paste0("data2/", files[j]))
  d |> nrow() -> n
  d |> select(contains(".rt") | contains(".time")) |> 
    sapply(FUN = sum, na.rm = TRUE) |> unlist() |> 
    sum() / 60 -> a 
  vn <- append(vn, n)  
  va <- append(va, a)
}

length(files)
length(va)
length(vn)

vn
va

va[vn == 232] -> va_

max(va_)
min(va_)


hist(va_, breaks = 30)
