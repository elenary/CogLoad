st_preproc <- function(d) {

  require(tidyverse)
  
  d |> select(
    # select columns we need
    "Индивидуальный_код",
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
      "id" = "Индивидуальный_код",
      "key" = key_resp_SE.keys,
      "is_correct" = key_resp_SE.corr,
      "rt" = key_resp_SE.rt
    ) -> ST_easy # ready to use
  
  d |> select(
    # select columns we need
    "Индивидуальный_код",
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
      "id" = "Индивидуальный_код",
      "key" = key_resp_SM.keys,
      "is_correct" = key_resp_SM.corr,
      "rt" = key_resp_SM.rt
    ) -> ST_medium # ready to use
  
  
  d |> select(
    # select columns we need
    "Индивидуальный_код",
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
      "id" = "Индивидуальный_код",
      "key" = resp_S_H_trials.keys,
      "is_correct" = resp_S_H_trials.corr,
      "rt" = resp_S_H_trials.rt
    ) -> ST_hard # ready to use
  
  # bind all conditions of sternberg task to one tibble
  bind_rows(ST_easy, ST_hard, ST_medium) -> ST
  
  return(ST)

}