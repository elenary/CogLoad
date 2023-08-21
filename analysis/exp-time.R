## Experimental time

EXPTIME <- read_csv("../preproc-data/EXPTIME_firstbanch_data.csv")


EXPTIME |> ggplot(aes(dur)) +
  geom_density()

EXPTIME |> 
  filter(dur < 250) |> 
  ggplot(aes(dur)) +
  # geom_density() +
  geom_histogram() +
  geom_vline(xintercept = 70) +
  geom_vline(xintercept = 75) +
  geom_vline(xintercept = 100)

EXPTIME |> 
  filter(dur < 250) |> 
  ggplot(aes(dur)) +
  geom_boxplot() 
#geom_vline(xintercept = 93, linetype = "dashed")

EXPTIME$dur |> min(na.rm = TRUE)
quantile(EXPTIME$dur[EXPTIME$dur<250], .75, na.rm = TRUE) + 1.5 * IQR(EXPTIME$dur[EXPTIME$dur<250], na.rm = TRUE)
