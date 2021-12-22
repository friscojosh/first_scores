library(tidyverse)
library(psych)

# first score: https://nfl-football.espntrumedia.com/football/teams-summary-stats?f=%7B%22fswr%22%3A%5B%7B%22fromSeason%22%3A2020%2C%22fromWeek%22%3A%221%22%2C%22toSeason%22%3A2020%2C%22toWeek%22%3A%2218%22%7D%5D%2C%22fgt%22%3A%5B%22regular%22%5D%7D&pc=%7B%22fsv%22%3A%22stats%22%2C%22fgb%22%3A%22by-season%22%2C%22ftt%22%3A%22standard%22%7D&t=%7B%22customReport%22%3A%7B%22selectedReportName%22%3A%22Scoring%2520First%22%2C%22selectedReportId%22%3A10009%7D%2C%22orderBy%22%3A%7B%22orderCols%22%3A%22Win%2525_Score1st%22%2C%22sortOrder%22%3A%22DESC%22%7D%7D
# epa: https://nfl-football.espntrumedia.com/football/teams-summary-stats?f=%7B%22fswr%22%3A%5B%7B%22fromSeason%22%3A2006%2C%22fromWeek%22%3A%221%22%2C%22toSeason%22%3A2020%2C%22toWeek%22%3A%2218%22%7D%5D%2C%22fgt%22%3A%5B%22regular%22%5D%7D&pc=%7B%22fsv%22%3A%22stats%22%2C%22fgb%22%3A%22by-season%22%2C%22ftt%22%3A%22standard%22%7D&t=%7B%22customReport%22%3A%7B%22selectedReportName%22%3A%22rush%2520pass%2520epa%22%2C%22selectedReportId%22%3A592%7D%2C%22orderBy%22%3A%7B%22orderCols%22%3A%22%255BOfSnap%255D%22%2C%22sortOrder%22%3A%22DESC%22%7D%7D
# 2021 first score: https://nfl-football.espntrumedia.com/football/teams-summary-stats?f=%7B%22fswr%22%3A%5B%7B%22fromSeason%22%3A%22def%22%2C%22fromWeek%22%3A%221%22%2C%22toSeason%22%3A%22def%22%2C%22toWeek%22%3A%2218%22%7D%5D%2C%22fgt%22%3A%5B%22regular%22%5D%7D&pc=%7B%22fsv%22%3A%22stats%22%2C%22fgb%22%3A%22by-season%22%2C%22ftt%22%3A%22standard%22%7D&t=%7B%22customReport%22%3A%7B%22selectedReportName%22%3A%22Scoring%2520First%22%2C%22selectedReportId%22%3A10009%7D%2C%22orderBy%22%3A%7B%22orderCols%22%3A%22Win%2525_Score1st%22%2C%22sortOrder%22%3A%22DESC%22%7D%7D

first_scores <- read_csv("data/first_score_2000-20.csv") |>
  mutate(first_score_pct = G_Score1st / G,
         win_pct = `Win%`) |>
  select(-Rank, -league, -teamImageId, -teamFullName, -teamAbbrevName, -espnTeamId, -G, -teamId_1, -null, -seasonId, -scatterId, -scatterExtra)

first_scores |>
  ggplot(aes(x = G_Score1st, y = W)) +
  geom_point() +
  geom_smooth()

first_scores |>
  ggplot(aes(x = first_score_pct, y = win_pct)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "First score %", y = "Win %") +
  theme_minimal()

model <- lm(data = first_scores, win_pct ~ first_score_pct)
summary(model)

epa_data <- read_csv("data/epa_2006-20.csv")|>
  mutate(comp_pct = parse_number(`Comp%`),
         first_down_pct = parse_number(`1D%`),
         success_rate = parse_number(`Scs%`)) |>
  select(-Rank, -league, -teamImageId, -teamFullName, -teamAbbrevName, -espnTeamId, -G, -teamId_1, -null, -seasonId, -scatterId, -scatterExtra,
         -`1D%`, -`Comp%`, -`Scs%`)
# join team offense data to the score first dataframe
joined <- first_scores |>
  inner_join(epa_data, by = c("teamId", "team", "season"))

corPlot(joined[, 19:32], scale = FALSE)
# Read the plot as just the first column, and downwards.
# The highest correlation besides win% is with Pass EPA/play.


# https://nfl-football.espntrumedia.com/football/teams-summary-stats?f=%7B%22fswr%22%3A%5B%7B%22fromSeason%22%3A%22def%22%2C%22fromWeek%22%3A1%2C%22toSeason%22%3A%22def%22%2C%22toWeek%22%3A16%7D%5D%2C%22fgt%22%3A%5B%22regular%22%5D%7D&pc=%7B%22fsv%22%3A%22stats%22%2C%22fgb%22%3A%22by-game%22%2C%22ftt%22%3A%22standard%22%7D&t=%7B%22customReport%22%3A%7B%22selectedReportName%22%3A%22Scoring%2520First%22%2C%22selectedReportId%22%3A10009%7D%2C%22orderBy%22%3A%7B%22orderCols%22%3A%22Win%2525_Score1st%22%2C%22sortOrder%22%3A%22DESC%22%7D%7D

weeks <- read_csv("data/first_score_by_week.csv") |>
  filter(G_Score1st == 1) |> # look just at teams that scored first in a game
  group_by(week) |>
  summarize(win_percentage = sum(W) / n())

write_csv(weeks, "weeks_chart.csv")

read_csv("data/first_score_by_week.csv") |>
  filter(G_Score1st == 1,
         week >= 9) |> # look just at teams that scored first in a game
  summarize(win_percentage = sum(W) / n())
