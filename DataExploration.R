library(StatsBombR)
library(tidyverse)

comps <- FreeCompetitions()

szn <- comps |>
  filter(season_id == 26 &
           competition_name == "La Liga")

games <- FreeMatches(szn)

data <- allclean(free_allevents(MatchesDF = games, Parallel=T))

