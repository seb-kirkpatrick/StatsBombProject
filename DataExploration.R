library(StatsBombR)
library(tidyverse)

comps <- FreeCompetitions()

szn <- comps |>
  filter(season_id == 26 &
           competition_name == "La Liga")

games <- FreeMatches(szn)

data <- allclean(free_allevents(MatchesDF = games, Parallel=T))

data |>
  filter(possession_team.name == "Barcelona") |>
  group_by(player.name) |>
  summarize(Shots = sum(type.id == 16, na.rm=T),
            Goals = sum(shot.outcome.id == 97, na.rm=T)) |>
  arrange(-Shots)

data |>
  filter(possession_team.name == "Barcelona") |>
  group_by(player.name) |>
  summarize(Shots = sum(type.id == 16, na.rm=T),
            Goals = sum(shot.outcome.id == 97, na.rm=T)) |>
  arrange(-Shots) |>
  top_n(10) |>
  ggplot(
    aes(x=Shots, y=player.name)) +
  geom_bar(stat="identity", width=0.75) +
  labs(y = "Shots") +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous( expand = c(0,0)) +
  coord_flip()
  
            
table(data$shot.outcome.name, data$possession_team.name)           

