library(StatsBombR)
library(tidyverse)
library(soccermatics)
library(SBpitch)

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

minutes <- get.minutesplayed(data) |>
  group_by(player.id) |>
  summarize(minutes = sum(MinutesPlayed))

shots <- data |>
  filter(possession_team.name == "Barcelona") |>
  group_by(player.name,player.id) |>
  summarize(Shots = sum(type.id == 16, na.rm=T),
            Goals = sum(shot.outcome.id == 97, na.rm=T)) |>
  arrange(-Shots) |>
  head(25)
  
  player_shots <- left_join(shots,minutes,by="player.id") |>
    mutate(shots90 = Shots / (minutes/90)) |>
    filter(shots90 > 0.5)
  
  ggplot(data=player_shots,
    aes(x=reorder(player.name,Shots), y=shots90)) +
  geom_bar(stat="identity", width=0.75) +
  labs(title = "Shots Per Game - 2014/15 La Liga",
       y = "Shots Per 90") +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme_SB()
  
table(data$shot.outcome.name, data$possession_team.name)       

pass <- data |>
  filter(possession_team.name == "Barcelona") |>
  group_by(player.name,player.id) |>
  summarize(Passes = sum(type.id == 30, na.rm=T),
            Completed_Passes = sum(type.id == 30 & is.na(pass.outcome.name)),
            PassPct = Completed_Passes/Passes) |>
  filter(Passes > 100) |>
  arrange(-PassPct)

games <- games |>
  mutate(Result = case_when(
    home_team.home_team_name == "Barcelona" & home_score > away_score ~ "W",
    home_team.home_team_name == "Barcelona" & home_score < away_score ~ "L",
    away_team.away_team_name == "Barcelona" & home_score > away_score ~ "L",
    away_team.away_team_name == "Barcelona" & home_score < away_score ~ "W",
    T ~ "T"
  ))

l <- games |>
  filter(Result == "L")

losses <- allclean(free_allevents(MatchesDF = l, Parallel=T))

loss1 <- losses |>
  filter(match_id == 267301)

loss1l <- get.lineupsFree(loss1)

nick1 <- loss1l[[3]][[1]][c(1,3)]

loss1j <- left_join(loss1,nick1, by= c("player.id" = "player_id"))
loss1j <- loss1j |>
  mutate(player_nickname = ifelse(is.na(player_nickname), player.name, player_nickname))


loss1j %>%
  filter(type.name == "Pass", team.name =="Barcelona", ) |>
  soccerPositionMap(
    id = "player_nickname",
    x = "location.x",
    y = "location.y",
    fill1 = "blue", 
    arrow = "r"
  )

loss1j %>%
  filter(type.name == "Pressure", team.name =="Celta Vigo") |>
  soccerPositionMap(
    id = "player_nickname",
    x = "location.x",
    y = "location.y",
    fill1 = "red", 
    arrow = "l",
    source = "statsbomb"
  )


loss1j |>
  filter(type.name == "Pressure" & team.name == "Barcelona") %>% 
  soccerHeatmap(x = "location.x", 
                y = "location.y",
                xBins = 21,
                yBins = 14)


loss2 <- losses |>
  filter(match_id == 266240)

loss2l <- get.lineupsFree(loss2)

nick2 <- loss2l[[3]][[2]][c(1,3)]

loss2j <- left_join(loss2,nick2, by= c("player.id" = "player_id"))
loss2j <- loss2j |>
  mutate(player_nickname = ifelse(is.na(player_nickname), player.name, player_nickname))


loss2j %>%
  filter(type.name == "Pass", team.name =="Barcelona", ) |>
  soccerPositionMap(
    id = "player_nickname",
    lengthPitch = 105,
    widthPitch = 75,
    x = "location.x",
    y = "location.y",
    fill1 = "blue", 
    arrow = "r"
  )

loss2j |>
  filter(type.name == "Pressure" & team.name == "Barcelona") %>% 
  soccerHeatmap(x = "location.x", 
                y = "location.y",
                xBins = 21,
                yBins = 14)

loss3 <- losses |>
  filter(match_id == 266148)

loss3l <- get.lineupsFree(loss3)

nick3 <- loss3l[[3]][[2]][c(1,3)]

loss3j <- left_join(loss3,nick3, by= c("player.id" = "player_id"))
loss3j <- loss3j |>
  mutate(player_nickname = ifelse(is.na(player_nickname), player.name, player_nickname))


loss3j %>%
  filter(type.name == "Pass", team.name =="Barcelona", ) |>
  soccerPositionMap(
    id = "player_nickname",
    x = "location.x",
    y = "location.y",
    fill1 = "blue", 
    arrow = "r"
  )

loss3j |>
  filter(type.name == "Pressure" & team.name == "Barcelona") %>% 
  soccerHeatmap(x = "location.x", 
                y = "location.y",
                xBins = 21,
                yBins = 14)


loss1j |>
  filter(team.name == "Celta Vigo") %>% 
  soccerPassmap(fill = "red",
                lengthPitch = 105,
                widthPitch = 75,
                minPass = 3,
                arrow="r")
loss1j |>
  filter(team.name == "Barcelona") %>% 
  soccerPassmap(fill = "blue",
                minPass = 3,
                arrow="r")

loss2j |>
  filter(type.name == "Pass" & team.name == "Real Sociedad") %>% 
  soccerHeatmap(x = "location.x", 
                y = "location.y",
                xBins = 21,
                yBins = 14)

loss3j |>
  filter(type.name == "Pass" & team.name == "Málaga") %>% 
  soccerHeatmap(x = "location.x", 
                y = "location.y",
                xBins = 21,
                yBins = 14)

passes1 = loss1j %>%  
  filter(type.name=="Pass" & is.na(pass.outcome.name) & team.name=="Barcelona") %>% 
  filter(pass.end_location.x>=102 & pass.end_location.y<=62 & pass.end_location.y>=18) 

create_Pitch() +
  geom_segment(data = passes1, aes(x = location.x, 
                                  y = location.y, 
                                  xend = pass.end_location.x, 
                                  yend = pass.end_location.y),
               lineend = "round", 
               size = 0.5, colour = "#000000", 
               arrow = arrow(length = unit(0.07, "inches"), 
                             ends = "last", 
                             type = "open")) +
  scale_x_reverse() +
  scale_y_reverse() +
  coord_fixed(ratio = 105/100)
