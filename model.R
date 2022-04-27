library(tidyverse)
library(rstanarm)

batters <- read.csv("batter_mvp.csv", check.names = F)
pitchers <- read.csv("pitcher_mvp.csv", check.names = F)

batters <- batters %>%
  mutate(`W-L%` = 100*`W-L%`) %>%
  mutate(OBP = 100*OBP) %>%
  mutate(SLG = 100*SLG) %>%
  select("Name", "Year", "Tm",
         "Vote Pts", "League", "Primary Position", "COVID",
         "W-L%", "WAR", "AB", "OBP", "SLG")

pitchers <- pitchers %>%
  mutate(Partial = round(as.numeric(IP - floor(IP)), 1))

pitchers <- pitchers %>%
  mutate(IP = case_when(Partial == 0.1 ~ IP - Partial + 0.33,
                         Partial == 0.2 ~ IP - Partial + 0.67,
                         T ~ IP)) %>%
  mutate(`W-L%` = 100*`W-L%`) %>%
  mutate(`H&BB` = round(WHIP*IP, 0)) %>%
  select("Name", "Year", "Tm",
         "Vote Pts", "League", "Primary Position", "COVID",
         "W-L%", "WAR", "IP", "ERA", "H&BB")

names(batters)[6] <- "Position"
names(pitchers)[6] <- "Position"

batters$League <- factor(batters$League)
pitchers$League <- factor(pitchers$League)
batters$Position <- factor(batters$Position)
pitchers$Position <- factor(pitchers$Position)
batters$COVID <- factor(batters$COVID)
pitchers$COVID <- factor(pitchers$COVID)

batters$League = relevel(batters$League, ref = "NL")
pitchers$League = relevel(pitchers$League, ref = "NL")
batters$Position = relevel(batters$Position, ref = "OF")
pitchers$Position = relevel(pitchers$Position, ref = "SP")
batters$COVID = relevel(batters$COVID, ref = "FALSE")
pitchers$COVID = relevel(pitchers$COVID, ref = "FALSE")

batter_model <- stan_glm(`Vote Pts` ~ League + Position + COVID + `W-L%`
                         + WAR + AB + OBP + SLG,
                         family = gaussian(link = "identity"), data = batters, seed = 440)
batter_model$stan_summary

pitcher_model <-  stan_glm(`Vote Pts` ~ League + Position + COVID + `W-L%`
                          + WAR + IP + ERA + `H&BB`,
                          family = gaussian(link = "identity"), data = pitchers, seed = 440)
pitcher_model$stan_summary

