library(tidyverse)
library(rstanarm)
library(bayesplot)
library(kableExtra)

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

batters %>%
  ggplot(aes(x=`Vote Pts`)) +
  geom_histogram()

pitchers %>%
  ggplot(aes(x=`Vote Pts`)) +
  geom_histogram()

batters %>%
  ggplot(aes(x=WAR, y=`Vote Pts`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y="Vote Points", title="Batter's WAR vs MVP Vote Points")

pitchers %>%
  ggplot(aes(x=WAR, y=`Vote Pts`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y="Vote Points", title="Pitcher's WAR vs MVP Vote Points")

batters %>%
  ggplot(aes(x=Position, y=`Vote Pts`)) +
  geom_boxplot() +
  labs(y="Vote Points", title="Batter's Position vs MVP Vote Points")

pitchers %>%
  ggplot(aes(x=Position, y=`Vote Pts`)) +
  geom_boxplot() +
  labs(y="Vote Points", title="Pitcher's Position vs MVP Vote Points")

batter_model <- stan_glm(`Vote Pts` ~ League + Position + COVID + `W-L%`
                         + WAR + AB + OBP + SLG,
                         family = Gamma(link = "log"), data = batters, seed = 440)

plot(batter_model, "trace")
pp_check(batter_model)

batter_results <- as.data.frame(batter_model$stan_summary) %>%
  select("mean", "2.5%", "97.5%")
names(batter_results) <- c("Mean", "Lower Bound", "Upper Bound")
rownames(batter_results) <- c("Intercept", "American League", "Catcher", "Corner Infielder", "Designated Hitter",
                              "Middle Infielder", "COVID Season", "Team's Win %", "WAR",
                              "At Bats", "On Base Percentage", "Slugging Percentage",
                              "shape", "mean_PPD", "log-posterior")
batter_results %>%
  kbl(caption = "MVP Batters Model Results", digits = 3) %>%
  kable_styling(full_width = F)

batter_diagnostics <- as.data.frame(batter_model$stan_summary) %>%
  select("n_eff", "Rhat")
names(batter_diagnostics) <- c("Effective Sample Size", "R hat")
rownames(batter_diagnostics) <- c("Intercept", "American League", "Catcher", "Corner Infielder", "Designated Hitter",
                                  "Middle Infielder", "COVID Season", "Team's Win %", "WAR",
                                  "At Bats", "On Base Percentage", "Slugging Percentage",
                                  "shape", "mean_PPD", "log-posterior")
batter_diagnostics %>%
  kbl(caption = "MVP Batters Model Diagnostics", digits = 3) %>%
  kable_styling(full_width = F)

pitcher_model <-  stan_glm(`Vote Pts` ~ League + Position + COVID + `W-L%`
                          + WAR + IP + ERA + `H&BB`,
                          family = Gamma(link = "log"), data = pitchers, seed = 440)

plot(pitcher_model, "trace")
pp_check(pitcher_model)

pitcher_results <- as.data.frame(pitcher_model$stan_summary) %>%
  select("mean", "2.5%", "97.5%")
names(pitcher_results) <- c("Mean", "Lower Bound", "Upper Bound")
rownames(pitcher_results) <- c("Intercept", "American League", "Reliever", "COVID Season", "Team's Win %",
                               "WAR", "Innings Pitched", "Earned Run Average", "Hits and Walks Allowed",
                              "shape", "mean_PPD", "log-posterior")
pitcher_results %>%
  kbl(caption = "MVP Pitchers Model Results", digits = 3) %>%
  kable_styling(full_width = F)

pitcher_diagnostics <- as.data.frame(pitcher_model$stan_summary) %>%
  select("n_eff", "Rhat")
names(pitcher_diagnostics) <- c("Effective Sample Size", "R hat")
rownames(pitcher_diagnostics) <- c("Intercept", "American League", "Reliever", "COVID Season", "Team's Win %",
                                   "WAR", "Innings Pitched", "Earned Run Average", "Hits and Walks Allowed",
                                   "shape", "mean_PPD", "log-posterior")
pitcher_diagnostics %>%
  kbl(caption = "MVP Pitchers Model Diagnostics", digits = 3) %>%
  kable_styling(full_width = F)

