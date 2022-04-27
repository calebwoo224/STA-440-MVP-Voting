library(tidyverse)
library(rstanarm)
library(stringr)

al_data <- read.csv("al_mvp.csv", check.names = F)
nl_data <- read.csv("nl_mvp.csv", check.names = F)

table(al_data$Position)
al_data %>%
  filter(Position == "First Baseman, Designated Hitter and Leftfielder")
al_data %>%
  filter(Position == "First Baseman, Designated Hitter and Third Baseman")
al_data %>%
  filter(Position == "Leftfielder and Designated Hitter")
al_data %>%
  filter(Position == "Outfielder and Designated Hitter")
al_data %>%
  filter(Position == "Rightfielder and Designated Hitter")

table(nl_data$Position)
nl_data %>%
  filter(Position == "First Baseman, Designated Hitter and Third Baseman")
nl_data %>%
  filter(Position == "Outfielder and Designated Hitter")

convert_positions <- function(primary) {
  if (primary == "Catcher") {
    return("C")
  }
  else if (primary == "Pitcher") {
    return("P")
  }
  else if (primary == "Designated Hitter") {
    return("DH")
  }
  else if (primary == "Outfielder") {
    return("OF")
  }
  else if (primary == "Leftfielder") {
    return("OF")
  }
  else if (primary == "Centerfielder") {
    return("OF")
  }
  else if (primary == "Rightfielder") {
    return("OF")
  }
  else if (primary == "Third Baseman") {
    return("CIF")
  }
  else if (primary == "First Baseman") {
    return("CIF")
  }
  else if (primary == "Shortstop") {
    return("MIF")
  }
  else if (primary == "Second Baseman") {
    return("MIF")
  }
}

al_primary <- c()
for (pos in al_data$Position) {
  if (pos == "Designated Hitter and Pitcher") {
    al_primary <- c(al_primary, "DH/P")
  }
  else if (pos == "First Baseman, Designated Hitter and Leftfielder") {
    al_primary <- c(al_primary, "DH")
  }
  else if (pos == "First Baseman, Designated Hitter and Third Baseman") {
    al_primary <- c(al_primary, "DH")
  }
  else if (pos == "Leftfielder and Designated Hitter") {
    al_primary <- c(al_primary, "DH")
  }
  else if (pos == "Outfielder and Designated Hitter") {
    al_primary <- c(al_primary, "DH")
  }
  else if (pos == "Rightfielder and Designated Hitter") {
    al_primary <- c(al_primary, "DH")
  }
  else {
    if (grepl(",", pos)) {
      primary <- str_split(pos, ", ")[[1]][1]
      al_primary <- c(al_primary, convert_positions(primary))
    }
    else if (grepl("and", pos)) {
      primary <- str_split(pos, " and ")[[1]][1]
      al_primary <- c(al_primary, convert_positions(primary))
    }
    else {
      primary <- pos
      al_primary <- c(al_primary, convert_positions(primary))
    }
  }
}
al_data$`Primary Position` <- al_primary

nl_primary <- c()
for (pos in nl_data$Position) {
  if (grepl(",", pos)) {
    primary <- str_split(pos, ", ")[[1]][1]
    nl_primary <- c(nl_primary, convert_positions(primary))
  }
  else if (grepl("and", pos)) {
    primary <- str_split(pos, " and ")[[1]][1]
    nl_primary <- c(nl_primary, convert_positions(primary))
  }
  else {
    primary <- pos
    nl_primary <- c(nl_primary, convert_positions(primary))
  }
}
nl_data$`Primary Position` <- nl_primary

table(al_data$`Primary Position`)
table(nl_data$`Primary Position`)

al_data$League <- rep("AL", nrow(al_data))
nl_data$League <- rep("NL", nrow(nl_data))

both <- rbind(al_data, nl_data)
both <- rbind(both %>%
  filter(Name == "Shohei Ohtani"), both)

which(both == "Shohei Ohtani", arr.ind = T)
both[1, "Primary Position"] <- "P"
both[1, "WAR"] <- 4.1
both[439, "Primary Position"] <- "DH"
both[439, "WAR"] <- 4.9

batter_data <- both %>%
  filter(`Primary Position` != "P") %>%
  select("Year", "League", "Tm", "Name", "Rank", "Vote Pts",
         "1st Place","Share", "W-L%", "Primary Position",
         "WAR", "AB", "OBP", "SLG")

pitcher_data <- both %>%
  filter(`Primary Position` == "P") %>%
  select("Year", "League", "Tm", "Name", "Rank", "Vote Pts",
         "1st Place","Share", "W-L%", "Primary Position",
         "WAR", "IP", "ERA", "WHIP")

batter_data <- batter_data %>%
  mutate(COVID = case_when(Year == 2020 ~ T,
                           Year != 2020 ~ F))

pitcher_data <- pitcher_data %>%
  mutate(COVID = case_when(Year == 2020 ~ T,
                           Year != 2020 ~ F))

p1 <- pitcher_data %>%
  filter(IP < 130, Year != 2020) %>%
  mutate(`Primary Position` = "RP")

p2 <- pitcher_data %>%
  filter(IP >= 130, Year != 2020) %>%
  mutate(`Primary Position` = "SP")

p3 <- pitcher_data %>%
  filter(Year == 2020, IP < 50) %>%
  mutate(`Primary Position` = "RP")

p4 <- pitcher_data %>%
  filter(Year == 2020, IP > 50) %>%
  mutate(`Primary Position` = "SP")

pitcher_all <- rbind(p1, p2, p3, p4)

write.csv(batter_data, "batter_mvp.csv", row.names = F)
write.csv(pitcher_all, "pitcher_mvp.csv", row.names = F)
