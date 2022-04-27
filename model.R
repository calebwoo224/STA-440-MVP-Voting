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

al_data <- al_data %>%
  mutate(is_P = case_when(`Primary Position` == "P" ~ T,
                          `Primary Position` == "DH/P" ~ T,
                          T ~ F)) %>%
  mutate(is_B = case_when(`Primary Position` == "P" ~ F,
                          `Primary Position` == "DH/P" ~ T,
                          T ~ T))

nl_data <- nl_data %>%
  mutate(is_P = case_when(`Primary Position` == "P" ~ T,
                          T ~ F)) %>%
  mutate(is_B = case_when(`Primary Position` == "P" ~ F,
                          T ~ T))

both <- rbind(al_data, nl_data)
both$`Primary Position` <- as.factor(both$`Primary Position`)
both$`Primary Position` <- relevel(both$`Primary Position`, ref = "OF")
levels(both$`Primary Position`)
typeof(both$`Primary Position`)
table(both$`Primary Position`)

model <- stan_glm(`Vote Pts` ~ `W-L%` + `Primary Position` + WAR + is_B + AB + is_P + IP,
                  family = gaussian(link = "identity"), data = both)
model$coefficients
