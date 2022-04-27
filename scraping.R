library(baseballr)
library(dplyr)
library(rvest)
library(stringr)

# League Awards Scraping Functions
year_league_award_df <- function (year, league, category) {
  url <- url(paste0('https://www.baseball-reference.com/awards/awards_', year, '.shtml'))
  path <- paste0('//*[@id="',league,'_',category,'_voting"]')
  table <- url %>%
    read_html() %>%
    html_nodes(xpath=path) %>%
    html_table()
  table <- table[[1]]
  table <- unname(table)
  table <- as.data.frame(table)
  cols <- table[1,]
  table <- table[-1,]
  names(table) <- cols
  positions <- c()
  for (row in seq(1, nrow(table), 1)) {
    path2 <- paste0(path, '/tbody/tr[', row, ']/td[1]/a')
    player <- url(paste0('https://www.baseball-reference.com/awards/awards_', year, '.shtml')) %>%
      read_html() %>%
      html_nodes(xpath=path2) %>%
      html_attr("href")
    pos <- url(paste0('https://www.baseball-reference.com', player)) %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="meta"]/div[2]/p[1]') %>%
      html_text()
    position <- str_trim(str_split(pos, "\n")[[1]][3])
    print(position)
    positions <- c(positions, position)
  }
  table$Position <- positions
  return (table)
}

league_award_2004_2021 <- function (league, category) {
  df <- year_league_award_df(2004, league, category)
  rows <- nrow(df)
  df$Year <- rep(2004, rows)
  for (year in seq(2005, 2021, 1)) {
    temp <- year_league_award_df(year, league, category)
    temp_rows <- nrow(temp)
    temp$Year <- rep(year, temp_rows)
    df <- rbind(df, temp)
    Sys.sleep(1)
  }
  return (df)
}

al_mvp <- league_award_2004_2021("AL", "MVP")
nl_mvp <- league_award_2004_2021("NL", "MVP")

names(al_mvp)[8] <- "G (B)"
names(al_mvp)[11] <- "H (B)"
names(al_mvp)[12] <- "HR (B)"
names(al_mvp)[15] <- "BB (B)"
names(al_mvp)[20] <- "W (P)"
names(al_mvp)[21] <- "L (P)"
names(al_mvp)[24] <- "G (P)"
names(al_mvp)[28] <- "H (P)"
names(al_mvp)[29] <- "HR (P)"
names(al_mvp)[30] <- "BB (P)"

names(nl_mvp)[8] <- "G (B)"
names(nl_mvp)[11] <- "H (B)"
names(nl_mvp)[12] <- "HR (B)"
names(nl_mvp)[15] <- "BB (B)"
names(nl_mvp)[20] <- "W (P)"
names(nl_mvp)[21] <- "L (P)"
names(nl_mvp)[24] <- "G (P)"
names(nl_mvp)[28] <- "H (P)"
names(nl_mvp)[29] <- "HR (P)"
names(nl_mvp)[30] <- "BB (P)"

# Fielding Scraping
year_league_fielding_df <- function (year, league) {
  url <- url(paste0('https://www.baseball-reference.com/leagues/majors/', year, '-standard-fielding.shtml'))
  path <- '//*[@id="players_players_standard_fielding_fielding"]'
  table <- url %>%
    read_html() %>%
    html_nodes(xpath=path) %>%
    html_table()
  table <- table[[1]]
  table <- as.data.frame(table) %>%
    filter(Lg == league)
  return (table)
}

fielding_2004_2021 <- function (league) {
  df <- year_league_fielding_df(2004, league)
  rows <- nrow(df)
  df$Year <- rep(2004, rows)
  for (year in seq(2005, 2021, 1)) {
    temp <- year_league_fielding_df(year, league)
    temp_rows <- nrow(temp)
    temp$Year <- rep(year, temp_rows)
    df <- rbind(df, temp)
    Sys.sleep(1)
  }
  return (df %>%
            select("Name", "Age", "Tm", `Pos Summary`, "Year"))
}

al_fielding <- fielding_2004_2021("AL")
nl_fielding <- fielding_2004_2021("NL")

# Standings Scraping Functions
al_standings_2004_2021 <- function () {
  df <- standings_on_date_bref("2004-12-01", "AL East")
  df <- rbind(df, standings_on_date_bref("2004-12-01", "AL Central"))
  df <- rbind(df, standings_on_date_bref("2004-12-01", "AL West"))
  rows <- nrow(df)
  df$Year <- rep(2004, rows)
  
  years <- seq(2005, 2021, 1)
  divisions <- c("AL Central", "AL West")
  for (year in years) {
    date <- paste0(year, "-12-01")
    temp <- standings_on_date_bref(date, "AL East")
    for (div in divisions) {
      temp <- rbind(temp, standings_on_date_bref(date, div))
    }
    temp_rows <- nrow(temp)
    temp$Year <- rep(year, temp_rows)
    df <- rbind(df, temp)
  }
  return (df)
}

nl_standings_2004_2021 <- function () {
  df <- standings_on_date_bref("2004-12-01", "NL East")
  df <- rbind(df, standings_on_date_bref("2004-12-01", "NL Central"))
  df <- rbind(df, standings_on_date_bref("2004-12-01", "NL West"))
  rows <- nrow(df)
  df$Year <- rep(2004, rows)
  
  years <- seq(2005, 2021, 1)
  divisions <- c("NL Central", "NL West")
  for (year in years) {
    date <- paste0(year, "-12-01")
    temp <- standings_on_date_bref(date, "NL East")
    for (div in divisions) {
      temp <- rbind(temp, standings_on_date_bref(date, div))
    }
    temp_rows <- nrow(temp)
    temp$Year <- rep(year, temp_rows)
    df <- rbind(df, temp)
  }
  return (df)
}

al_standings <- al_standings_2004_2021()
nl_standings <- nl_standings_2004_2021()

# Combine MVP and Standings
al_data <- merge(al_mvp, al_standings, by=c("Year", "Tm"), all.x=T)
nl_data <- merge(nl_mvp, nl_standings, by=c("Year", "Tm"), all.x=T)

# Fix Traded AL Players
al_data$Traded <- rep(F, nrow(al_data))
which(is.na(al_data$W))

# Victor Martinez 2009
vm_t1 <- standings_on_date_bref("2009-12-01", "AL Central") %>%
  filter(Tm == "CLE") %>%
  select(`W-L%`) %>%
  pull()
vm_t2 <- standings_on_date_bref("2009-12-01", "AL East") %>%
  filter(Tm == "BOS") %>%
  select(`W-L%`) %>%
  pull()
al_data[166, "Traded"] <- T
al_data[166, "Tm"] <- "CLE/BOS"
al_data[166, "W-L%"] <- (vm_t1 + vm_t2) / 2

# David Price 2015
dp_t1 <- standings_on_date_bref("2015-12-01", "AL Central") %>%
  filter(Tm == "DET") %>%
  select(`W-L%`) %>%
  pull()
dp_t2 <- standings_on_date_bref("2015-12-01", "AL East") %>%
  filter(Tm == "TOR") %>%
  select(`W-L%`) %>%
  pull()
al_data[311, "Traded"] <- T
al_data[311, "Tm"] <- "DET/TOR"
al_data[311, "W-L%"] <- (dp_t1 + dp_t2) / 2

# Justin Upton 2017
ju_t1 <- standings_on_date_bref("2017-12-01", "AL Central") %>%
  filter(Tm == "DET") %>%
  select(`W-L%`) %>%
  pull()
ju_t2 <- standings_on_date_bref("2017-12-01", "AL West") %>%
  filter(Tm == "LAA") %>%
  select(`W-L%`) %>%
  pull()
al_data[356, "Traded"] <- T
al_data[356, "Tm"] <- "DET/LAA"
al_data[356, "W-L%"] <- (ju_t1 + ju_t2) / 2

# Fix Traded NL Players
nl_data$Traded <- rep(F, nrow(nl_data))
which(is.na(nl_data$W))

# Steve Finley 2004
sf_t1 <- standings_on_date_bref("2004-12-01", "NL West") %>%
  filter(Tm == "ARI") %>%
  select(`W-L%`) %>%
  pull()
sf_t2 <- standings_on_date_bref("2004-12-01", "NL West") %>%
  filter(Tm == "LAD") %>%
  select(`W-L%`) %>%
  pull()
nl_data[31, "Traded"] <- T
nl_data[31, "Tm"] <- "ARI/LAD"
nl_data[31, "W-L%"] <- (sf_t1 + sf_t2) / 2

# Carlos Beltran 2011
cb_t1 <- standings_on_date_bref("2011-12-01", "NL East") %>%
  filter(Tm == "NYM") %>%
  select(`W-L%`) %>%
  pull()
cb_t2 <- standings_on_date_bref("2011-12-01", "NL West") %>%
  filter(Tm == "SFG") %>%
  select(`W-L%`) %>%
  pull()
nl_data[221, "Traded"] <- T
nl_data[221, "Tm"] <- "NYM/SFG"
nl_data[221, "W-L%"] <- (cb_t1 + cb_t2) / 2

# Hunter Pence 2011
hp_t1 <- standings_on_date_bref("2011-12-01", "NL Central") %>%
  filter(Tm == "HOU") %>%
  select(`W-L%`) %>%
  pull()
hp_t2 <- standings_on_date_bref("2011-12-01", "NL East") %>%
  filter(Tm == "PHI") %>%
  select(`W-L%`) %>%
  pull()
nl_data[222, "Traded"] <- T
nl_data[222, "Tm"] <- "HOU/PHI"
nl_data[222, "W-L%"] <- (hp_t1 + hp_t2) / 2

# Hunter Pence 2012
hp2_t1 <- standings_on_date_bref("2012-12-01", "NL East") %>%
  filter(Tm == "PHI") %>%
  select(`W-L%`) %>%
  pull()
hp2_t2 <- standings_on_date_bref("2012-12-01", "NL West") %>%
  filter(Tm == "SFG") %>%
  select(`W-L%`) %>%
  pull()
nl_data[253, "Traded"] <- T
nl_data[253, "Tm"] <- "PHI/SFG"
nl_data[253, "W-L%"] <- (hp2_t1 + hp2_t2) / 2

# Max Scherzer 2021
ms_t1 <- standings_on_date_bref("2021-12-01", "NL East") %>%
  filter(Tm == "WSN") %>%
  select(`W-L%`) %>%
  pull()
ms_t2 <- standings_on_date_bref("2021-12-01", "NL West") %>%
  filter(Tm == "LAD") %>%
  select(`W-L%`) %>%
  pull()
nl_data[462, "Traded"] <- T
nl_data[462, "Tm"] <- "WSN/LAD"
nl_data[462, "W-L%"] <- (ms_t1 + ms_t2) / 2

# Trea Turner 2021
tt_t1 <- standings_on_date_bref("2021-12-01", "NL East") %>%
  filter(Tm == "WSN") %>%
  select(`W-L%`) %>%
  pull()
tt_t2 <- standings_on_date_bref("2021-12-01", "NL West") %>%
  filter(Tm == "LAD") %>%
  select(`W-L%`) %>%
  pull()
nl_data[463, "Traded"] <- T
nl_data[463, "Tm"] <- "WSN/LAD"
nl_data[463, "W-L%"] <- (tt_t1 + tt_t2) / 2

which(is.na(al_data$Position))
al_data[99, "Position"] <- "Pitcher"
al_data[370, "Position"] <- "Rightfielder"

which(is.na(nl_data$Position))
nl_data[205, "Position"] <- "Rightfielder"
nl_data[239, "Position"] <- "Rightfielder"
nl_data[294, "Position"] <- "Rightfielder"
nl_data[370, "Position"] <- "Rightfielder"

write.csv(al_data, "al_mvp.csv", row.names = F)
write.csv(nl_data, "nl_mvp.csv", row.names = F)
thing <- read.csv("al_mvp.csv", check.names = F)
