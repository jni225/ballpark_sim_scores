### PROJECT: Ballpark Similarity Scores
### AUTHOR: John Incantalupo
### COMPLETED: August 10, 2025
### GitHub Repo: https://github.com/jni225/ballpark_sim_scores

#install.packages("tidyverse")
#install.packages("baseballr")
#install.packages("ggtext")

library(tidyverse)
library(baseballr)
library(ggtext)
#setwd("path") #Set path to the location of the mlbparks.csv and milbparks.csv files and the Logos folder

##### IMPORTANT NOTE: This data has been taken from the 2022-2024 MLB seasons. The data taken
#####   from MLB Gameday refers to these ballparks as what they were called at that time. As an
#####   example, the Astros' home ballpark is referred to as Minute Maid Park. This also applies
#####   to teams that are in new ballparks in 2025, such as the Athletics, whose home ballpark
#####   was still the Oakland Coliseum at that time.

#Load data (see GitHu for access to CSV files)
mlbparks <- read.csv("mlb_parks.csv")
milbparks <- read.csv("milb_parks.csv")

#Acquiring ALL regular season game_pks from 2022 to 2024
map_df(.x = seq.Date(as.Date("2022-01-01"),
                     as.Date("2024-12-31"),
                     'day'),
       ~mlb_game_pks(date = .x, level_ids = 1)) %>%
  filter(seriesDescription == "Regular Season" &
           status.codedGameState == "F" &
           is.na(resumedFrom) &
           venue.name %in% mlbparks$stadium_name) %>%
  mutate(class = "MLB") -> MLBgamepks3yr
map_df(.x = seq.Date(as.Date("2022-01-01"),
                     as.Date("2024-12-31"),
                     'day'),
       ~mlb_game_pks(date = .x, level_ids = 11)) %>%
  filter(seriesDescription == "Regular Season" &
           status.codedGameState == "F" &
           is.na(resumedFrom) &
           venue.name %in% milbparks$stadium_name) %>%
  mutate(class = "AAA") -> AAAgamepks3yr
map_df(.x = seq.Date(as.Date("2022-01-01"),
                     as.Date("2024-12-31"),
                     'day'),
       ~mlb_game_pks(date = .x, level_ids = 12)) %>%
  filter(seriesDescription == "Regular Season" &
           status.codedGameState == "F" &
           is.na(resumedFrom) &
           venue.name %in% milbparks$stadium_name) %>%
  mutate(class = "AA") -> AAgamepks3yr
map_df(.x = seq.Date(as.Date("2022-01-01"),
                     as.Date("2024-12-31"),
                     'day'),
       ~mlb_game_pks(date = .x, level_ids = 13)) %>%
  filter(seriesDescription == "Regular Season" &
           status.codedGameState == "F" &
           is.na(resumedFrom) &
           venue.name %in% milbparks$stadium_name) %>%
  mutate(class = "High-A") -> HighAgamepks3yr
map_df(.x = seq.Date(as.Date("2022-01-01"),
                     as.Date("2024-12-31"),
                     'day'),
       ~mlb_game_pks(date = .x, level_ids = 14)) %>%
  filter(seriesDescription == "Regular Season" &
           status.codedGameState == "F" &
           is.na(resumedFrom) &
           venue.name %in% milbparks$stadium_name) %>%
  mutate(class = "Low-A") -> LowAgamepks3yr
rbind(MLBgamepks3yr, AAAgamepks3yr, AAgamepks3yr,
      HighAgamepks3yr, LowAgamepks3yr)[,c(7:72)] -> Allgamepks3yr

#Fixing teams who changed names during this time period
for (i in 1:length(Allgamepks3yr$teams.home.team.id)) {
  if(Allgamepks3yr$teams.home.team.id[i] == 432)
    Allgamepks3yr$teams.home.team.name[i] <- "Rome Emperors"
  if(Allgamepks3yr$teams.away.team.id[i] == 432)
    Allgamepks3yr$teams.away.team.name[i] <- "Rome Emperors"
  if(Allgamepks3yr$teams.home.team.id[i] == 238)
    Allgamepks3yr$teams.home.team.name[i] <- "Oklahoma City Baseball Club"
  if(Allgamepks3yr$teams.away.team.id[i] == 238)
    Allgamepks3yr$teams.away.team.name[i] <- "Oklahoma City Baseball Club"
}

#Creating the base data frame that all PBP data will be added to
mlb_pbp(game_pk = 567012) %>%
  filter(isPitch == TRUE) %>%
  group_by(game_pk, atBatIndex) %>%
  summarize(result = first(result.event)) %>%
  filter(atBatIndex < 0) -> total_pbp

#Scraping PBP data from MLB Gameday (WARNING: This will take hours)
for (i in 1:length(Allgamepks3yr$game_pk)) {
  rbind(total_pbp,
        (mlb_pbp(game_pk = Allgamepks3yr$game_pk[[i]]) %>%
           filter(isPitch == TRUE) %>%
           group_by(game_pk, atBatIndex) %>%
           summarize(result = first(result.event)))) -> total_pbp
  print(i)
}

#Filtering out non-finished PAs (Ex: Caught stealing to end inning)
total_pbp %>%
  filter(!result %in% c("Catcher Interference", "Caught Stealing 2B",
                        "Caught Stealing 3B", "Caught Stealing Home",
                        "Field Error", "Hit By Pitch", "Intent Walk",
                        "Pickoff 1B", "Pickoff 2B", "Pickoff 3B",
                        "Pickoff Caught Stealing 2B",
                        "Pickoff Caught Stealing 3B",
                        "Pickoff Caught Stealing Home",
                        "Runner Out", "Sac Bunt", "Stolen Base 2B",
                        "Wild Pitch")) -> total_pbp
total_pbp$result <- ifelse(total_pbp$result %in% c("Strikeout", "Walk", "Single",
                                                   "Double", "Triple", "Home Run"),
                           total_pbp$result, "Out")
total_pbp$atBatIndex <- as.numeric(total_pbp$atBatIndex)

#Integrating team, class, and venue info into PBP data
merge(total_pbp, (Allgamepks3yr %>%
                    select(game_pk, class, venue.id, venue.name,
                           teams.home.team.id, teams.home.team.name,
                           teams.away.team.id, teams.away.team.name)),
      by = "game_pk", all.x = TRUE) -> total_pbp

#Fixing teams who changed names during this time period
for (i in 1:length(total_pbp$teams.home.team.id)) {
  if(total_pbp$teams.home.team.id[i] == 432)
    total_pbp$teams.home.team.name[i] <- "Rome Emperors"
  if(total_pbp$teams.away.team.id[i] == 432)
    total_pbp$teams.away.team.name[i] <- "Rome Emperors"
  if(total_pbp$teams.home.team.id[i] == 238)
    total_pbp$teams.home.team.name[i] <- "Oklahoma City Baseball Club"
  if(total_pbp$teams.away.team.id[i] == 238)
    total_pbp$teams.away.team.name[i] <- "Oklahoma City Baseball Club"
}



#### CALCULATING PARK FACTORS

#Hit rates were calculated as a proportion of batted balls, while strikeout and walk rates were calculated as a proportion of all PAs
total_pbp %>%
  filter(halfInning == "bottom") %>%
  group_by(teams.home.team.name) %>%
  summarize(class = first(class),
            home_SO_rate = sum(result == "Strikeout") / n(),
            home_BB_rate = sum(result == "Walk") / n(),
            home_1B_rate = sum(result == "Single") / (n() - sum(result %in% c("Strikeout", "Walk"))),
            home_2B_rate = sum(result == "Double") / (n() - sum(result %in% c("Strikeout", "Walk"))),
            home_3B_rate = sum(result == "Triple") / (n() - sum(result %in% c("Strikeout", "Walk"))),
            home_HR_rate = sum(result == "Home Run") / (n() - sum(result %in% c("Strikeout", "Walk")))) -> home_factors

total_pbp %>%
  filter(halfInning == "top") %>%
  group_by(teams.away.team.name) %>%
  summarize(away_SO_rate = sum(result == "Strikeout") / n(),
            away_BB_rate = sum(result == "Walk") / n(),
            away_1B_rate = sum(result == "Single") / (n() - sum(result %in% c("Strikeout", "Walk"))),
            away_2B_rate = sum(result == "Double") / (n() - sum(result %in% c("Strikeout", "Walk"))),
            away_3B_rate = sum(result == "Triple") / (n() - sum(result %in% c("Strikeout", "Walk"))),
            away_HR_rate = sum(result == "Home Run") / (n() - sum(result %in% c("Strikeout", "Walk")))) -> away_factors


merge(home_factors, away_factors,
      by.x = "teams.home.team.name",
      by.y = "teams.away.team.name") %>%
  mutate(bhf_SO = home_SO_rate / away_SO_rate,
         bhf_BB = home_BB_rate / away_BB_rate,
         bhf_1B = home_1B_rate / away_1B_rate,
         bhf_2B = home_2B_rate / away_2B_rate,
         bhf_3B = home_3B_rate / away_3B_rate,
         bhf_HR = home_HR_rate / away_HR_rate) -> bhf_calc

#Creating road factors by calculating schedule percentages
Allgamepks3yr %>%
  group_by(teams.home.team.name, teams.away.team.name) %>%
  summarize(n = n()) %>%
  merge((Allgamepks3yr %>%
           group_by(teams.away.team.name) %>%
           summarize(away_games = n())),
        by = "teams.away.team.name", all.x = TRUE) %>%
  mutate(pct = n/away_games) %>%
  merge((bhf_calc %>%
           select(teams.home.team.name, bhf_SO, bhf_BB,
                  bhf_1B, bhf_2B, bhf_3B, bhf_HR)),
        by = "teams.home.team.name", all.x = TRUE) %>%
  mutate(brf_SO_ind = pct * bhf_SO,
         brf_BB_ind = pct * bhf_BB,
         brf_1B_ind = pct * bhf_1B,
         brf_2B_ind = pct * bhf_2B,
         brf_3B_ind = pct * bhf_3B,
         brf_HR_ind = pct * bhf_HR) %>%
  group_by(teams.away.team.name) %>%
  summarize(brf_SO = sum(brf_SO_ind),
            brf_BB = sum(brf_BB_ind),
            brf_1B = sum(brf_1B_ind),
            brf_2B = sum(brf_2B_ind),
            brf_3B = sum(brf_3B_ind),
            brf_HR = sum(brf_HR_ind)) -> brf_calc

#Combining away and home factors
merge(bhf_calc, brf_calc,
      by.x = "teams.home.team.name",
      by.y = "teams.away.team.name") %>%
  mutate(iPF_SO = (bhf_SO + brf_SO)/2,
         iPF_BB = (bhf_BB + brf_BB)/2,
         iPF_1B = (bhf_1B + brf_1B)/2,
         iPF_2B = (bhf_2B + brf_2B)/2,
         iPF_3B = (bhf_3B + brf_3B)/2,
         iPF_HR = (bhf_HR + brf_HR)/2) -> pf_calc

#Calculating average park factor by class so that 100 is the average park factor
pf_calc %>% group_by(class) %>% 
  summarize(aPF_SO = mean(iPF_SO),
            aPF_BB = mean(iPF_BB),
            aPF_1B = mean(iPF_1B),
            aPF_2B = mean(iPF_2B),
            aPF_3B = mean(iPF_3B),
            aPF_HR = mean(iPF_HR)) -> aPF

#Dividing by average park factor to get our final park factors
pf_calc %>%
  merge(aPF, by = "class", all.x = TRUE) %>%
  mutate(pf_SO = (iPF_SO/aPF_SO)*100,
         pf_BB = (iPF_BB/aPF_BB)*100,
         pf_1B = (iPF_1B/aPF_1B)*100,
         pf_2B = (iPF_2B/aPF_2B)*100,
         pf_3B = (iPF_3B/aPF_3B)*100,
         pf_HR = (iPF_HR/aPF_HR)*100) -> pf_calc

merge(pf_calc,
      rbind(mlbparks[,c("team", "stadium_name")], milbparks[,c("team", "stadium_name")]),
      by.x = "teams.home.team.name", by.y = "team", all.x = TRUE) -> pf_calc

#Averaging out park factors for Roger Dean Chevrolet Stadium, the only stadium used by two affiliated teams in 2024
pf_calc %>%
  group_by(stadium_name) %>%
  summarize(PF_SO = mean(pf_SO),
            PF_BB = mean(pf_BB),
            PF_1B = mean(pf_1B),
            PF_2B = mean(pf_2B),
            PF_3B = mean(pf_3B),
            PF_HR = mean(pf_HR)) -> pf_calc

#Adding park factors to park dimensions data frame
merge(mlbparks, pf_calc, by = "stadium_name", all.x = TRUE) -> mlbparks
merge(milbparks, pf_calc, by = "stadium_name", all.x = TRUE) -> milbparks


#Standardizing all variables
rbind(mlbparks, milbparks) -> std_parks

std_parks %>%
  group_by(team) %>%
  summarize(stadium_name = first(stadium_name),
            affiliate_id = first(affiliate_id),
            class = first(class),
            PF_SO = (PF_SO - 100) / sd(std_parks$PF_SO),
            PF_BB = (PF_BB - 100) / sd(std_parks$PF_BB),
            PF_1B = (PF_1B - 100) / sd(std_parks$PF_1B),
            PF_2B = (PF_2B - 100) / sd(std_parks$PF_2B),
            PF_3B = (PF_3B - 100) / sd(std_parks$PF_3B),
            PF_HR = (PF_HR - 100) / sd(std_parks$PF_HR),
            lf_fh = (lf_fh - mean(std_parks$lf_fh)) / sd(std_parks$lf_fh),
            cf_fh = (cf_fh - mean(std_parks$cf_fh)) / sd(std_parks$cf_fh),
            rf_fh = (rf_fh - mean(std_parks$rf_fh)) / sd(std_parks$rf_fh),
            lf_dist = (lf_dist - mean(std_parks$lf_dist)) / sd(std_parks$lf_dist),
            lc_dist = (lc_dist - mean(std_parks$lc_dist)) / sd(std_parks$lc_dist),
            cf_dist = (cf_dist - mean(std_parks$cf_dist)) / sd(std_parks$cf_dist),
            rc_dist = (rc_dist - mean(std_parks$rc_dist)) / sd(std_parks$rc_dist),
            rf_dist = (rf_dist - mean(std_parks$rf_dist)) / sd(std_parks$rf_dist),
            elevation = (elevation - mean(std_parks$elevation)) / sd(std_parks$elevation),
            fair_territory = (fair_territory - mean(std_parks$fair_territory)) / sd(std_parks$fair_territory)) -> std_parks

std_parks %>% filter(class == "MLB") -> std_parks_MLB
std_parks %>% filter(class != "MLB") -> std_parks_MiLB



#### CALCULATING SIMILARITY SCORES

#Sim score component functions

#Park factors
sim_score_PFs <- function(a, b) {
  sqrt((std_parks$PF_SO[a] - std_parks$PF_SO[b])^2 +
         (std_parks$PF_BB[a] - std_parks$PF_BB[b])^2 +
         (std_parks$PF_1B[a] - std_parks$PF_1B[b])^2 +
         (std_parks$PF_2B[a] - std_parks$PF_2B[b])^2 +
         (std_parks$PF_3B[a] - std_parks$PF_3B[b])^2 +
         (std_parks$PF_HR[a] - std_parks$PF_HR[b])^2)
}

#Outfield dimensions
sim_score_dims <- function(a, b) {
  sqrt((std_parks$lf_fh[a] - std_parks$lf_fh[b])^2 +
         (std_parks$cf_fh[a] - std_parks$cf_fh[b])^2 +
         (std_parks$rf_fh[a] - std_parks$rf_fh[b])^2 +
         (std_parks$lf_dist[a] - std_parks$lf_dist[b])^2 +
         (std_parks$lc_dist[a] - std_parks$lc_dist[b])^2 +
         (std_parks$cf_dist[a] - std_parks$cf_dist[b])^2 +
         (std_parks$rc_dist[a] - std_parks$rc_dist[b])^2 +
         (std_parks$rf_dist[a] - std_parks$rf_dist[b])^2)
}

#Elevation and fair territory
sim_score_misc <- function(a, b) {
  sqrt((std_parks$elevation[a] - std_parks$elevation[b])^2 +
         (std_parks$fair_territory[a] - std_parks$fair_territory[b])^2)
}

#Creating a 3D array for each component
simscore_matrix <- array(0, dim = c(30, 30, 4), dimnames = list(std_parks_MLB$team, std_parks_MLB$team, c("PFs", "Dims", "Misc", "Total")))

for (i in 1:30) {
  for (j in 1:30) {
    simscore_matrix[i, j, "PFs"] <- sim_score_PFs(which(std_parks$team == std_parks_MLB$team[i]),
                                                  which(std_parks$team == std_parks_MLB$team[j]))
    simscore_matrix[i, j, "Dims"] <- sim_score_dims(which(std_parks$team == std_parks_MLB$team[i]),
                                                    which(std_parks$team == std_parks_MLB$team[j]))
    simscore_matrix[i, j, "Misc"] <- sim_score_misc(which(std_parks$team == std_parks_MLB$team[i]),
                                                    which(std_parks$team == std_parks_MLB$team[j]))
    simscore_matrix[i, j, "Total"] <- 6*simscore_matrix[i, j, "PFs"] + 3*simscore_matrix[i, j, "Dims"] + simscore_matrix[i, j, "Misc"]
  }
}

#Converting to a data frame
data.frame(team1 = rep(std_parks_MLB$team, times = 30),
           team2 = rep(std_parks_MLB$team, each = 30),
           PF_sim = as.vector(simscore_matrix[,,"PFs"]),
           dims_sim = as.vector(simscore_matrix[,,"Dims"]),
           misc_sim = as.vector(simscore_matrix[,,"Misc"]),
           sim_score = as.vector(simscore_matrix[,,"Total"])) -> simscore_df

#Smallest and largest sim scores by team
simscore_df %>%
  filter(team1 != team2) %>%
  group_by(team1) %>%
  summarize(avg_sim = mean(sim_score),
            closest_comp = team2[which(sim_score == min(sim_score))],
            closest_score = min(sim_score),
            biggest_diff = team2[which(sim_score == max(sim_score))],
            biggest_score = max(sim_score)) -> simscore_teams



#### TOTAL AFFILIATE SCORE

#Creating affiliate data frame
mlbparks[,c("affiliate_id", "team", "stadium_name")] %>%
  rename(MLB_team = team, MLB_stadium = stadium_name) %>%
  merge((milbparks %>% filter(class == "AAA"))[c("affiliate_id", "team", "stadium_name")], by = "affiliate_id") %>%
  rename(AAA_team = team, AAA_stadium = stadium_name) %>%
  merge((milbparks %>% filter(class == "AA"))[c("affiliate_id", "team", "stadium_name")], by = "affiliate_id") %>%
  rename(AA_team = team, AA_stadium = stadium_name) %>%
  merge((milbparks %>% filter(class == "High-A"))[c("affiliate_id", "team", "stadium_name")], by = "affiliate_id") %>%
  rename(HighA_team = team, HighA_stadium = stadium_name) %>%
  merge((milbparks %>% filter(class == "Low-A"))[c("affiliate_id", "team", "stadium_name")], by = "affiliate_id") %>%
  rename(LowA_team = team, LowA_stadium = stadium_name) %>%
  mutate(AAA_PF = 0, AAA_dim = 0, AAA_misc = 0, AA_PF = 0, AA_dim = 0, AA_misc = 0,
         HighA_PF = 0, HighA_dim = 0, HighA_misc = 0, LowA_PF = 0, LowA_dim = 0, LowA_misc = 0) -> affiliate_sim

#Calculating sim score components between MLB teams and their affiliates
for (i in 1:length(affiliate_sim$affiliate_id)) {
  j <- which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i])
  affiliate_sim$AAA_PF[i] <- sim_score_PFs(which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i]),
                                           which(milbparks$affiliate_id == affiliate_sim$affiliate_id[i] & milbparks$class == "AAA"))
  affiliate_sim$AAA_dim[i] <- sim_score_dims(which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i]),
                                             which(milbparks$affiliate_id == affiliate_sim$affiliate_id[i] & milbparks$class == "AAA"))
  affiliate_sim$AAA_misc[i] <- sim_score_misc(which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i]),
                                              which(milbparks$affiliate_id == affiliate_sim$affiliate_id[i] & milbparks$class == "AAA"))
  affiliate_sim$AA_PF[i] <- sim_score_PFs(which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i]),
                                          which(milbparks$affiliate_id == affiliate_sim$affiliate_id[i] & milbparks$class == "AA"))
  affiliate_sim$AA_dim[i] <- sim_score_dims(which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i]),
                                            which(milbparks$affiliate_id == affiliate_sim$affiliate_id[i] & milbparks$class == "AA"))
  affiliate_sim$AA_misc[i] <- sim_score_misc(which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i]),
                                             which(milbparks$affiliate_id == affiliate_sim$affiliate_id[i] & milbparks$class == "AA"))
  affiliate_sim$HighA_PF[i] <- sim_score_PFs(which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i]),
                                             which(milbparks$affiliate_id == affiliate_sim$affiliate_id[i] & milbparks$class == "High-A"))
  affiliate_sim$HighA_dim[i] <- sim_score_dims(which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i]),
                                               which(milbparks$affiliate_id == affiliate_sim$affiliate_id[i] & milbparks$class == "High-A"))
  affiliate_sim$HighA_misc[i] <- sim_score_misc(which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i]),
                                                which(milbparks$affiliate_id == affiliate_sim$affiliate_id[i] & milbparks$class == "High-A"))
  affiliate_sim$LowA_PF[i] <- sim_score_PFs(which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i]),
                                            which(milbparks$affiliate_id == affiliate_sim$affiliate_id[i] & milbparks$class == "Low-A"))
  affiliate_sim$LowA_dim[i] <- sim_score_dims(which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i]),
                                              which(milbparks$affiliate_id == affiliate_sim$affiliate_id[i] & milbparks$class == "Low-A"))
  affiliate_sim$LowA_misc[i] <- sim_score_misc(which(mlbparks$affiliate_id == affiliate_sim$affiliate_id[i]),
                                               which(milbparks$affiliate_id == affiliate_sim$affiliate_id[i] & milbparks$class == "Low-A"))
}

#Calculating total sim score between MLB teams and their affiliates
affiliate_sim %>% mutate(AAA_sim = 6*AAA_PF + 3*AAA_dim + AAA_misc,
                         AA_sim = 6*AA_PF + 3*AA_dim + AA_misc,
                         HighA_sim = 6*HighA_PF + 3*HighA_dim + HighA_misc,
                         LowA_sim = 6*LowA_PF + 3*LowA_dim + LowA_misc) -> affiliate_sim

### CALCULATING TOTAL AFFILIATE SCORE (sum of sim scores between the 4 affiliates)
affiliate_sim %>%
  mutate(total_affil_score = AAA_sim + AA_sim + HighA_sim + LowA_sim) -> affiliate_sim



#### GRAPHICS

#Adding logos
MLBlogos <- paste("<img src='Logos/", (mlbparks %>% arrange(team))$affiliate_id, ".png' width='15' height='15'/>", sep = "")

#Eliminating duplicate pairings for graphics
simscore_df %>% filter(team1 != team2) %>% mutate(duplicate = FALSE) -> simscore_unique
for (i in 1:(length(simscore_unique$team1)-1)) {
  for(j in (i+1):length(simscore_unique$team1)) {
    if(simscore_unique$team2[i] == simscore_unique$team1[j] &
       simscore_unique$team1[i] == simscore_unique$team2[j]) {
      simscore_unique$duplicate[i] <- TRUE
    }
  }
}
simscore_unique %>% filter(duplicate == FALSE) %>%
  select(team1, team2, PF_sim, dims_sim, misc_sim, sim_score) -> simscore_unique

#MLB sim score histogram
ggplot(simscore_unique, aes(x = sim_score)) +
  geom_histogram(bins = 15, binwidth = 4, fill = "midnightblue", color = "skyblue") +
  scale_x_continuous(breaks = seq(10, 70, 20)) + scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Similarity Score", y = "Count") +
  geom_vline(xintercept = mean(simscore_unique$sim_score), linetype = "dashed", color = "#FF0000") +
  annotate("label", label = "Mean Sim Score = 30.701", x = mean(simscore_unique$sim_score) + 9,
           y = 80, vjust = -0.5, color = "#FF0000") + theme_classic()
ggsave("simscore_hist.png", width = 8, height = 5)

#Sim score heatmap
ggplot((simscore_df %>% mutate(across(c(PF_sim, dims_sim, misc_sim, sim_score), ~ na_if(., 0)))),
       aes(x = team1, y = team2, fill = sim_score)) + geom_tile() +
  scale_fill_gradient2(breaks = c(min(simscore_df$sim_score[simscore_df$sim_score > 0]) + 10,
                                  max(simscore_df$sim_score[simscore_df$sim_score > 0]) - 10),
                       labels = c("More Similar", "Less Similar"),
                       mid = "#FF0000", high = "#FFCC00", na.value = "#FFFFFF",
                       midpoint = min(simscore_df$sim_score)) + coord_fixed() +
  guides(fill = guide_colorbar(title = "Similarity Score", ticks = FALSE, reverse = TRUE)) +
  scale_x_discrete(labels = MLBlogos) + scale_y_discrete(labels = MLBlogos) +
  theme(axis.text.x = element_markdown(), axis.text.y = element_markdown(),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title = "2024 MLB Ballpark Similarity Scores")
ggsave("simscore_heatmap.png", width = 8.5, height = 8.5)

#Heatmap w/o title
ggplot((simscore_df %>% mutate(across(c(PF_sim, dims_sim, misc_sim, sim_score), ~ na_if(., 0)))),
       aes(x = team1, y = team2, fill = sim_score)) + geom_tile() +
  scale_fill_gradient2(breaks = c(min(simscore_df$sim_score[simscore_df$sim_score > 0]) + 10,
                                  max(simscore_df$sim_score[simscore_df$sim_score > 0]) - 10),
                       labels = c("More Similar", "Less Similar"),
                       mid = "#FF0000", high = "#FFCC00", na.value = "#FFFFFF",
                       midpoint = min(simscore_df$sim_score)) + coord_fixed() +
  guides(fill = guide_colorbar(title = "Similarity Score", ticks = FALSE, reverse = TRUE)) +
  scale_x_discrete(labels = MLBlogos) + scale_y_discrete(labels = MLBlogos) +
  theme(axis.text.x = element_markdown(), axis.text.y = element_markdown(),
        axis.title.x = element_blank(), axis.title.y = element_blank())
ggsave("simscore_heatmap_wo_title.png", width = 8, height = 8)

#Average affiliate sim score by minor league class
data.frame(class = factor(c("AAA", "AA", "High-A", "Low-A"),
                          levels = c("AAA", "AA", "High-A", "Low-A")),
           avg_sim = c(mean(affiliate_sim$AAA_sim),
                       mean(affiliate_sim$AA_sim),
                       mean(affiliate_sim$HighA_sim),
                       mean(affiliate_sim$LowA_sim))) %>%
  ggplot(aes(x = class, y = avg_sim)) + geom_bar(stat = "identity", color = "grey74") +
  geom_text(aes(label = round(avg_sim, digits = 3)), vjust = -0.5) +
  labs(title = "Average Similarity Score by Minor League Level", x = "Class", y = "Avg Sim Score") +
  coord_cartesian(ylim = c(25, 35)) +
  geom_hline(yintercept = sum(simscore_df$sim_score)/870,
             linetype = "dashed", color = "#FF0000") +
  annotate("label", label = "MLB Avg (30.701)", x = 0.9, y = mean(simscore_unique$sim_score) - 0.1,
           vjust = -0.5, color = "#FF0000") + theme_classic()
ggsave("affil_simscore_bar.png", width = 7, height = 4)

#Average MLB sim score vs Total Affiliate Score
simscore_teams %>%
  select(team1, avg_sim) %>% arrange(team1) %>%
  merge(affiliate_sim, by.x = "team1", by.y = "MLB_team") %>%
  mutate(logo = MLBlogos) %>%
  ggplot(aes(x = avg_sim, y = total_affil_score, label = logo)) +
  geom_richtext(fill = NA, label.color = NA) +
  labs(x = "Avg MLB Sim Score", y = "Total Affiliate Score") +
  annotate("label", label = "R^2 = 0.5584", x = 30, y = 275, vjust = -0.5, color = "#000000") +
  theme_classic()
ggsave("team_scatter.png", width = 7, height = 5)


#Saving data frames with MLB team sim scores, MLB team comparisons, and affiliate sim scores
write.csv(simscore_df, "mlb_simscores.csv", row.names = FALSE)
write.csv(simscore_teams, "simscore_comp.csv", row.names = FALSE)
write.csv(affiliate_sim, "affiliate_simscores.csv", row.names = FALSE)
#NOTE: These three data frames are already available on the GitHub Repository under the 'Results' folder