#### LOADING/INSTALLING PACKAGES ####
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("ggimage")
library(ggimage)
#install.packages("stringr")
library(stringr)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggtext")
library(ggtext)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("tidyr")
library(tidyr)
#install.packages("ggrepel")
library(ggrepel)
#install.packages("gt")
library(gt)
#install.packages("gtExtras")
library(gtExtras)
#install.packages("scales")
library(scales)
#install.packages("cfbfastR")
library(cfbfastR)
#install.packages("cfbplotR")
library(cfbplotR)
#install.packages("extrafont")
library(extrafont)

font_import()

options(scipen = 999)

Sys.setenv(CFBD_API_KEY = "") # INSERT YOUR CFBD API KEY HERE ; GET ONE - https://collegefootballdata.com/key

#### INPUT TO SELECT ####
fbsTeams <- load_cfb_teams(fbs_only = T) %>%
  filter(classification == "fbs") %>% 
  summarise(school) %>% 
  pull(school) # selects 1 team

seasons <- 2014:2023 # select 1 season

#### INPUT ####
fPosTeam <- c("Purdue") # Boiler Up!
fSeasons <- c(2023) # 2014 and later

#### Initial ####
fPosTeamWMascotText <- load_cfb_teams(fbs_only = T) %>% 
  filter(classification == "fbs") %>% 
  filter(school %in% fPosTeam) %>%
  mutate(fPosTeamWMascotText = paste0(school, " ", mascot)) %>% 
  summarise(fPosTeamWMascotText) %>% 
  pull(fPosTeamWMascotText)

fPosTeamColor <- load_cfb_teams(fbs_only = T) %>% 
  filter(classification == "fbs") %>% 
  filter(school %in% fPosTeam) %>%
  summarise(color) %>% 
  pull(color)

fPosTeamAltColor <- load_cfb_teams(fbs_only = T) %>% 
  filter(classification == "fbs") %>% 
  filter(school %in% fPosTeam) %>%
  summarise(alt_color) %>% 
  pull(alt_color)

fConf <- load_cfb_teams(fbs_only = T) %>% 
  filter(classification == "fbs") %>% 
  summarise(school, conference) %>% 
  filter(school %in% fPosTeam) %>% 
  select(conference) %>% 
  pull(conference)

fConfAvgText <- paste0(fConf, " Average")

#### OVERALL DATA ####

tableCaption <- "Ethan Sterbis | @EthanSterbis on X | data via cfbfastR"
captionTextCFBD <- "Ethan Sterbis | @EthanSterbis on X | data via cfbfastR & CFBD"

# Custom color palette
color_palette <- c("#37BF00", "#E8E8E8", "#CA0000")
color_palette_reverse <- c("#CA0000", "#E8E8E8", "#37BF00")
color_palette_fPosTeam <- c(fPosTeamAltColor, "#ffffff", fPosTeamColor)

captionText <- "Ethan Sterbis | @EthanSterbis on X | data via cfbfastR"

sterb_analytics_theme <- function(..., base_size = 12) {
  
  theme(
    text = element_text(family = "Bahnschrift", size = base_size),
    axis.ticks = element_blank(),
    axis.title = element_text(color = "black",
                              face = "bold"),
    axis.text = element_text(color = "black",
                             face = "bold",
                             size = base_size),
    plot.title.position = "plot",
    plot.title = element_text(size = base_size * 1.52,
                              face = "bold",
                              color = "black",
                              vjust = .02,
                              hjust = 0.08),
    plot.subtitle = element_text(size = base_size * 1.08,
                                 color = "black",
                                 hjust = 0.08),
    plot.caption = element_text(size = base_size,
                                color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#cccccc"),
    panel.background = element_rect(fill = "#f8f8f8"),
    plot.background = element_rect(fill = "#ffffff"),
    panel.border = element_blank())
}

fbsTeamsColorsLogos <- load_cfb_teams(fbs_only = T) %>% 
  filter(classification == "fbs") %>% 
  select(team_id, school, mascot, abbreviation, conference,
         color, alt_color, logo, logo_2)

pbp <- cfbfastR::load_cfb_pbp(seasons = fSeasons) %>% 
  filter(home %in% fbsTeams | away %in% fbsTeams)

playerUsage <- cfbd_player_usage(year = fSeasons, excl_garbage_time = T) %>% 
  filter(team %in% fbsTeams) %>%
  ungroup() %>% 
  mutate(pctile_usg_overall = percent_rank(usg_overall)) %>% 
  group_by(position) %>% 
  mutate(pctile_pos_usg = percent_rank(usg_overall))



#### DOWN AND DISTANCE ####

dnd <- pbp %>%
  filter(wp_before >= 0.10 & wp_before <= 0.90) %>% 
  filter(!is.na(down), !is.na(distance)) %>% 
  mutate(
    `Down & Distance` = case_when(
      down == 1 & distance == 10 ~ '1st & 10',
      down == 1 & distance > 10 ~ '1st and 11+',
      down == 2 & (distance == 1 | distance == 2) ~ '2nd & 1-2',
      down == 2 & distance >= 3 & distance <= 6 ~ '2nd & 3-6',
      down == 2 & distance >= 7 & distance <= 9 ~ '2nd & 7-9',
      down == 2 & distance >= 10 ~ '2nd & 10+',
      down == 3 & (distance == 1 | distance == 2) ~ '3rd & 1-2',
      down == 3 & distance >= 3 & distance <= 6 ~ '3rd & 3-6',
      down == 3 & distance >= 7 & distance <= 9 ~ '3rd & 7-9',
      down == 3 & distance >= 10 ~ '3rd & 10+',
      TRUE ~ '4th Down'
    )
  )

dnd$`Down & Distance` <- factor(dnd$`Down & Distance`, levels = c('1st & 10', '1st & 11+',
                                                                  '2nd & 1-2',
                                                                  '2nd & 3-6', '2nd & 7-9', '2nd & 10+',
                                                                  '3rd & 1-2', '3rd & 3-6',
                                                                  '3rd & 7-9', '3rd & 10+',
                                                                  '4th Down'))

##### dnd rushing #####

dndRush <- dnd %>%
  filter(pos_team %in% fbsTeams) %>% 
  filter(rush == 1)

dndRushfPosTeam <- dndRush %>% 
  filter(!is.na(`Down & Distance`)) %>% 
  filter(pos_team %in% fPosTeam) %>% 
  group_by(pos_team, `Down & Distance`) %>% 
  summarise(plays_dd_thresh = n(),
            success_rate = sum(success) / plays_dd_thresh,
            epa = mean(EPA, na.rm = T))

dndRushfConf <- dndRush %>% 
  filter(!is.na(`Down & Distance`)) %>% 
  filter(offense_conference %in% fConf) %>% 
  group_by(`Down & Distance`) %>% 
  summarise(plays_dd_thresh = n(),
            success_rate = sum(success) / plays_dd_thresh,
            epa = mean(EPA, na.rm = T)) %>% 
  mutate(pos_team = paste0(fConf, " Average")) %>% 
  select(pos_team, everything())

dndRushOVR <- dndRush %>% 
  filter(!is.na(`Down & Distance`)) %>% 
  group_by(`Down & Distance`) %>% 
  summarise(plays_dd_thresh = n(),
            success_rate = sum(success) / plays_dd_thresh,
            epa = mean(EPA, na.rm = T)) %>% 
  mutate(pos_team = paste0("FBS Average")) %>% 
  select(pos_team, everything())

dndRushRks <- dndRush %>% 
  filter(!is.na(`Down & Distance`)) %>%
  filter(pos_team %in% fbsTeams) %>% 
  group_by(pos_team, `Down & Distance`) %>% 
  summarise(plays_dd_thresh = n(),
            success_rate = sum(success) / plays_dd_thresh,
            epa = mean(EPA, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(`Down & Distance`) %>%
  summarise(pos_team,
            rk_plays_dd_thresh = round(rank(-plays_dd_thresh), digits = 0),
            rk_success_rate = round(rank(-success_rate), digits = 0),
            rk_epa = round(rank(-epa), digits = 0)) %>% 
  select(pos_team, everything())

dndRushComp <- dndRushfPosTeam %>% 
  rbind(dndRushfConf) %>% 
  rbind(dndRushOVR) %>%
  left_join(dndRushRks, by = c("pos_team", "Down & Distance")) %>% 
  ungroup() %>% 
  group_by(`Down & Distance`) %>% 
  mutate(count_dd_thresh_ind = n()) %>% 
  ungroup() %>% 
  filter(count_dd_thresh_ind == 3) %>% 
  select(-count_dd_thresh_ind) %>% 
  mutate(rush_pass = "RUSH") %>% 
  rename(`Team` = pos_team,
         `Plays` = plays_dd_thresh,
         `Success Rate` = success_rate,
         `EPA` = epa,
         `Rush / Pass` = rush_pass) %>% 
  rename(`Rk Plays` = rk_plays_dd_thresh,
         `Rk Success Rate` = rk_success_rate,
         `Rk EPA` = rk_epa) %>% 
  select(`Team`, `Down & Distance`, `Plays`, `Rk Plays`,
         `EPA`, `Rk EPA`, `Success Rate`, `Rk Success Rate`, `Rush / Pass`)

##### dnd passing #####

dndPass <- dnd %>%
  filter(pos_team %in% fbsTeams) %>% 
  filter(pass == 1)

dndPassfPosTeam <- dndPass %>% 
  filter(!is.na(`Down & Distance`)) %>% 
  filter(pos_team %in% fPosTeam) %>% 
  group_by(pos_team, `Down & Distance`) %>% 
  summarise(plays_dd_thresh = n(),
            success_rate = sum(success) / plays_dd_thresh,
            epa = mean(EPA, na.rm = T))

dndPassfConf <- dndPass %>% 
  filter(!is.na(`Down & Distance`)) %>% 
  filter(offense_conference %in% fConf) %>% 
  group_by(`Down & Distance`) %>% 
  summarise(plays_dd_thresh = n(),
            success_rate = sum(success) / plays_dd_thresh,
            epa = mean(EPA, na.rm = T)) %>% 
  mutate(pos_team = paste0(fConf, " Average")) %>% 
  select(pos_team, everything())

dndPassOVR <- dndPass %>% 
  filter(!is.na(`Down & Distance`)) %>% 
  group_by(`Down & Distance`) %>% 
  summarise(plays_dd_thresh = n(),
            success_rate = sum(success) / plays_dd_thresh,
            epa = mean(EPA, na.rm = T)) %>% 
  mutate(pos_team = paste0("FBS Average")) %>% 
  select(pos_team, everything())

dndPassRks <- dndPass %>% 
  filter(!is.na(`Down & Distance`)) %>%
  filter(pos_team %in% fbsTeams) %>% 
  group_by(pos_team, `Down & Distance`) %>% 
  summarise(plays_dd_thresh = n(),
            success_rate = sum(success) / plays_dd_thresh,
            epa = mean(EPA, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(`Down & Distance`) %>%
  summarise(pos_team,
            rk_plays_dd_thresh = round(rank(-plays_dd_thresh), digits = 0),
            rk_success_rate = round(rank(-success_rate), digits = 0),
            rk_epa = round(rank(-epa), digits = 0)) %>% 
  select(pos_team, everything())

dndPassComp <- dndPassfPosTeam %>% 
  rbind(dndPassfConf) %>% 
  rbind(dndPassOVR) %>%
  left_join(dndPassRks, by = c("pos_team", "Down & Distance")) %>%
  ungroup() %>% 
  group_by(`Down & Distance`) %>% 
  mutate(count_dd_thresh_ind = n()) %>% 
  ungroup() %>% 
  filter(count_dd_thresh_ind == 3) %>% 
  select(-count_dd_thresh_ind) %>% 
  mutate(rush_pass = "PASS") %>% 
  rename(`Team` = pos_team,
         `Plays` = plays_dd_thresh,
         `Success Rate` = success_rate,
         `EPA` = epa,
         `Rush / Pass` = rush_pass) %>% 
  rename(`Rk Plays` = rk_plays_dd_thresh,
         `Rk Success Rate` = rk_success_rate,
         `Rk EPA` = rk_epa) %>% 
  select(`Team`, `Down & Distance`, `Plays`, `Rk Plays`,
         `EPA`, `Rk EPA`, `Success Rate`, `Rk Success Rate`, `Rush / Pass`)


##### dnd summary ####

dndSummary <- dnd %>%
  filter(pos_team %in% fbsTeams) %>% 
  filter(rush == 1 | pass == 1)


dndSummaryfPosTeam <- dndSummary %>% 
  filter(!is.na(down), !is.na(`Down & Distance`)) %>% 
  filter(pos_team %in% fPosTeam) %>% 
  group_by(pos_team, `Down & Distance`) %>% 
  summarise(tot_plays = n(),
            pass_rate = sum(pass) / tot_plays,
            rush_rate = sum(rush) / tot_plays,
            success_rate = sum(success) / tot_plays,
            epa = mean(EPA, na.rm = T))

dndSummaryfConf <- dndSummary %>% 
  filter(!is.na(down), !is.na(`Down & Distance`)) %>% 
  filter(offense_conference %in% fConf) %>% 
  group_by(`Down & Distance`) %>% 
  summarise(tot_plays = n(),
            pass_rate = sum(pass) / tot_plays,
            rush_rate = sum(rush) / tot_plays,
            success_rate = sum(success) / tot_plays,
            epa = mean(EPA, na.rm = T)) %>% 
  mutate(pos_team = fConfAvgText) %>% 
  select(pos_team, everything())

dndSummaryOVR <- dndSummary %>% 
  filter(!is.na(down), !is.na(`Down & Distance`)) %>% 
  group_by(`Down & Distance`) %>% 
  summarise(tot_plays = n(),
            pass_rate = sum(pass) / tot_plays,
            rush_rate = sum(rush) / tot_plays,
            success_rate = sum(success) / tot_plays,
            epa = mean(EPA, na.rm = T)) %>% 
  mutate(pos_team = "FBS Average") %>% 
  select(pos_team, everything())

dndSummaryRks <- dndSummary %>% 
  filter(!is.na(`Down & Distance`)) %>%
  filter(pos_team %in% fbsTeams) %>% 
  group_by(pos_team, `Down & Distance`) %>% 
  summarise(tot_plays = n(),
            pass_rate = sum(pass) / tot_plays,
            rush_rate = sum(rush) / tot_plays,
            success_rate = sum(success) / tot_plays,
            epa = mean(EPA, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(`Down & Distance`) %>% 
  summarise(pos_team,
            rk_tot_plays = round(rank(-tot_plays)),
            rk_pass_rate = round(rank(-pass_rate)),
            rk_rush_rate = round(rank(-rush_rate)),
            rk_success_rate = round(rank(-success_rate)),
            rk_epa = round(rank(-epa))) %>% 
  select(pos_team, everything())

dndSummaryComp <- dndSummaryfPosTeam %>% 
  rbind(dndSummaryfConf) %>% 
  rbind(dndSummaryOVR) %>% 
  left_join(dndSummaryRks, by = c("pos_team", "Down & Distance")) %>% 
  ungroup() %>% 
  group_by(`Down & Distance`) %>% 
  mutate(count_dd_thresh_ind = n()) %>% 
  ungroup() %>% 
  filter(count_dd_thresh_ind == 3) %>% 
  select(-count_dd_thresh_ind) %>% 
  rename(`Team` = pos_team,
         `Plays` = tot_plays,
         `Pass Rate` = pass_rate,
         `Rush Rate` = rush_rate,
         `Success Rate` = success_rate,
         `EPA` = epa) %>% 
  rename(`Rk Plays` = rk_tot_plays,
         `Rk Pass Rate` = rk_pass_rate,
         `Rk Rush Rate` = rk_rush_rate,
         `Rk Success Rate` = rk_success_rate,
         `Rk EPA` = rk_epa) %>% 
  select(`Team`, `Down & Distance`, `Plays`, `Rk Plays`,
         `EPA`, `Rk EPA`, `Success Rate`, `Rk Success Rate`,
         `Pass Rate`, `Rk Pass Rate`, `Rush Rate`, `Rk Rush Rate`)


##### dnd tables #####

###### dnd rush table ######
dndRushTableTitleText <- paste0(fPosTeamWMascotText, " Rushing Down & Distance Analysis")
dndRushTableSubtitleText <- paste0(fSeasons, " College Football Season")

dndRushTable <- dndRushComp %>%
  gt() %>%
  cols_hide(columns = `Rush / Pass`) %>%
  data_color(
    columns = c("Rk Plays", "Rk Success Rate", "Rk EPA"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = c(1, 131),
      na.color = "white"
    )
  ) %>%
  fmt_missing(
    columns = c("Rk Plays", "Rk Success Rate", "Rk EPA"),
    missing_text = ""
  ) %>% 
  tab_header(title = md(dndRushTableTitleText),
             subtitle = md(dndRushTableSubtitleText)) %>%
  tab_source_note(source_note = md(captionText)) %>% 
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(
    columns = c("EPA", "Success Rate"),
    decimals = 4
  ) %>%
  gt_theme_538()

#dndRushTable

###### dnd pass table ######

dndPassTableTitleText <- paste0(fPosTeamWMascotText, " Passing Down & Distance Analysis")
dndPassTableSubtitleText <- paste0(fSeasons, " College Football Season")

dndPassTable <- dndPassComp %>%
  gt() %>%
  cols_hide(columns = `Rush / Pass`) %>%
  data_color(
    columns = c("Rk Plays", "Rk Success Rate", "Rk EPA"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = c(1, 131),
      na.color = "white"
    )
  ) %>%
  fmt_missing(
    columns = c("Rk Plays", "Rk Success Rate", "Rk EPA"),
    missing_text = ""
  ) %>% 
  tab_header(title = md(dndPassTableTitleText),
             subtitle = md(dndPassTableSubtitleText)) %>%
  tab_source_note(source_note = md(captionText)) %>%
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(
    columns = c("EPA", "Success Rate"),
    decimals = 4
  ) %>%
  gt_theme_538()

#dndPassTable

###### dnd summary table ######

dndSummaryTableTitleText <- paste0(fPosTeamWMascotText, " Overall Down & Distance Analysis")
dndSummaryTableSubtitleText <- paste0(fSeasons, " College Football Season")

dndSummaryTable <- dndSummaryComp %>%
  gt() %>%
  data_color(
    columns = c("Rk Plays", "Rk Pass Rate", "Rk Rush Rate", "Rk Success Rate", "Rk EPA"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = c(1, 131),
      na.color = "white"
    )
  ) %>%
  fmt_missing(
    columns = c("Rk Plays", "Rk Pass Rate", "Rk Rush Rate", "Rk Success Rate", "Rk EPA"),
    missing_text = ""
  ) %>% 
  tab_header(title = md(dndSummaryTableTitleText),
             subtitle = md(dndSummaryTableSubtitleText)) %>%
  tab_source_note(source_note = md(captionText)) %>%
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(
    columns = c("EPA", "Success Rate", "Pass Rate", "Rush Rate"),
    decimals = 4
  ) %>%
  gt_theme_538()

# dndSummaryTable

#### RED ZONE ####

dndRZ <- pbp %>%
  filter(wp_before >= 0.10 & wp_before <= 0.90) %>%
  filter(rz_play == 1) %>%
  filter(rush == 1 | pass == 1) %>% 
  mutate(dist_thresh = case_when(
    (distance > 0 & distance <= 3) ~ "1-3",
    (distance > 3 & distance <= 6) ~ "3-6",
    (distance > 6 & distance <= 10) ~ "7-10",
    (distance > 10) ~ "+10"
  )) %>% 
  ungroup() %>% 
  mutate(drive_td = ifelse(drive_result == "TD", 1, 0),
         is_td = ifelse((rush_td == 1 | pass_td == 1), 1, 0))

dndRZfPosTeam <- dndRZ %>%
  filter(!is.na(down), !is.na(distance), !is.na(dist_thresh)) %>% 
  filter(pos_team %in% fPosTeam) %>% 
  group_by(pos_team) %>% 
  summarise(tot_rz_tds = sum(is_td), 
            tot_rz_drives = n_distinct(drive_id), 
            tot_rz_plays = n(), 
            rz_drive_td_rate = tot_rz_tds / tot_rz_drives, 
            rz_drive_pass_td_rate = sum(pass_td) / tot_rz_drives, 
            rz_drive_rush_td_rate = sum(rush_td) / tot_rz_drives,
            rz_play_td_rate = sum(pass_td) / tot_rz_plays, 
            rz_play_pass_td_rate = sum(pass_td) / tot_rz_plays, 
            rz_play_rush_td_rate = sum(rush_td) / tot_rz_plays,
            rz_pass_tds = sum(pass_td), 
            rz_rush_tds = sum(rush_td), 
            pct_rz_tds_pass = rz_pass_tds / tot_rz_tds, 
            pct_rz_tds_rush = rz_rush_tds / tot_rz_tds) %>% 
  select(pos_team, tot_rz_drives, tot_rz_plays, tot_rz_tds, rz_drive_td_rate, rz_play_td_rate,
         rz_pass_tds, rz_drive_pass_td_rate, rz_play_pass_td_rate, pct_rz_tds_pass,
         rz_rush_tds, rz_drive_rush_td_rate, rz_play_rush_td_rate, pct_rz_tds_rush)

dndRZfConf <- dndRZ %>%
  filter(!is.na(down), !is.na(distance), !is.na(dist_thresh)) %>%
  filter(offense_conference %in% fConf) %>% 
  summarise(tot_rz_tds = sum(is_td), 
            tot_rz_drives = n_distinct(drive_id), 
            tot_rz_plays = n(), 
            rz_drive_td_rate = tot_rz_tds / tot_rz_drives,
            rz_drive_pass_td_rate = sum(pass_td) / tot_rz_drives,
            rz_drive_rush_td_rate = sum(rush_td) / tot_rz_drives,
            rz_play_td_rate = sum(pass_td) / tot_rz_plays,
            rz_play_pass_td_rate = sum(pass_td) / tot_rz_plays,
            rz_play_rush_td_rate = sum(rush_td) / tot_rz_plays,
            rz_pass_tds = sum(pass_td),
            rz_rush_tds = sum(rush_td), 
            pct_rz_tds_pass = rz_pass_tds / tot_rz_tds,
            pct_rz_tds_rush = rz_rush_tds / tot_rz_tds) %>% 
  mutate(pos_team = fConfAvgText) %>% 
  select(pos_team, tot_rz_drives, tot_rz_plays, tot_rz_tds, rz_drive_td_rate, rz_play_td_rate,
         rz_pass_tds, rz_drive_pass_td_rate, rz_play_pass_td_rate, pct_rz_tds_pass,
         rz_rush_tds, rz_drive_rush_td_rate, rz_play_rush_td_rate, pct_rz_tds_rush)

dndRZOVR <- dndRZ %>%
  filter(!is.na(down), !is.na(distance), !is.na(dist_thresh)) %>% 
  summarise(tot_rz_tds = sum(is_td), 
            tot_rz_drives = n_distinct(drive_id), 
            tot_rz_plays = n(), 
            rz_drive_td_rate = tot_rz_tds / tot_rz_drives,
            rz_drive_pass_td_rate = sum(pass_td) / tot_rz_drives,
            rz_drive_rush_td_rate = sum(rush_td) / tot_rz_drives,
            rz_play_td_rate = sum(pass_td) / tot_rz_plays,
            rz_play_pass_td_rate = sum(pass_td) / tot_rz_plays,
            rz_play_rush_td_rate = sum(rush_td) / tot_rz_plays,
            rz_pass_tds = sum(pass_td),
            rz_rush_tds = sum(rush_td), 
            pct_rz_tds_pass = rz_pass_tds / tot_rz_tds,
            pct_rz_tds_rush = rz_rush_tds / tot_rz_tds) %>% 
  mutate(pos_team = "FBS Average") %>% 
  select(pos_team, tot_rz_drives, tot_rz_plays, tot_rz_tds, rz_drive_td_rate, rz_play_td_rate,
         rz_pass_tds, rz_drive_pass_td_rate, rz_play_pass_td_rate, pct_rz_tds_pass,
         rz_rush_tds, rz_drive_rush_td_rate, rz_play_rush_td_rate, pct_rz_tds_rush)

dndRZComp <- dndRZfPosTeam %>% 
  rbind(dndRZfConf) %>% 
  rbind(dndRZOVR) %>% 
  rename(`Team` = pos_team,
         `Tot RZ Drives` = tot_rz_drives,
         `Tot RZ Plays` = tot_rz_plays,
         `Tot RZ TDs` = tot_rz_tds,
         `RZ Drive TD Rate` = rz_drive_td_rate,
         `RZ Play TD Rate` = rz_play_td_rate,
         `RZ Pass TDs` = rz_pass_tds,
         `RZ Drive Pass TD Rate` = rz_drive_pass_td_rate,
         `RZ Play Pass TD Rate` = rz_play_pass_td_rate,
         `PCT RZ TDs Pass` = pct_rz_tds_pass,
         `RZ Rush TDs` = rz_rush_tds,
         `RZ Drive Rush TD Rate` = rz_drive_rush_td_rate,
         `RZ Play Rush TD Rate` = rz_play_rush_td_rate,
         `PCT RZ TDs Rush` = pct_rz_tds_rush
  )

##### red zone table #####

dndRZTableTitleText <- paste0(fPosTeamWMascotText, " Red Zone Analysis")
dndRZTableSubtitleText <- paste0(fSeasons, " College Football Season")

dndRZTable <- dndRZComp %>%
  gt() %>%
  tab_header(title = md(dndRZTableTitleText),
             subtitle = md(dndRZTableSubtitleText)) %>%
  tab_source_note(source_note = md(captionText)) %>%
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(
    columns = c("RZ Drive TD Rate", "RZ Play TD Rate", "RZ Drive Pass TD Rate", "RZ Play Pass TD Rate", "PCT RZ TDs Pass", "RZ Drive Rush TD Rate", "RZ Play Rush TD Rate", "PCT RZ TDs Rush"),
    decimals = 4
  ) %>%
  gt_theme_538()

#dndRZTable

#### OFF SUMM ####

offSumm <- pbp %>% 
  filter(wp_before >= 0.10 & wp_before <= 0.90) %>%
  filter(pos_team %in% fbsTeams) %>% 
  filter(!is.na(down)) %>% 
  filter(rush == 1 | pass == 1) %>%
  ungroup() %>% 
  mutate(rush_expl = ifelse((rush == 1 & yards_gained >= 10), 1, 0),
         pass_expl = ifelse((pass == 1 & yards_gained >= 20), 1, 0),
         expl = ifelse((pass_expl == 1 | rush_expl == 1), 1, 0))

offSummfPosTeam <- offSumm %>% 
  filter(pos_team %in% fPosTeam) %>% 
  ungroup() %>%
  group_by(pos_team) %>% 
  summarise(tot_plays = n(),
            pass_plays = sum(pass),
            rush_plays = sum(rush),
            pass_rate = pass_plays / tot_plays,
            rush_rate = rush_plays / tot_plays,
            epa = mean(EPA, na.rm = T),
            pass_epa = mean(EPA[pass == 1], na.rm = T),
            rush_epa = mean(EPA[rush == 1], na.rm = T),
            success_rate = sum(epa_success) / tot_plays,
            pass_success_rate = sum(epa_success[pass == 1]) / pass_plays,
            rush_success_rate = sum(epa_success[rush == 1]) / rush_plays,
            pass_expl_pct = sum(pass_expl) / pass_plays,
            rush_expl_pct = sum(rush_expl) / rush_plays,
            expl_rate = sum(expl) / tot_plays) %>% 
  select(pos_team, tot_plays, epa, success_rate, expl_rate,
         pass_rate, pass_epa, pass_success_rate, pass_expl_pct, pass_plays,
         rush_rate, rush_epa, rush_success_rate, rush_expl_pct, rush_plays)

offSummfConf <- offSumm %>% 
  filter(offense_conference %in% fConf) %>% 
  ungroup() %>%
  summarise(tot_plays = n(),
            pass_plays = sum(pass),
            rush_plays = sum(rush),
            pass_rate = pass_plays / tot_plays,
            rush_rate = rush_plays / tot_plays,
            epa = mean(EPA, na.rm = T),
            pass_epa = mean(EPA[pass == 1], na.rm = T),
            rush_epa = mean(EPA[rush == 1], na.rm = T),
            success_rate = sum(epa_success) / tot_plays,
            pass_success_rate = sum(epa_success[pass == 1]) / pass_plays,
            rush_success_rate = sum(epa_success[rush == 1]) / rush_plays,
            pass_expl_pct = sum(pass_expl) / pass_plays,
            rush_expl_pct = sum(rush_expl) / rush_plays,
            expl_rate = sum(expl) / tot_plays) %>%
  mutate(pos_team = fConfAvgText) %>% 
  select(pos_team, tot_plays, epa, success_rate, expl_rate,
         pass_rate, pass_epa, pass_success_rate, pass_expl_pct, pass_plays,
         rush_rate, rush_epa, rush_success_rate, rush_expl_pct, rush_plays)

offSummOVR <- offSumm %>%
  ungroup() %>%
  summarise(tot_plays = n(),
            pass_plays = sum(pass),
            rush_plays = sum(rush),
            pass_rate = pass_plays / tot_plays,
            rush_rate = rush_plays / tot_plays,
            epa = mean(EPA, na.rm = T),
            pass_epa = mean(EPA[pass == 1], na.rm = T),
            rush_epa = mean(EPA[rush == 1], na.rm = T),
            success_rate = sum(epa_success) / tot_plays,
            pass_success_rate = sum(epa_success[pass == 1]) / pass_plays,
            rush_success_rate = sum(epa_success[rush == 1]) / rush_plays,
            pass_expl_pct = sum(pass_expl) / pass_plays,
            rush_expl_pct = sum(rush_expl) / rush_plays,
            expl_rate = sum(expl) / tot_plays) %>%
  mutate(pos_team = "FBS Average") %>% 
  select(pos_team, tot_plays, epa, success_rate, expl_rate,
         pass_rate, pass_epa, pass_success_rate, pass_expl_pct, pass_plays,
         rush_rate, rush_epa, rush_success_rate, rush_expl_pct, rush_plays)

offSummRks <- offSumm %>% 
  filter(pos_team %in% fbsTeams) %>%
  ungroup() %>%
  group_by(pos_team) %>% 
  summarise(tot_plays = n(),
            pass_plays = sum(pass),
            rush_plays = sum(rush),
            pass_rate = pass_plays / tot_plays,
            rush_rate = rush_plays / tot_plays,
            epa = mean(EPA, na.rm = T),
            pass_epa = mean(EPA[pass == 1], na.rm = T),
            rush_epa = mean(EPA[rush == 1], na.rm = T),
            success_rate = sum(epa_success) / tot_plays,
            pass_success_rate = sum(epa_success[pass == 1]) / pass_plays,
            rush_success_rate = sum(epa_success[rush == 1]) / rush_plays,
            pass_expl_pct = sum(pass_expl) / pass_plays,
            rush_expl_pct = sum(rush_expl) / rush_plays,
            expl_rate = sum(expl) / tot_plays) %>% 
  ungroup() %>% 
  summarise(pos_team,
            rk_epa = round(rank(-epa)),
            rk_success_rate = round(rank(-success_rate)),
            rk_expl_rate = round(rank(-expl_rate)),
            rk_pass_rate = round(rank(-pass_rate)),
            rk_pass_epa = round(rank(-pass_epa)),
            rk_pass_success_rate = round(rank(-pass_success_rate)),
            rk_pass_expl_pct = round(rank(-pass_expl_pct)),
            rk_rush_rate = round(rank(-rush_rate)),
            rk_rush_epa = round(rank(-rush_epa)),
            rk_rush_success_rate = round(rank(-rush_success_rate)),
            rk_rush_expl_pct = round(rank(-rush_expl_pct))) %>% 
  select(pos_team, everything())

offSummComp <- offSummfPosTeam %>% 
  rbind(offSummfConf) %>% 
  rbind(offSummOVR) %>% 
  left_join(offSummRks, by = "pos_team") %>% 
  rename(`Team` = pos_team,
         `Tot Plays` = tot_plays,
         `EPA` = epa,
         `Rk EPA` = rk_epa,
         `Success Rate` = success_rate,
         `Rk Success Rate` = rk_success_rate,
         `Expl Play Rate` = expl_rate,
         `Rk Expl Play Rate` = rk_expl_rate,
         `Pass Rate` = pass_rate,
         `Rk Pass Rate` = rk_pass_rate,
         `Pass EPA` = pass_epa,
         `Rk Pass EPA` = rk_pass_epa,
         `Pass Success Rate` = pass_success_rate,
         `Rk Pass Success Rate` = rk_pass_success_rate,
         `Pct Pass Expl` = pass_expl_pct,
         `Rk Pct Pass Expl` = rk_pass_expl_pct,
         `Pass Plays` = pass_plays,
         `Rush Rate` = rush_rate,
         `Rk Rush Rate` = rk_rush_rate,
         `Rush EPA` = rush_epa,
         `Rk Rush EPA` = rk_rush_epa,
         `Rush Success Rate` = rush_success_rate,
         `Rk Rush Success Rate` = rk_rush_success_rate,
         `Pct Rush Expl` = rush_expl_pct,
         `Rk Pct Rush Expl` = rk_rush_expl_pct,
         `Rush Plays` = rush_plays) %>% 
  select(`Team`, 
         `Tot Plays`,
         `EPA`, `Rk EPA`, 
         `Success Rate`, `Rk Success Rate`, 
         `Expl Play Rate`, `Rk Expl Play Rate`, 
         `Pass Rate`, `Rk Pass Rate`, 
         `Pass EPA`, `Rk Pass EPA`, 
         `Pass Success Rate`, `Rk Pass Success Rate`, 
         `Pct Pass Expl`, `Rk Pct Pass Expl`, 
         `Pass Plays`, 
         `Rush Rate`, `Rk Rush Rate`, 
         `Rush EPA`, `Rk Rush EPA`, 
         `Rush Success Rate`, `Rk Rush Success Rate`, 
         `Pct Rush Expl`, `Rk Pct Rush Expl`, 
         `Rush Plays`)

##### off sum table #####

offSummTableTitleText <- paste0(fPosTeamWMascotText, " Overall Offensive Analysis")
offSummTableSubtitleText <- paste0(fSeasons, " College Football Season")

offSummTable <- offSummComp %>%
  gt() %>%
  data_color(
    columns = c("Rk EPA", "Rk Success Rate", "Rk Expl Play Rate", "Rk Pass Rate", "Rk Pass EPA", "Rk Pass Success Rate", "Rk Pct Pass Expl", "Rk Rush Rate", "Rk Rush EPA", "Rk Rush Success Rate", "Rk Pct Rush Expl"),
    colors = scales::col_numeric(
      palette = color_palette,
      domain = c(1, 131),
      na.color = "white"
    )
  ) %>%
  fmt_missing(
    columns = c("Rk EPA", "Rk Success Rate", "Rk Expl Play Rate", "Rk Pass Rate", "Rk Pass EPA", "Rk Pass Success Rate", "Rk Pct Pass Expl", "Rk Rush Rate", "Rk Rush EPA", "Rk Rush Success Rate", "Rk Pct Rush Expl"),
    missing_text = ""
  ) %>% 
  tab_header(title = md(offSummTableTitleText),
             subtitle = md(offSummTableSubtitleText)) %>%
  tab_source_note(source_note = md(captionText)) %>%
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(
    columns = c("EPA", "Success Rate", "Expl Play Rate", "Pass Rate", "Pass EPA", "Pass Success Rate", "Pct Pass Expl", "Rush Rate", "Rush EPA", "Rush Success Rate", "Pct Rush Expl"),
    decimals = 4
  ) %>%
  gt_theme_538()

#offSummTable


##### plots #####

offSummPlotData <- offSumm %>%
  ungroup() %>%
  group_by(pos_team) %>% 
  summarise(is_fposteam = ifelse(pos_team %in% fPosTeam, 1, 0),
            tot_plays = n(),
            pass_plays = sum(pass),
            rush_plays = sum(rush),
            pass_rate = pass_plays / tot_plays,
            rush_rate = rush_plays / tot_plays,
            epa = mean(EPA, na.rm = T),
            pass_epa = mean(EPA[pass == 1], na.rm = T),
            rush_epa = mean(EPA[rush == 1], na.rm = T),
            success_rate = sum(epa_success) / tot_plays,
            pass_success_rate = sum(epa_success[pass == 1]) / pass_plays,
            rush_success_rate = sum(epa_success[rush == 1]) / rush_plays,
            pass_expl_pct = sum(pass_expl) / pass_plays,
            rush_expl_pct = sum(rush_expl) / rush_plays,
            expl_rate = sum(expl) / tot_plays) %>% 
  select(pos_team, is_fposteam, tot_plays, epa, success_rate, expl_rate,
         pass_rate, pass_epa, pass_success_rate, pass_expl_pct, pass_plays,
         rush_rate, rush_epa, rush_success_rate, rush_expl_pct, rush_plays) %>% 
  unique() %>% 
  left_join(fbsTeamsColorsLogos, by = c("pos_team" = "school")) %>%
  ungroup() %>% 
  mutate(is_fconf = ifelse(conference %in% fConf, 1, 0)) %>% 
  filter(!is.na(logo)) %>% 
  mutate(point_color = ifelse(is_fconf == 1, color, "gray72"),
         point_alt_color = ifelse(is_fconf == 1, alt_color, "gray48")) # Gray out all teams not in fConf

###### pass rate eff plot ######

passRateEffPlot <- ggplot(data = offSummPlotData, aes(x = pass_rate, y = pass_epa)) +
  geom_hline(yintercept = mean(offSummPlotData$pass_epa),
             linewidth = 0.8, color = "red", lty = "dashed") +
  geom_vline(xintercept = mean(offSummPlotData$pass_rate),
             linewidth = 0.8, color = "red", lty = "dashed") +
  geom_point(shape = 21, fill = offSummPlotData$point_color,
             color = offSummPlotData$point_alt_color, size = 4.5) +
  geom_image(data = filter(offSummPlotData, is_fposteam == 1),
             aes(image = logo), size = 0.096) +
  scale_x_continuous(breaks = pretty_breaks(),
                     labels = percent_format()) +
  scale_y_continuous(breaks = pretty_breaks(n = 8)) +
  geom_text_repel(data = filter(offSummPlotData, is_fconf == 1),
                  aes(label = pos_team),
                  size = 3.6, box.padding = 0.64, fontface = "bold", family = "Bahnschrift") +
  labs(title = paste0(fPosTeamWMascotText, ": Pass Rate vs. Pass Efficiency"),
       subtitle = paste0(fSeasons, " College Football Season"),
       x = "Pass Rate",
       y = "Pass EPA",
       caption = captionText) +
  sterb_analytics_theme()

#passRateEffPlot

###### rush rate eff plot ######

rushRateEffPlot <- ggplot(data = offSummPlotData, aes(x = rush_rate, y = rush_epa)) +
  geom_hline(yintercept = mean(offSummPlotData$rush_epa),
             linewidth = 0.8, color = "red", lty = "dashed") +
  geom_vline(xintercept = mean(offSummPlotData$rush_rate),
             linewidth = 0.8, color = "red", lty = "dashed") +
  geom_point(shape = 21, fill = offSummPlotData$point_color,
             color = offSummPlotData$point_alt_color, size = 4.5) +
  geom_image(data = filter(offSummPlotData, is_fposteam == 1),
             aes(image = logo), size = 0.096) +
  scale_x_continuous(breaks = pretty_breaks(),
                     labels = percent_format()) +
  scale_y_continuous(breaks = pretty_breaks(n = 8)) +
  geom_text_repel(data = filter(offSummPlotData, is_fconf == 1),
                  aes(label = pos_team),
                  size = 3.6, box.padding = 0.64, fontface = "bold", family = "Bahnschrift") +
  labs(title = paste0(fPosTeamWMascotText, ": Rush Rate vs. Rush Efficiency"),
       subtitle = paste0(fSeasons, " College Football Season"),
       x = "Rush Rate",
       y = "Rush EPA",
       caption = captionText) +
  sterb_analytics_theme()

#rushRateEffPlot

###### pct expl plot ######

pctExplPlot <- ggplot(data = offSummPlotData, aes(x = pass_expl_pct, y = rush_expl_pct)) +
  geom_hline(yintercept = mean(offSummPlotData$rush_expl_pct),
             linewidth = 0.8, color = "red", lty = "dashed") +
  geom_vline(xintercept = mean(offSummPlotData$pass_expl_pct),
             linewidth = 0.8, color = "red", lty = "dashed") +
  geom_point(shape = 21, fill = offSummPlotData$point_color,
             color = offSummPlotData$point_alt_color, size = 4.5) +
  geom_image(data = filter(offSummPlotData, is_fposteam == 1),
             aes(image = logo), size = 0.096) +
  scale_x_continuous(breaks = pretty_breaks(n = 8),
                     labels = percent_format()) +
  scale_y_continuous(breaks = pretty_breaks(n = 8),
                     labels = percent_format()) +
  geom_text_repel(data = filter(offSummPlotData, is_fconf == 1),
                  aes(label = pos_team),
                  size = 3.6, box.padding = 0.64, fontface = "bold", family = "Bahnschrift") +
  labs(title = paste0(fPosTeamWMascotText, ": Percent Passes Explosive vs. Percent Rushes Explosive"),
       subtitle = paste0(fSeasons, " College Football Season | Explosive Pass: 20+ yds gained ; Explosive Rush: 10+ yds"),
       x = "% Passes Explosive",
       y = "% Rushes Explosive",
       caption = captionText) +
  sterb_analytics_theme()

#pctExplPlot

#### RECORD SUMM ####

gameRecords <- cfbd_game_records(year = fSeasons) %>% 
  filter(team %in% fPosTeam)

gameRecordsfPosTeam <- gameRecords %>% 
  select(expected_wins, total_games, total_wins, total_losses) %>% 
  mutate(ind = "Overall") %>% 
  rename(`Expected Wins` = expected_wins,
         `Games` = total_games,
         `Wins` = total_wins,
         `Losses` = total_losses) %>%  
  select(ind, everything())

gameRecordsfConfRow <- gameRecords %>% 
  select(conference_games, conference_wins, conference_losses)  %>% 
  mutate(ind = "Conference Games",
         expected_wins = "") %>% 
  rename(`Expected Wins` = expected_wins,
         `Games` = conference_games,
         `Wins` = conference_wins,
         `Losses` = conference_losses) %>%
  select(ind, everything())

gameRecordsHomeRow <- gameRecords %>% 
  select(home_games, home_wins, home_losses)  %>% 
  mutate(ind = "Home Games",
         expected_wins = "") %>% 
  rename(`Expected Wins` = expected_wins,
         `Games` = home_games,
         `Wins` = home_wins,
         `Losses` = home_losses) %>%
  select(ind, everything())

gameRecordsAwayRow <- gameRecords %>% 
  select(away_games, away_wins, away_losses)  %>% 
  mutate(ind = "Away Games",
         expected_wins = "") %>% 
  rename(`Expected Wins` = expected_wins,
         `Games` = away_games,
         `Wins` = away_wins,
         `Losses` = away_losses) %>%
  select(ind, everything())

gameRecordsfPosTeamFIN <- gameRecordsfPosTeam %>% 
  rbind(gameRecordsfConfRow) %>% 
  rbind(gameRecordsHomeRow) %>% 
  rbind(gameRecordsAwayRow) %>% 
  select(ind, `Wins`, `Losses`, `Games`, `Expected Wins`) %>% 
  ungroup() %>% 
  group_by(ind) %>% 
  mutate(`Win PCT` = `Wins` / `Games`) %>% 
  ungroup() %>% 
  select(ind, `Wins`, `Losses`, `Games`, `Win PCT`, `Expected Wins`)

##### record summ table #####

gameRecordSummTableTitleText <- paste0(fPosTeamWMascotText, " Record Summary")
gameRecordSummTableSubtitleText <- paste0(fSeasons, " College Football Season")

gameRecordSummTable <- gameRecordsfPosTeamFIN %>%
  gt(rowname_col = "ind") %>%
  tab_header(title = md(gameRecordSummTableTitleText),
             subtitle = md(gameRecordSummTableSubtitleText)) %>%
  tab_source_note(source_note = md(captionTextCFBD)) %>%
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(
    columns = c("Expected Wins", "Win PCT"),
    decimals = 2
  ) %>%
  data_color(
    columns = c("Win PCT"),
    colors = scales::col_numeric(
      palette = color_palette_reverse,
      domain = c(1, 0),
      na.color = "white"
    )
  ) %>%
  gt_theme_538()

#gameRecordSummTable



#### CFBD ####

##### player usage #####

playerUsagefPosTeam <- playerUsage %>% 
  filter(team %in% fPosTeam) %>% 
  ungroup() %>% 
  group_by(position) %>% 
  mutate(position_usg_rank = round(rank(-usg_overall), 0)) %>%
  ungroup() %>% 
  arrange(-usg_overall) %>% 
  select(season, position, position_usg_rank, name,
         usg_overall, pctile_usg_overall, pctile_pos_usg,
         usg_pass, usg_rush, usg_standard_downs,
         usg_1st_down, usg_2nd_down, usg_3rd_down) %>%
  mutate(pctile_usg_overall = pctile_usg_overall * 100,
         pctile_pos_usg = pctile_pos_usg *100,
         pos_ind = case_when(position == "QB" ~ 1,
                             position == "RB" ~ 2,
                             position == "WR" ~ 3,
                             position == "TE" ~ 4)) %>% 
  rename(`Season` = season,
         `Position` = position,
         `Team POS USG RK` = position_usg_rank,
         `Player` = name,
         `OVR USG`= usg_overall,
         `OVR USG %ile` = pctile_usg_overall,
         `POS USG %ile` = pctile_pos_usg,
         `Pass USG` = usg_pass,
         `Rush USG` = usg_rush,
         `STD Down USG` = usg_standard_downs,
         `1st Down USG` = usg_1st_down,
         `2nd Down USG` = usg_2nd_down,
         `3rd Down USG` = usg_3rd_down) %>% 
  arrange(pos_ind, -`OVR USG`)

playerUsagefPosTeam$`Position` <- factor(playerUsagefPosTeam$`Position`, levels = c("QB", "RB", "WR", "TE"))

# pull max usage for fPosTeam
maxUsg <- playerUsagefPosTeam %>% 
  ungroup() %>% 
  summarise(max_usg = max(`OVR USG`)) %>% 
  pull(max_usg)

playerUsageTeamTableTitleText <- paste0(fPosTeamWMascotText, " Player Usage Analysis")
playerUsageTeamTableSubtitleText <- paste0(fSeasons, " College Football Season")

playerUsageTeamTable <- playerUsagefPosTeam %>%
  gt() %>%
  tab_header(title = md(playerUsageTeamTableTitleText),
             subtitle = md(playerUsageTeamTableSubtitleText)) %>%
  tab_source_note(source_note = md(captionTextCFBD)) %>%
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(
    columns = c("OVR USG"),
    decimals = 4
  ) %>%
  fmt_number(
    columns = c("OVR USG %ile", "POS USG %ile"),
    decimals = 2
  ) %>% 
  #data_color(
  #  columns = c("OVR USG"),
  #  colors = scales::col_numeric(
  #    palette = color_palette_reverse,
  #    domain = c(maxUsg, 0),
  #    na.color = "white"
  #  )
  #)  %>% 
  data_color(
    columns = c("OVR USG %ile", "POS USG %ile"),
    colors = scales::col_numeric(
      palette = color_palette_reverse,
      domain = c(100, 0),
      na.color = "white"
    )
  ) %>%
  tab_source_note(source_note = md(captionTextCFBD)) %>%
  cols_hide(columns = "pos_ind") %>% 
  gt_theme_538()

#playerUsageTeamTable

#### OUTPUTS ####

gameRecordSummTable #1
#gtsave(gameRecordSummTable, "1gameRecordSummTable.png")

offSummTable #2
#gtsave(offSummTable, "2offSummTable.png")

dndSummaryTable #3
#gtsave(dndSummaryTable, "3dndSummaryTable.png")

dndPassTable #4
#gtsave(dndPassTable, "4dndPassTable.png")

dndRushTable #5
#gtsave(dndRushTable, "5dndRushTable.png")

dndRZTable #6
#gtsave(dndRZTable, "6dndRZTable.png")

playerUsageTeamTable #7
#gtsave(playerUsageTeamTable, "7playerUsageTeamTable.png")

passRateEffPlot #8
#ggsave("8passRateEffPlot.png", dpi = "retina", width = 10, height = 10)

rushRateEffPlot #9
#ggsave("9rushRateEffPlot.png", dpi = "retina", width = 10, height = 10)

pctExplPlot #10
#ggsave("10pctExplPlot.png", dpi = "retina", width = 10, height = 10)
