nba_stats <- read.csv("\\nba_stats.csv")

library(dplyr)
library(tidyr)
library(ggplot2)

#column name changes
nba_stats <- nba_stats %>%
  rename('fg%' = fg., '3pm' = X3pm, '3pa' = X3pa,
         '3p%' = X3p., 'ft%' = ft.)

library(hrbrthemes)

#warriors average points scored and allowed
scores <- nba_stats %>% group_by(season) %>%
  #giving rankings to pts_scored and pts_conceded for each season
  mutate(rank_scored = rank(desc(pts_scored), ties.method = "min"),
         rank_allowed = rank(pts_conceded, ties.method = "min")) %>%
  select(team, season, pts_scored, pts_conceded,
         rank_scored, rank_allowed)
  #changing the rank, if rank is 1, add "st", if 2, add "nd", so on
scores$rank_scored <-
  ifelse(scores$rank_scored == 1,
         paste(scores$rank_scored, "st", sep = ""),
         ifelse(scores$rank_scored == 2,
                paste(scores$rank_scored, "nd", sep =""),
                ifelse(scores$rank_scored == 3,
                       paste(scores$rank_scored, "rd", sep = ""),
                       paste(scores$rank_scored, "th", sep = ""))))
  scores$rank_allowed <-
  ifelse(scores$rank_allowed == 1,
         paste(scores$rank_allowed, "st", sep = ""),
         ifelse(scores$rank_allowed == 2,
                paste(scores$rank_allowed, "nd", sep =""),
                ifelse(scores$rank_allowed == 3,
                       paste(scores$rank_allowed, "rd", sep = ""),
                       paste(scores$rank_allowed, "th", sep = ""))))
  
  #to make a side-by-side bar chart,
  #divide the table and give label "scored" or "allowed"
scored <- scores %>% filter(team == "Warriors") %>%
  select(season, pts_scored, rank_scored) %>%
  transmute(pts = pts_scored, rank = rank_scored,
            points = "scored")

allowed <- scores %>% filter(team == "Warriors") %>%
  select(season, pts_conceded, rank_allowed) %>%
  transmute(pts = pts_conceded, rank = rank_allowed,
            points = "allowed")

  #combine the two table
scores <- rbind(scored, allowed)

library(RColorBrewer)
#bar graph for points
  #making "scored" come before "allowed"
scores$points <- factor(scores$points,
                        levels = c("scored", "allowed"),
                        ordered = TRUE)

ggplot(scores, aes(x = season, y = pts, fill = points)) +
  geom_col( width= 0.8, position = "dodge") +
  scale_fill_manual(values = c("#045A8D", "darkred")) +
  #inside the bar, add points as label
  geom_text(aes(label = pts), vjust = 1.5, colour = "white",
            position = position_dodge(.8)) +
  #over the bar, add rank as label
  geom_text(aes(label = rank), vjust = -0.5,
            position = position_dodge(.8)) +
  theme_ipsum() +
  ggtitle("Points scored and allowed") +
  #removing grid lines
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  #removing y axis text and titles for both axes
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#bar graph for differentials
  #point differential is scored - allowed
diff <- nba_stats %>%
  mutate(differential = pts_scored - pts_conceded) %>% 
  group_by(season) %>%
  mutate(rank = rank(desc(differential),
                     ties.method = "min")) %>%
  select(team, season, differential, rank) %>%
  filter(team == "Warriors")

diff$differential <- round(diff$differential, 1)

diff$rank <- ifelse(diff$rank == 1,
                   paste(diff$rank, "st", sep = ""), diff$rank)
diff$rank <- ifelse(diff$rank == 2,
                    paste(diff$rank, "nd", sep = ""), diff$rank)
diff$rank <- ifelse(diff$rank == 3,
                   paste(diff$rank, "rd", sep = ""), diff$rank)

d <- ggplot(diff, aes(x = season, y = differential)) +
  geom_col(width = 0.8, fill = "#045A8D") +
  geom_text(aes(label = differential), vjust = 1.5,
            colour = "white") +
  geom_text(aes(label = rank), vjust = -0.5) +
  theme_ipsum() +
  ggtitle("Points differential") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank())

#graph for win_ratio, same process as points differential
win <- nba_stats %>%
  group_by(season) %>%
  mutate(rank = rank(desc(win_ratio), ties.method = "min")) %>%
  select(team, season, win_ratio, rank) %>%
  filter(team == "Warriors")

win$rank <- ifelse(win$rank == 1,
                    paste(win$rank, "st", sep = ""), win$rank)
win$rank <- ifelse(win$rank == 3,
                    paste(win$rank, "rd", sep = ""), win$rank)

w <- ggplot(win, aes(x = season, y = win_ratio)) +
  geom_col(width = 0.8, fill = "#045A8D") +
  geom_text(aes(label = win_ratio), vjust = 1.5, colour = "white") +
  geom_text(aes(label = rank), vjust = -0.5) +
  theme_ipsum() +
  ggtitle("Win percentage") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank())

#print points differential and win percentage in one image
library(cowplot)
plot_grid(d, w)

#parallel coordinates chart for playoff teams 2014 - 2018
library(GGally)

#create list of names for western conference teams
west_teams <- c("Warriors", "Trail Blazers", "Timberwolves","Thunder",
                "Suns", "Spurs", "Rockets", "Pelicans", "Nuggets",
                "Mavericks", "Lakers", "Kings", "Jazz", "Grizzlies",
                "Clippers")

#if team name is in the list, west, if not, east. 
nba_stats$conference <- ifelse(nba_stats$team %in% west_teams,
                               "West", "East")

#for each conference, find top 8 teams for each season
playoff <- nba_stats %>% group_by(season, conference) %>%
  slice_max(order_by = win_ratio, n = 8)

#due to ties, had to do online search to drop three rows
#2014 Pacer 9, 2014 Thunder 18, 2016 Heat 43
playoff <- playoff[-c(9,18,43),]

#combine teams other than warriors as "Reast of the league"
playoff$team <- ifelse(playoff$team == "Warriors",
                     "Warriors", "Rest of the league")

#give each seasons of warriors unique title like "2014 Warriors"
playoff$team <-
  ifelse(playoff$team == "Warriors",
         paste(playoff$season, playoff$team, sep = " "),
         playoff$team)

#parallel coordinates chart for columns 9 through 17
p <- playoff %>% arrange(desc(team)) %>%
  ggparcoord(playoff, columns = 9:17,
             groupColumn = 1, alphaLines = 1,
             scale = "uniminmax") +
  #unique colors for each season of warriors and grey for the rest
  scale_color_manual(values=c("#33CC33", "#660066",
                              "#FF6600", "#00CCFF",
                              "#CC0033", "#E8E8E8") ) +
  theme_ipsum()
#dealing with margins between x and y values
p <- p + scale_y_continuous(expand = c(0.02, 0.02))
p <- p + scale_x_discrete(expand = c(0.02, 0.02))
#removing axis labels
p <- p + theme(axis.ticks = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y = element_blank(),
               axis.title.x = element_blank())
#removing grid lines except for scales of the variables
p <- p + theme(panel.grid.minor = element_blank(),
               panel.grid.major.y = element_blank(),
               panel.grid.major.x = element_line(color = "#bbbbbb"))
# bring legends to the bottom of the chart
p <- p + theme(legend.position = "bottom")

#figuring out y axis range
min_y <- min(p$data$value)
max_y <- max(p$data$value)
range_y <- (max_y - min_y) * 0.1

#to add labels to top and bottom of the scale
lab_x <- rep(1:9, times = 2)
lab_y <- rep(c(min_y - range_y, max_y + range_y), each = 9)
lab_z <- c(sapply(playoff[, 9:17], min),
           sapply(playoff[, 9:17], max))
lab_z <- as.character(lab_z)
p <- p + annotate("text", x = lab_x, y = lab_y,
                  label = lab_z, size = 3)


print(p)

#repeat the same for columns 18:25
p1 <- playoff %>% arrange(desc(team)) %>%
  ggparcoord(playoff, columns = 18:25,
             groupColumn = 1, alphaLines = 1,
             scale = "uniminmax") +
  scale_color_manual(values=c("#33CC33", "#660066",
                              "#FF6600", "#00CCFF",
                              "#CC0033", "#E8E8E8")) +
  theme_ipsum()
p1 <- p1 + scale_y_continuous(expand = c(0.02, 0.02))
p1 <- p1 + scale_x_discrete(expand = c(0.02, 0.02))
p1 <- p1 + theme(axis.ticks = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y = element_blank(),
               axis.title.x = element_blank())
p1 <- p1 + theme(panel.grid.minor = element_blank(),
               panel.grid.major.y = element_blank(),
               panel.grid.major.x = element_line(color = "#bbbbbb"))
p1 <- p1 + theme(legend.position = "bottom")
min_y1 <- min(p1$data$value)
max_y1 <- max(p1$data$value)
range_y1 <- (max_y1 - min_y1) * 0.1
lab_x1 <- rep(1:8, times = 2) # 2 times, 1 for min 1 for max
lab_y1 <- rep(c(min_y1 - range_y1, max_y1 + range_y1), each = 8)
lab_z1 <- c(sapply(playoff[, 18:25], min),
            sapply(playoff[, 18:25], max))
lab_z1 <- as.character(lab_z1)
p1 <- p1 + annotate("text", x = lab_x1, y = lab_y1,
                    label = lab_z1, size = 3)
print(p1)


#top assists, assist/field goal, assist/turnover
assists <- nba_stats %>% arrange(desc(ast)) %>%
  select(team, season, ast) %>%
  arrange(desc(ast)) %>% head(8)


#to make bold label for warriors team
bold <- ifelse(assists$team == "Warriors", "bold", "plain")
bold <- rev(bold)

#concatenate season with team name
assists$team <- paste(assists$season, assists$team, sep = " ")

#bar graph 
a <- ggplot(assists, aes(x = reorder(team, ast), y = ast)) +
  geom_col(width = 0.8, fill = "#045A8D") +
  #put teams on y axis, assists on x
  coord_flip() +
  geom_text(aes(label = ast), hjust = 1.2, vjust = 0.1, color = "White") +
  theme_ipsum() +
  ggtitle("Top teams for assists") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(face = bold))
  
print(a)

#assists to field goal ratio
astfgm <- nba_stats %>%
  mutate(ast_pct = ast / fgm) %>%
  arrange(desc(ast_pct)) %>%
  select(team, season, ast_pct)%>%
  head(8)
bold <- ifelse(astfgm$team == "Warriors", "bold", "plain")
bold <- rev(bold)
astfgm$team <- paste(astfgm$season, astfgm$team, sep = " ")
astfgm$ast_pct <- round(astfgm$ast_pct, 3)
af <- ggplot(astfgm, aes(x = reorder(team, ast_pct), y = ast_pct)) +
  geom_col(width = 0.8, fill = "#045A8D") +
  coord_flip() +
  geom_text(aes(label = ast_pct), hjust = 1.2,
            vjust = 0.1, color = "White") +
  theme_ipsum() +
  ggtitle("Assist to field goal") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(face = bold))

#assist to turnover ratio
asttov <- nba_stats %>%
  mutate(ast_tov = ast / tov) %>%
  arrange(desc(ast_tov)) %>%
  select(team, season, ast_tov) %>% 
  head(8)
bold <- ifelse(asttov$team == "Warriors", "bold", "plain")
bold <- rev(bold)
#2015 warriors at 22, 2014 warriors at 23
asttov$team <- paste(asttov$season, asttov$team, sep = " ")
asttov$ast_tov <- round(asttov$ast_tov, 3)
at <- ggplot(asttov, aes(x = reorder(team, ast_tov), y = ast_tov)) +
  geom_col(width = 0.8, fill = "#045A8D") +
  coord_flip() +
  geom_text(aes(label = ast_tov), hjust = 1.2,
            vjust = 0.1, color = "White") +
  theme_ipsum() +
  ggtitle("Assist to turnover") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(face = bold))

plot_grid(af, at)

#true shooting percentage, ts% - pts/ (2 *(fga + 0.44 * fta))
ts <- nba_stats %>%
  mutate(ts_pct = pts_scored / (2 *(fga + 0.44 * fta))) %>%
  arrange(desc(ts_pct)) %>%
  select(team, season, ts_pct)

ts$ts_pct <- round(ts$ts_pct, 3)

#ts for warriors vs league, to create line chart
tsw <- ts %>% filter(team == "Warriors")
tsr <- ts %>% filter(team != "Warriors") %>%
  group_by(season) %>%
  transmute(ts_pct = mean(ts_pct), team = "Rest of the league") %>% 
  select(team, season, ts_pct)
tsr <- unique(tsr)
tsl <- rbind(tsw, tsr)
tsl$ts_pct <- round(tsl$ts_pct, 3)

ts <- ts %>% head(8)
bold <- ifelse(ts$team == "Warriors", "bold", "plain")
bold <- rev(bold)

ts$team <- paste(ts$season, ts$team, sep = " ")

#2014 warriors at 17 second highest in 2014 clippers at 38 with 0.560

tsg <- ggplot(ts, aes(x = reorder(team, ts_pct), y = ts_pct)) +
  geom_col(width = 0.8, fill = "#045A8D") +
  coord_flip() +
  geom_text(aes(label = ts_pct), hjust = 1.2,
            vjust = 0.1, color = "White") +
  theme_ipsum() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(face = bold))

#line chart for warriors ts and league ts
tsg2 <- ggplot(tsl, aes(x = season, y = ts_pct)) +
  geom_line(aes(color = team)) +
  scale_color_manual(values = c("darkred", "#045A8D")) +
  geom_text(aes(label = ts_pct), vjust = 1, size = 3) +
  ylim(0.4, 0.7) +
  theme_ipsum() +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank()) +
  theme(legend.position = "bottom")
  

plot_grid(tsg, tsg2)


#Effective Field Goal %, efg% = (2pt FGM + 1.5 * 3pt FGM) / FGA
efg <- nba_stats %>%
  mutate(efg_pct = (((fgm - nba_stats[[12]]) +
                       1.5 *nba_stats[[12]]) / fga )) %>%
  arrange(desc(efg_pct)) %>%
  select(team, season, efg_pct)

efg$efg_pct <- round(efg$efg_pct, 3)

#2015 warriors, 2014 warriors at 12, 
#next highest in 2014 is clippers at 28 at 0.527

efgw <- efg %>% filter(team == "Warriors")
efgr <- efg %>% filter(team != "Warriors") %>%
  group_by(season) %>%
  transmute(efg_pct = mean(efg_pct), team = "Rest of the league") %>% 
  select(team, season, efg_pct)
efgr <- unique(efgr)
efgl <- rbind(efgw, efgr)
efgl$efg_pct <- round(efgl$efg_pct, 3)

efg <- efg %>% head(8)
bold <- ifelse(efg$team == "Warriors", "bold", "plain")
bold <- rev(bold)

efg$team <- paste(efg$season, efg$team, sep = " ")

efgg <-  ggplot(efg, aes(x = reorder(team, efg_pct), y = efg_pct)) +
  geom_col(width = 0.8, fill = "#045A8D") +
  coord_flip() +
  geom_text(aes(label = efg_pct), hjust = 1.2,
            vjust = 0.1, color = "White") +
  theme_ipsum() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(face = bold))

efgg2 <- ggplot(efgl, aes(x = season, y = efg_pct)) +
  geom_line(aes(color = team)) +
  scale_color_manual(values = c("darkred", "#045A8D")) +
  geom_text(aes(label = efg_pct), vjust = 1, size = 3) +
  ylim(0.4, 0.7) +
  theme_ipsum() +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank()) +
  theme(legend.position = "bottom")

plot_grid(efgg, efgg2)
