library(tidyverse)
library(dplyr)
library(rdrop2)
library(ggplot2)
library(ggrepel)
library(ggpubr)


#Load Datasets
iowa_db_token <- readRDS("C:/Users/march/Documents/Baseball/Iowa Baseball/Secondary Pitch Analysis/iowa_db_token.rds")
NCAA_TM_CLEAN <- drop_read_csv("/Datasets/Trackman/NCAA_TM_CLEAN.csv", dtoken = iowa_db_token)
NCAA_TM_UNCLEAN <- drop_read_csv("/Datasets/Trackman/NCAA_TM_UNCLEAN.CSV", dtoken = iowa_db_token)
ncaa <- rbind(NCAA_TM_CLEAN, NCAA_TM_UNCLEAN)




all <- ncaa %>%
  filter(TaggedPitchType == 'Fastball') %>%
  mutate(MovementDiff = round(abs(InducedVertBreak) - abs(HorzBreak),1)) %>%
  group_by(MovementDiff) %>%
  summarize(
    count = n(),
    Whiff =sum(PitchCall == 'StrikeSwinging' & Swing == TRUE, na.rm = TRUE) / sum(Swing == TRUE, na.rm = TRUE),
    SwingPct = sum(Swing == TRUE) / n(),
    ChasePct = sum(InZone == FALSE & Swing == TRUE) / sum(InZone == FALSE),
    ZoneSwingPct = (sum(Swing == TRUE & (InZone == TRUE))) / sum(InZone == TRUE),
    ZoneWhiffPct =(sum(Swing == TRUE & (InZone == TRUE) & PitchCall == 'StrikeSwinging')) / sum(Swing == TRUE & InZone == TRUE, na.rm = TRUE),
    BB = sum(KorBB == "Walk"),
    HBP = sum(PitchCall == "HitByPitch"),
    Single = sum(PlayResult == "Single"),
    Error = sum(PlayResult == "Error"),
    Double = sum(PlayResult == "Double"),
    Triple = sum(PlayResult == "Triple"),
    HomeRun = sum(PlayResult == "HomeRun"),
    AB = sum(KorBB == "Strikeout" | PitchCall == "InPlay")) %>%
  mutate(woba = (.806*BB + .829*HBP + .947*Single + 1.291*Double + 1.609*Triple + 1.891*HomeRun)/(AB+BB+HBP))%>%
  filter(!is.nan(Whiff), MovementDiff <=20 & MovementDiff >= -20, count>20, AB>10) %>%
  pivot_longer(c(Whiff, SwingPct, ChasePct, ZoneSwingPct, ZoneWhiffPct, woba), names_to = "KPI", values_drop_na = TRUE, values_to = "Value")



plot_all <- ggplot(all, aes(x = MovementDiff, y = Value, group = KPI, color = KPI)) + 
  scale_y_continuous(limits = c(0,.8), breaks = seq(0,1,.1)) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5, family = "avenir"), plot.subtitle = element_text(hjust = 0.5, family = "avenir")) +
  geom_smooth(method = "loess", size = 1, se = TRUE) +
  labs(x = "Movement Difference", y = "Percentage", title = "All Fastballs", subtitle = "min. 20 pitches, 10 PAs", color = "KPI") +
  scale_color_manual(values = c("red", "orange", "blue", "green", "gold", "black")) +
  geom_vline(aes(xintercept = -5), size = .5, linetype = "dashed") +
  geom_vline(aes(xintercept = 5), size = .5, linetype = "dashed") +
  annotate("text", x = -10, y = .8, label = "Run", family = "avenir") +
  annotate("text", x = 0, y = .8, label = "Dead Zone", family = "avenir") +
  annotate("text", x = 10, y = .8, label = "Ride", family = "avenir")
plot_all






between90and93 <- ncaa %>%
  filter(TaggedPitchType == 'Fastball', RelSpeed>90, RelSpeed<93) %>%
  mutate(MovementDiff = round(abs(InducedVertBreak) - abs(HorzBreak),1)) %>%
  group_by(MovementDiff) %>%
  summarize(
    count = n(),
    Whiff = sum(PitchCall == 'StrikeSwinging' & Swing == TRUE, na.rm = TRUE) / sum(Swing == TRUE, na.rm = TRUE),
    SwingPct = sum(Swing == TRUE) / n(),
    ChasePct = sum(InZone == FALSE & Swing == TRUE) / sum(InZone == FALSE),
    ZoneSwingPct = (sum(Swing == TRUE & (InZone == TRUE))) / sum(InZone == TRUE),
    ZoneWhiffPct =(sum(Swing == TRUE & (InZone == TRUE) & PitchCall == 'StrikeSwinging')) / sum(Swing == TRUE & InZone == TRUE, na.rm = TRUE),
    BB = sum(KorBB == "Walk"),
    HBP = sum(PitchCall == "HitByPitch"),
    Single = sum(PlayResult == "Single"),
    Error = sum(PlayResult == "Error"),
    Double = sum(PlayResult == "Double"),
    Triple = sum(PlayResult == "Triple"),
    HomeRun = sum(PlayResult == "HomeRun"),
    AB = sum(KorBB == "Strikeout" | PitchCall == "InPlay")) %>%
  mutate(woba = (.806*BB + .829*HBP + .947*Single + 1.291*Double + 1.609*Triple + 1.891*HomeRun)/(AB+BB+HBP))%>%
  filter(!is.nan(Whiff), MovementDiff <=20 & MovementDiff >= -20, count>20, AB>10) %>%
  pivot_longer(c(Whiff, SwingPct, ChasePct, ZoneSwingPct, ZoneWhiffPct, woba), names_to = "KPI", values_drop_na = TRUE, values_to = "Value")


plot_average <- ggplot(between90and93, aes(x = MovementDiff, y = Value, group = KPI, color = KPI)) + 
  scale_y_continuous(limits = c(0,.8), breaks = seq(0,1,.1)) +
  scale_x_continuous(limits = c(-20, 20), breaks = seq(-20, 20, 10)) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5, family = "avenir"), plot.subtitle = element_text(family = "avenir", hjust = 0.5)) +
  geom_smooth(method = "loess", size = 1, se = TRUE) +
  labs(x = "Movement Difference", y = "Percentage", title = "Average Velocity (90-93)", subtitle = "min. 20 pitches, 10 PAs", color = "KPI") +
  scale_color_manual(values = c("red", "orange", "blue", "green", "gold", "black")) +
  geom_vline(aes(xintercept = -5), size = .5, linetype = "dashed") +
  geom_vline(aes(xintercept = 5), size = .5, linetype = "dashed") +
  annotate("text", x = -10, y = .8, label = "Run") +
  annotate("text", x = 0, y = .8, label = "Dead Zone") +
  annotate("text", x = 10, y = .8, label = "Ride")








above93 <- ncaa %>%
  filter(TaggedPitchType == 'Fastball', RelSpeed>93) %>%
  mutate(MovementDiff = round(abs(InducedVertBreak) - abs(HorzBreak),1)) %>%
  group_by(MovementDiff) %>%
  summarize(
    count = n(),
    Whiff = sum(PitchCall == 'StrikeSwinging' & Swing == TRUE, na.rm = TRUE) / sum(Swing == TRUE, na.rm = TRUE),
    SwingPct = sum(Swing == TRUE) / n(),
    ChasePct = sum(InZone == FALSE & Swing == TRUE) / sum(InZone == FALSE),
    ZoneSwingPct = (sum(Swing == TRUE & (InZone == TRUE))) / sum(InZone == TRUE),
    ZoneWhiffPct =(sum(Swing == TRUE & (InZone == TRUE) & PitchCall == 'StrikeSwinging')) / sum(Swing == TRUE & InZone == TRUE, na.rm = TRUE),
    BB = sum(KorBB == "Walk"),
    HBP = sum(PitchCall == "HitByPitch"),
    Single = sum(PlayResult == "Single"),
    Error = sum(PlayResult == "Error"),
    Double = sum(PlayResult == "Double"),
    Triple = sum(PlayResult == "Triple"),
    HomeRun = sum(PlayResult == "HomeRun"),
    AB = sum(KorBB == "Strikeout" | PitchCall == "InPlay")) %>%
  mutate(woba = (.806*BB + .829*HBP + .947*Single + 1.291*Double + 1.609*Triple + 1.891*HomeRun)/(AB+BB+HBP))%>%
  filter(!is.nan(Whiff), MovementDiff <=20 & MovementDiff >= -20, count>20, AB>5) %>%
  pivot_longer(c(Whiff, SwingPct, ChasePct, ZoneSwingPct, ZoneWhiffPct, woba), names_to = "KPI", values_drop_na = TRUE, values_to = "Value")


plot_fast <- ggplot(above93, aes(x = MovementDiff, y = Value, group = KPI, color = KPI)) + 
  scale_y_continuous(limits = c(0,.8), breaks = seq(0,1,.1)) +
  scale_x_continuous(limits = c(-20, 20), breaks = seq(-20, 20, 10)) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5, family = "avenir"), plot.subtitle = element_text(family = "avenir", hjust = 0.5)) +
  geom_smooth(method = "loess", size = 1, se = TRUE) +
  labs(x = "Movement Difference", y = "Percentage", title = "Above Average Velocity (93+)", subtitle = "min. 20 pitches, 5 ABs", color = "KPI") +
  scale_color_manual(values = c("red", "orange", "blue", "green", "gold", "black")) +
  geom_vline(aes(xintercept = -5), size = .5, linetype = "dashed") +
  geom_vline(aes(xintercept = 5), size = .5, linetype = "dashed") +
  annotate("text", x = -10, y = .8, label = "Run") +
  annotate("text", x = 0, y = .8, label = "Dead Zone") +
  annotate("text", x = 10, y = .8, label = "Ride")








highinzone <- ncaa %>%
  filter(TaggedPitchType == 'Fastball', PlateLocHeight>2.35, RelSpeed>88) %>%
  mutate(MovementDiff = round(abs(InducedVertBreak) - abs(HorzBreak),1)) %>%
  group_by(MovementDiff) %>%
  summarize(
    count = n(),
    Whiff = sum(PitchCall == 'StrikeSwinging' & Swing == TRUE, na.rm = TRUE) / sum(Swing == TRUE, na.rm = TRUE),
    SwingPct = sum(Swing == TRUE) / n(),
    ChasePct = sum(InZone == FALSE & Swing == TRUE) / sum(InZone == FALSE),
    ZoneSwingPct = (sum(Swing == TRUE & (InZone == TRUE))) / sum(InZone == TRUE),
    ZoneWhiffPct =(sum(Swing == TRUE & (InZone == TRUE) & PitchCall == 'StrikeSwinging')) / sum(Swing == TRUE & InZone == TRUE, na.rm = TRUE),
    BB = sum(KorBB == "Walk"),
    HBP = sum(PitchCall == "HitByPitch"),
    Single = sum(PlayResult == "Single"),
    Error = sum(PlayResult == "Error"),
    Double = sum(PlayResult == "Double"),
    Triple = sum(PlayResult == "Triple"),
    HomeRun = sum(PlayResult == "HomeRun"),
    AB = sum(KorBB == "Strikeout" | PitchCall == "InPlay")) %>%
  mutate(woba = (.806*BB + .829*HBP + .947*Single + 1.291*Double + 1.609*Triple + 1.891*HomeRun)/(AB+BB+HBP))%>%
  filter(!is.nan(Whiff), MovementDiff <=20 & MovementDiff >= -20, count>20, AB>5) %>%
  pivot_longer(c(Whiff, SwingPct, ChasePct, ZoneSwingPct, ZoneWhiffPct, woba), names_to = "KPI", values_drop_na = TRUE, values_to = "Value")



plot_high <- ggplot(highinzone, aes(x = MovementDiff, y = Value, group = KPI, color = KPI)) + 
  scale_y_continuous(limits = c(0,.8), breaks = seq(0,1,.1)) +
  scale_x_continuous(limits = c(-20, 20), breaks = seq(-20, 20, 10)) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5, family = "avenir"), plot.subtitle = element_text(family = "avenir", hjust = 0.5)) +
  geom_smooth(method = "loess", size = 1, se = TRUE) +
  labs(x = "Movement Difference", y = "Percentage", title = "High Pitches", subtitle = "min. 20 pitches, 5 ABs", color = "KPI") +
  scale_color_manual(values = c("red", "orange", "blue", "green", "gold", "black")) +
  geom_vline(aes(xintercept = -5), size = .5, linetype = "dashed") +
  geom_vline(aes(xintercept = 5), size = .5, linetype = "dashed") +
  annotate("text", x = -10, y = .8, label = "Run") +
  annotate("text", x = 0, y = .8, label = "Dead Zone") +
  annotate("text", x = 10, y = .8, label = "Ride")








lowinzone <- ncaa %>%
  filter(TaggedPitchType == 'Fastball', PlateLocHeight<2.35, RelSpeed>88) %>%
  mutate(MovementDiff = round(abs(InducedVertBreak) - abs(HorzBreak),1)) %>%
  group_by(MovementDiff) %>%
  summarize(
    count = n(),
    Whiff = sum(PitchCall == 'StrikeSwinging' & Swing == TRUE, na.rm = TRUE) / sum(Swing == TRUE, na.rm = TRUE),
    SwingPct = sum(Swing == TRUE) / n(),
    ChasePct = sum(InZone == FALSE & Swing == TRUE) / sum(InZone == FALSE),
    ZoneSwingPct = (sum(Swing == TRUE & (InZone == TRUE))) / sum(InZone == TRUE),
    ZoneWhiffPct =(sum(Swing == TRUE & (InZone == TRUE) & PitchCall == 'StrikeSwinging')) / sum(Swing == TRUE & InZone == TRUE, na.rm = TRUE),
    BB = sum(KorBB == "Walk"),
    HBP = sum(PitchCall == "HitByPitch"),
    Single = sum(PlayResult == "Single"),
    Error = sum(PlayResult == "Error"),
    Double = sum(PlayResult == "Double"),
    Triple = sum(PlayResult == "Triple"),
    HomeRun = sum(PlayResult == "HomeRun"),
    AB = sum(KorBB == "Strikeout" | PitchCall == "InPlay")) %>%
  mutate(woba = (.806*BB + .829*HBP + .947*Single + 1.291*Double + 1.609*Triple + 1.891*HomeRun)/(AB+BB+HBP))%>%
  filter(!is.nan(Whiff), MovementDiff <=20 & MovementDiff >= -20, count>20, AB>5) %>%
  pivot_longer(c(Whiff, SwingPct, ChasePct, ZoneSwingPct, ZoneWhiffPct, woba), names_to = "KPI", values_drop_na = TRUE, values_to = "Value")


plot_low <- ggplot(lowinzone, aes(x = MovementDiff, y = Value, group = KPI, color = KPI)) + 
  scale_y_continuous(limits = c(0,.8), breaks = seq(0,1,.1)) +
  scale_x_continuous(limits = c(-20, 20), breaks = seq(-20, 20, 10)) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5, family = "avenir"), plot.subtitle = element_text(family = "avenir", hjust = 0.5)) +
  geom_smooth(method = "loess", size = 1, se = TRUE) +
  labs(x = "Movement Difference", y = "Percentage", title = "Low Pitches", subtitle = "min. 20 pitches, 5 ABs", color = "KPI") +
  scale_color_manual(values = c("red", "orange", "blue", "green", "gold", "black")) +
  geom_vline(aes(xintercept = -5), size = .5, linetype = "dashed") +
  geom_vline(aes(xintercept = 5), size = .5, linetype = "dashed") +
  annotate("text", x = -10, y = .8, label = "Run") +
  annotate("text", x = 0, y = .8, label = "Dead Zone") +
  annotate("text", x = 10, y = .8, label = "Ride")

plot_all
plot_average
plot_fast
plot_high
plot_low









#Find a dead zone pitcher (Lavelle, Quinn)
pitch <- ncaa %>%
  filter(TaggedPitchType == "Fastball") %>%
  group_by(Pitcher) %>%
  summarize(
    'Count' = n(),
    'VB' = round(mean(InducedVertBreak, na.rm = TRUE),1),
    'HB' = round(abs(mean(HorzBreak, na.rm = TRUE)),1)) %>%
  mutate(MovementDiff = VB-HB) %>%
  filter(MovementDiff > -1 & MovementDiff < 1 & Count > 300)

dead_zone_pitcher <- ncaa %>%
  filter(Pitcher == "Lavelle, Quinn", TaggedPitchTypeAbbr == "FB" | TaggedPitchTypeAbbr == "CH" | TaggedPitchTypeAbbr == "SL")


dead_zone_plot <- ggplot(data = dead_zone_pitcher, aes(x = -HorzBreak, y = InducedVertBreak)) + 
  labs(title = "Pitch Movement") + 
  scale_x_continuous(limits = c(-30, 30),  breaks = c(-30, -20, -10, 0, 10, 20, 30)) + 
  scale_y_continuous(limits = c(-30, 30),  breaks = c(-30, -20, -10, 0, 10, 20, 30)) +
  geom_segment(aes(x = 0, y = -30, xend = 0, yend = 30), size = 1, color = "grey55") + 
  geom_segment(aes(x = -30, y = 0, xend = 30, yend = 0), size = 1, color = "grey55") +
  geom_point(aes(fill = TaggedPitchTypeAbbr), color = "black", pch = 21, alpha = 0.8, size = 4, na.rm = TRUE) + 
  theme_bw() + theme(text = element_text(size = 12,  family = "avenir"), panel.grid = element_line(colour = 'gray90', linetype = 'dashed')) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text = element_text(size = 12), axis.title = element_blank())
  #theme(legend.position = "none")
dead_zone_plot



