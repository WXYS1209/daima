getwd()

library(tidyverse)
library(openxlsx)
library(readxl)

team_mapping <- read_excel("./team_mapping_football.xlsx", sheet = "New") 
schedule <- read_excel("../Schedule.xlsx")

filtered_schedule <- schedule %>%
  filter(Property %in% c("Premier League 2024/2025", 
                         "Bundesliga 2024/2025", 
                         "Ligue 1 2024/2025", 
                         "Primera División 2024/2025",
                         "Serie A 2024/2025"))

schedule_with_codes <- filtered_schedule %>%
  left_join(team_mapping, by = c("Home Team" = "Org")) %>%
  rename(Home_Team_Code = Team_Code) %>%
  left_join(team_mapping, by = c("Away Team" = "Org")) %>%
  rename(Away_Team_Code = Team_Code)

any(is.na(schedule_with_codes$League.x))
any(is.na(schedule_with_codes$League.y))

code_mapping = read_excel("./team_mapping_football.xlsx", sheet = "Mapping")

sch = schedule_with_codes %>%
  left_join(code_mapping, by = c("Home_Team_Code" = "Team_Code")) %>%
  rename(Home_Name = Eng_Name) %>%
  left_join(code_mapping, by = c("Away_Team_Code" = "Team_Code")) %>%
  rename(Away_Name = Eng_Name) %>% 
  mutate(
    vs. = paste(Home_Name, "vs.", Away_Name),
    赛事 = case_when(
      grepl("^Premier League", Property) ~ "EPL",
      grepl("^Bundesliga", Property) ~ "Bundesliga",
      grepl("^Ligue 1", Property) ~ "Ligue 1",
      grepl("^Primera División", Property) ~ "La Liga",
      grepl("^Serie A", Property) ~ "Serie A",
      .default = "TBC"
    ),
    开始时间 = 
      if_else(
        hour(hm(`Time (GMT+8)`)) <= 1,
        sprintf("%d:%02d", hour(hm(`Time (GMT+8)`)+hours(24)), minute(hm(`Time (GMT+8)`))),
        `Time (GMT+8)`
      ),
    日期 = if_else(
      hour(hm(开始时间)) >= 24,
      as.character( `Date (GMT+8)` - days(1) ),
      as.character( `Date (GMT+8)` )
    ),
    start = "",
    时间 = "",
    End = hm(开始时间) + hours(2),
    End = if_else(
      hour(End) == 26,
      if_else(
        minute(End) == 0,
        End,
        End - hours(24)
      ),
      if_else(hour(End) > 26, End - hours(24), End)
    ),
    End = sprintf("%d:%02d", hour(End), minute(End)),
    `Live Timeslot` = paste0(开始时间, "-", End),
    主队 = Home_Name,
    客队 = Away_Name
  )


sch_final = sch %>% 
  group_by(赛事) %>% 
  arrange(日期, hm(开始时间)) %>% 
  mutate(
    `Match in Season` = row_number(),
    轮次 = 
      case_when(
        赛事 == "Bundesliga" ~ (`Match in Season` - 1) %/% 9 + 1,
        .default = (`Match in Season` - 1) %/% 10 + 1,
      ),
    Round = sprintf("Round %02d", 轮次)
  ) %>% 
  ungroup() %>% 
  select(
    赛事, 轮次, 时间, 主队, 客队, 日期, 开始时间, 
    vs., start, `Live Timeslot`, Round, `Match in Season`
  )

write.xlsx(sch_final, "D:/wangxiaoyang/Regular_Work/报告/五大联赛/sch_seq1.xlsx")

