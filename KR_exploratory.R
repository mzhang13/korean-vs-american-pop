library(dplyr)
library(ggplot2)

theme_set(theme_bw())

kr <- read.csv("number1_KR.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"))
kr <- kr[,colSums(is.na(kr)) < nrow(kr)]

# checking n's
group_by(kr, year) %>%
  summarize(weeks = sum(weeks))
(sum(kr$weeks) - 26) / 52 # 3 away from divisible?

# Idina Menzel
sum(is.na(kr$label))

# artists with most weeks as lead
lead_artist_weeks <-
  group_by(kr, artist) %>%
  summarize(total_weeks = sum(weeks)) %>%
  arrange(desc(total_weeks)) %>%
print(lead_artist_weeks)  

# labels with most #1 weeks
top_labels_weeks <-
  filter(kr, !is.na(label)) %>%
  group_by(label) %>%
  summarize(total_weeks = sum(weeks)) %>%
  arrange(desc(total_weeks))
print(top_labels_weeks)

# all songs with more than 2 weeks at #1
filter(kr, weeks > 2) %>%
  arrange(desc(weeks), desc(year))

# artists by #1 singles that have lasted more than 1 week
group_by(kr, artist) %>%
  summarize(multi_week = sum(weeks > 1)) %>%
  arrange(desc(multi_week))

# histogram of single staying power
ggplot(kr) +
  geom_histogram(aes(weeks), bins = 5) + 
  xlab("Weeks") +
  ylab("Songs") +
  ggtitle("Weeks Spent at #1 (Singles)")

# any months look interesting?
group_by(kr, month) %>%
  summarize(total_weeks = sum(weeks)) %>%
  arrange(desc(total_weeks))

# is this significant? (nope)
anova(lm(data = kr, weeks ~ month))

# what about season? (thinking about songs like Cherry Blossom Ending)
season_determiner <- function(x) {
  if (x %in% c("December", "January", "Feburary")) {
    season <- "winter"
  }
  else if (x %in% c("March", "April", "May")) {
    season <- "spring"
  }
  else if (x %in% c("June", "July", "August")) {
    season <- "summer"
  }
  else if (x %in% c("September", "October", "November")) {
    season <- "autumn"
  }
  else {
    season <- NA
  }
  return(season)
}

kr$season <- unlist(lapply(kr$month, season_determiner))

anova(lm(data = kr, weeks ~ season))
# (nope)

# SM, YG, JYP
big_3 <- filter(kr, label == "SM" | label == "JYP" | label == "YG")
head(big_3)

# percentage of singles from Big 3
nrow(big_3)/nrow(kr) * 100

# best perfomers in the big 3?
group_by(big_3, artist) %>%
  summarize(total_weeks = sum(weeks)) %>%
  arrange(desc(total_weeks))

IU <- filter(kr, artist == "IU")
IU

# all songs with 2+ weeks
multiweek_songs <- filter(kr, weeks > 1) %>%
  arrange(desc(weeks))
multiweek_songs

# more than 2 weeks?
filter(kr, weeks > 2) %>%
  arrange(desc(weeks))

str(kr)
