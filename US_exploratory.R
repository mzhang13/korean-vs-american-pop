library(dplyr)
library(ggplot2)

theme_set(theme_bw())

us <- read.csv("number1_US.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"))
us <- us[,colSums(is.na(us)) < nrow(us)]

# checking n's
group_by(us, year) %>%
  summarize(weeks = sum(weeks))

# artists with most weeks as lead
lead_artist_weeks <-
  group_by(us, artist) %>%
  summarize(total_weeks = sum(weeks)) %>%
  arrange(desc(total_weeks)) %>%
  print(lead_artist_weeks)  

top_songs <-
  arrange(us, desc(weeks)) %>%
  select(song, artist, year, weeks)
head(top_songs, n = 15)

# histogram of single staying power
ggplot(us) +
  geom_histogram(aes(weeks), bins = 10) + 
  xlab("Weeks") +
  ylab("Songs") +
  ggtitle("Weeks Spent at #1 (Singles)")

# any months look interesting?
group_by(us, month) %>%
  summarize(total_weeks = sum(weeks)) %>%
  arrange(desc(total_weeks))

# is this significant? (nope)
anova(lm(data = us, weeks ~ month))