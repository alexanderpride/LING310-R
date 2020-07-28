install.packages("tidyverse")

YOURNAME_formants <- read.csv("alexander_pride_formants.csv") #this is our data

View(YOURNAME_formants) #opens up the data so you can see it

ggplot(data = YOURNAME_formants, aes(x = F2, y = F1, colour = phoneme, label = phoneme)) +
  geom_text(show.legend = FALSE) +
  stat_ellipse(level = 0.8, show.legend = FALSE) +
  scale_x_reverse(position = "top") +
  scale_y_reverse(position = "right") +
  theme_bw()
  
YOURNAME_formants_means <- YOURNAME_formants %>%
  group_by(phoneme)
