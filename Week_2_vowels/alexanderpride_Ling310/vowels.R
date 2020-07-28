install.packages("tidyverse")
install.packages("ggplot2")


YOURNAME_formants <- read.csv("alexander_pride_formants.csv") #this is our data

View(YOURNAME_formants) #opens up the data so you can see it

YOURNAME_formants_means <- YOURNAME_formants %>% #take the original data
  group_by(phoneme) %>% #tells it to group by each of the vowels
  summarise(F1 = mean(F1), #calculates the mean F1
            F2 = mean(F2)) #calculates the mean F2

#see what the data looks like
YOURNAME_formants_means

ggplot(data = YOURNAME_formants, aes(x = F2, y = F1, colour = phoneme, label = phoneme)) +
  geom_text(data = YOURNAME_formants_means, show.legend = FALSE) +
  stat_ellipse(level = 0.8, show.legend = FALSE) +
  scale_x_reverse(position = "top") +
  scale_y_reverse(position = "right") +
  theme_bw()

James_formants <- read.csv("https://jamesbrandscience.github.io/James_formants.csv")

YOURNAME_formants_binded <- YOURNAME_formants %>%
  rbind(James_formants)

YOURNAME_formants_binded %>%
  group_by(file) %>%
  summarise(n_rows = n())

ggplot(data = YOURNAME_formants_binded, aes(x = F2, y = F1, colour = phoneme, label = phoneme)) +
  geom_text(show.legend = FALSE) +
  stat_ellipse(level = 0.8, show.legend = FALSE) +
  scale_x_reverse(position = "top") +
  scale_y_reverse(position = "right") +
  facet_wrap(~file) + #new layer!
  theme_bw()

#standard Lobanov normalisation - calculate means across all vowels per speaker
YOURNAME_formants_means_lobanov <- YOURNAME_formants_binded %>%
  group_by(file) %>%
  summarise(mean_F1_lobanov = mean(F1),
            mean_F2_lobanov = mean(F2),
            sd_F1_lobanov = sd(F1),
            sd_F2_lobanov = sd(F2))

YOURNAME_formants_binded_lobanov <- YOURNAME_formants_binded %>%
  left_join(YOURNAME_formants_means_lobanov)

YOURNAME_formants_binded_lobanov <- YOURNAME_formants_binded_lobanov %>%
  mutate(F1_lobanov = (F1 - mean_F1_lobanov)/sd_F1_lobanov,
         F2_lobanov = (F2 - mean_F2_lobanov)/sd_F2_lobanov)

ggplot(data = YOURNAME_formants_binded_lobanov, aes(x = F2_lobanov, y = F1_lobanov, colour = phoneme, label = phoneme)) +
  geom_text(show.legend = FALSE) +
  stat_ellipse(level = 0.8, show.legend = FALSE) +
  scale_x_reverse(position = "top") +
  scale_y_reverse(position = "right") +
  facet_wrap(~file) + #new layer!
  theme_bw()
