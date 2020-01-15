library(tidyverse)
# Get the Data

dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')


# Top breed in each state
dogState <- dog_descriptions %>%
  filter(str_length(contact_state)==2) %>% #select states with 2 letters
  count(contact_state, breed_primary) %>% #make count of each state/breed
  arrange(contact_state, desc(n)) %>% #sort by state/n
  group_by(contact_state) %>% #group-by state for slice
  slice(1) %>% #select top in each state
  ungroup() %>%
  rename(abbr=contact_state) #rename for later merge

library(usmap)


usmap_withdogs <- us_map() %>%
  left_join(dogState, by="abbr")

usmap_centroid <- usmap_withdogs %>%
  group_by(abbr) %>%
  summarise(meanx=mean(x),
            meany=mean(y))


ggplot() + 
  geom_polygon(data=usmap_withdogs,
               aes(x=x, y=y,  group=group, fill=breed_primary), colour="black") +
  geom_text(data=usmap_centroid, aes(x=meanx,y=meany, label=abbr)) +
  theme(legend.position="bottom") +
  coord_equal()


