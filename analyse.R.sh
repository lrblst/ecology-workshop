#charger les packages
library(tidyverse)
library(ratdat)

#graphique
ggplot(data = complete_old, aes(x = weight, y = hindfoot_length)) +
geom_point()

