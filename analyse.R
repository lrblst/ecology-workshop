#charger les packages
library(tidyverse)
library(ratdat)

#graphique
ggplot(data = complete_old, aes(x = weight, y = hindfoot_length)) +
  geom_point(color = "red", alpha = 0.2)

#git add analyse.R / git commit -m "ajout de la couleur"