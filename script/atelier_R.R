#EXPLORATION DE DONNEES
library(ratdat)
library(tidyverse)
?complete_old
summary(complete_old)
head(complete_old)
#tibble = un datafram avec plus d'informations et qui les présente de différentes façons
# dataframe, jeu de donnéess = pleins de vecteurs mis ensemble; tous les vecteurs ont un meme type par colonne
#matrice = toute valeurs ont meme type, alors que dataframe plusieurs types de vecteurs 
str(complete_old)

#UTILISATION GGPLOT
library(ggplot2) #ggplot = grammar of graphics 
#structure de base = ggplot( data = data, mappping = aes (fonction quu permet de specifier les variables qu'on veut y mettre,))
# + geom_function = comment faire le graph
ggplot(data = complete_old, mapping = aes(x = weight, y = hindfoot_length)) +
       geom_point(alpha = 0.1, color = "purple")
completeld <- filter(complete_old, !is.na(weight), !is.na(hindfoot_length))
ggplot(data = complete_old, mapping = aes(x = weight, y = hindfoot_length, 
       color = plot_type, shape = sex)) +
  geom_point(alpha = 0.1, )
ggplot(data = complete_old, mapping = aes(x = weight, y = hindfoot_length, 
                                          color = plot_type, shape = sex)) +
  geom_point(alpha = 0.3, ) + 
  scale_color_viridis_d() + #changement de couleur des donnes
  scale_x_log10() #changement d'echelle sur x

#pour changer de type de graph
ggplot(data = complete_old, mapping = aes(x = plot_type, y = hindfoot_length, color = plot_type)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) + #bruits aleatoire
  scale_x_discrete(labels = label_wrap_gen(width = 10)) #pour mieux voir les titres

ggplot(data = complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.3, aes(color = plot_type)) + #si on veut seulement colorer les points et non les plot
  geom_boxplot(outlier.shape = NA) + #point a l'extreme
  scale_x_discrete(labels = label_wrap_gen(width = 10)) #pour mieux voir les titres

ggplot(data = complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.3, aes(color = plot_type)) + #si on veut seulement colorer les points et non les plot
  geom_boxplot(outlier.shape = NA, fill = NA) + #rendre transparent
  scale_x_discrete(labels = label_wrap_gen(width = 10)) #pour mieux voir les titres

#autre type de graph
ggplot(data = complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.3, aes(color = plot_type)) +
  geom_violin(fill = "slateblue2")

#theme
ggplot(data = complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.3, aes(color = plot_type)) + 
  geom_boxplot(outlier.shape = NA, fill = NA) + 
  facet_wrap(vars(sex, ncol = 1)) + #ajouter un graph
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme(legend.position = "none") +
  labs(x = "plot_type", y = "hindfoot_length")
plotfinal <- ggplot(data = complete_old, mapping = aes(x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.3, aes(color = plot_type)) + 
  geom_boxplot(outlier.shape = NA, fill = NA) + 
  facet_wrap(vars(sex, ncol = 1)) + #ajouter un graph
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme(legend.position = "none") +
  labs(x = "plot_type", y = "hindfoot_length")
plotfinal

ggsave(filename = "figures/plotfinal.png",
       plot = plotfinal,
       height = 6,
       width = 8)




#tidyverse
surveys <- read_csv("data/raw/surveys_complete_77_89.csv")
str(surveys)
#select = selectionner des colonnes, filter = selectioner des lignes
## mutate = creer des nouvelles colonnes, group by et summurise = permettent de
#calculer des stat siommair sur notr jeu de donne

###select
select(surveys, plot_id, species_id)
select(surveys, c(3,4)) #pas bonne idee,  la position peut changer 
select(surveys, -plot_id) #toutes les colonnes sauf une
select(surveys, where(is.numeric)) #seulement les variables numeriques
select(surveys, where(anyNA)) #toutes les colonnes avec valeurs mqnauqntes

#filter
filter(surveys, year == 1988) #seulement les odnnées de 1988
filter(surveys, species_id %in% c("RM", "DO"))

#filtrer ligne 
surveys_year <- filter(surveys, year>= 1980, year <=1985)
surveys_year
select(surveys_year, year, month, species_id, plot_id)
#ça peut devenir melangeant si y'a trop d'objets

#emboiter les fonctions 
select(filter(surveys, year>= 1980, year <=1985), year, month, species_id, plot_id)
#moins clair a lire

surveys %>% 
  filter(year == (1980:1985)) %>% 
  select(year, month, species_id, plot_id)
#une pipeline permet de faciliter la lecture et faciliter la vie lol

surveys %>% 
  filter(year == 1988) %>% 
  select(record_id, month, species_id)

#mutate
surveys %>% 
  mutate(weight_kg = weight / 1000,
         weight_lbs = weight_kg * 2.2) %>% 
  relocate(weight_kg, .after = record_id) %>% 
  relocate(weight_lbs, .after = weight_kg)

library(lubridate)
surveys %>% 
  mutate(date = paste(year, month, day, sep = "-")) %>% 
  relocate(date, .after = year) 

#group by et sumurise
surveys %>% 
  group_by(sex) %>% 
  summarise(mean.weight = mean(weight), na.rm = T, count = n())

#dataframe du nombre danimaux de chaque sex par jour
surveys %>% 
  filter(!is.na(sex)) %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
  relocate(date, .after = year) %>% 
  group_by(sex, date) %>% 
  summarise(count =n()) %>% 
  ggplot(mapping = aes(x = date, y = count, color = sex)) +
  geom_line()
