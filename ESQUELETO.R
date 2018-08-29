library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(BiodiversityR)
library(vegan)
parks <- read_csv("parks.csv")
species <- read_csv("species.csv")
species.summary <- select(species, `Park Name`, `Category`, `Family`, `Scientific Name`, `Nativeness`)

CONTAR LA CANTIDAD DE VARIABLES DISTINTAS
summarise(Total.Parks, n_distinct(`Park Name`))
summarise(Total.species, n_distinct(`Scientific Name`))
summarise(Total.Parks, n_distinct(`Park Name`))
Total.species %>% group_by(`Scientific Name`) %>% n_distinct()

ORGANIZACIÓN DE DATOS
species.summary <- select(species, "Park Name", "Category", "Family", "Scientific Name", "Nativeness")
Total.Category <- count(species.summary, Category)
Total.species <- count(species.summary, `Scientific Name`) 
Total.Parks <- count(species.summary, `Park Name`)
Total.Family <- count(species.summary, `Family`)

ARMANDO COLUMNA NUEVA A PARTIR DE CATEGORY (FLORA, FAUNA, FUNGI)
Paso 1 SEPARAR CATEGORÍAS
PLANTS1 <- subset(species.summary, Category %in% "Nonvascular Plant")
PLANTS2 <- subset(species.summary, Category %in% "Vascular Plant")
PLANTS3 <- subset(species.summary, Category %in% "Algae")
FUNGI <- subset(species.summary, Category %in% "Fungi")
ANIMAL <- subset(species.summary, Category %in% "Amphibian")
ANIMAL2 <- subset(species.summary, Category %in% "Bird")
ANIMAL3 <- subset(species.summary, Category %in% "Crab/Lobster/Shrimp")
ANIMAL4 <- subset(species.summary, Category %in% "Fish")
ANIMAL5 <- subset(species.summary, Category %in% "Insect")
ANIMAL6 <- subset(species.summary, Category %in% "Invertebrate")
ANIMAL7 <- subset(species.summary, Category %in% "Mammal")
ANIMAL8 <- subset(species.summary, Category %in% "Reptile")
ANIMAL9 <- subset(species.summary, Category %in% "Slug/Snail")
ANIMAL10 <- subset(species.summary, Category %in% "Spider/Scorpion")

Paso 2 AGREGAR COLUMNA
FLORA1 <- mutate(PLANTS1, Group = "Flora")
FLORA2 <- mutate(PLANTS2, Group = "Flora")
FLORA3 <- mutate(PLANTS3, Group = "Flora")
FUNGI.F <- mutate(FUNGI, Group = "Fungi")
FAUNA1 <- mutate(ANIMAL, Group = "Fauna")
FAUNA2 <- mutate(ANIMAL2, Group = "Fauna")
FAUNA3 <- mutate(ANIMAL3, Group = "Fauna")
FAUNA4 <- mutate(ANIMAL4, Group = "Fauna")
FAUNA5 <- mutate(ANIMAL5, Group = "Fauna")
FAUNA6 <- mutate(ANIMAL6, Group = "Fauna")
FAUNA7 <- mutate(ANIMAL7, Group = "Fauna")
FAUNA8 <- mutate(ANIMAL8, Group = "Fauna")
FAUNA9 <- mutate(ANIMAL9, Group = "Fauna")
FAUNA10 <- mutate(ANIMAL10, Group = "Fauna")

PASO 3 UNIR DATAF.FRAME EN FILAS
SPECIES_GROUP <- bind_rows(FLORA1, FLORA2, FLORA3, FUNGI.F, FAUNA1, FAUNA2, FAUNA3, FAUNA4, FAUNA5, FAUNA6, FAUNA7, FAUNA8, FAUNA9, FAUNA10)
FREQUENCY <- SPECIES_GROUP %>% count(`Park Name`, `Group`) FRECUENCIAS DE CADA GRUPO POR GRUPO


GRAFICO BARRAS APILADAS POR GRUPO
ggplot(data=FREQUENCY, aes(x=`Park Name`, y=n, fill=Group)) + geom_bar(stat="identity", alpha=0.8, position=position_identity()) + scale_fill_manual(values=c("darkblue", "turquoise","blue")) + coord_flip() + ggtitle("Distribucion de la Biodiversidad de Especies en los Parques Nacionales de EEUU") + labs(x = "Parques Nacionales", y = "Cantidad de Especies")

GRAFICOS POR SEPARADO
SPECIES_FLORA <- bind_rows(FLORA1, FLORA2, FLORA3)
SPECIES_FUNGI <- bind_rows(FUNGI.F)
SPECIES_FAUNA <- bind_rows(FAUNA1, FAUNA2, FAUNA3, FAUNA4, FAUNA5, FAUNA6, FAUNA7, FAUNA8, FAUNA9, FAUNA10)

FREQUENCY.FLORA <- SPECIES_FLORA %>% count(`Park Name`, `Group`)
FREQUENCY.FUNGI <- SPECIES_FUNGI %>% count(`Park Name`, `Group`)
FREQUENCY.FAUNA <- SPECIES_FAUNA %>% count(`Park Name`, `Group`)

ggplot(data=FREQUENCY.FLORA, aes(x=`Park Name`, y=n, fill=Group)) + geom_bar(stat="identity", position=position_identity()) + scale_fill_manual(values=c("darkblue")) + coord_flip() + ggtitle("Distribucion de la Biodiversidad de Flora en los Parques Nacionales de EEUU") + labs(x = "Parques Nacionales", y = "Cantidad de Especies")
ggplot(data=FREQUENCY.FAUNA, aes(x=`Park Name`, y=n, fill=Group)) + geom_bar(stat="identity", position=position_identity()) + scale_fill_manual(values=c("turquoise")) + coord_flip() + ggtitle("Distribucion de la Biodiversidad de Fauna en los Parques Nacionales de EEUU") + labs(x = "Parques Nacionales", y = "Cantidad de Especies")
ggplot(data=FREQUENCY.FUNGI, aes(x=`Park Name`, y=n, fill=Group)) + geom_bar(stat="identity", position=position_identity()) + scale_fill_manual(values=c("blue")) + coord_flip() + ggtitle("Distribucion de la Biodiversidad de Especies Fungicas en los Parques Nacionales de EEUU") + labs(x = "Parques Nacionales", y = "Cantidad de Especies")


MAPA
map <- get_map(location = 'USA', zoom = 4, maptype = "terrain")
Map.Parks <- ggmap(map) + geom_point(data=parks, aes(x=Longitude, y=Latitude), alpha = 0.7, show_guide = FALSE, size=5)
Map.Parks

group_by(Total.species, n_distinct(`Scientific Name`))

s <- Total.species %>% group_by(`Scientific Name`) %>% n_distinct()
c <- Total.Category %>% group_by(`Category`) %>% n_distinct()
p <- Total.Parks %>% group_by(`Park Name`) %>% n_distinct()
f <- Total.Family %>% group_by(`Family`) %>% n_distinct()

SPECIES_GROUP %>% count(`Group`)
Total.Category %>% group_by(`Category`) %>% n_distinct()

SPECIES_GROUP %>% group_by(`Group`) %>% n_distinct()


