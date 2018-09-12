library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(vegan)
library(forcats)
library(raster)
library(animation)
library("psych")
library("ggplot2")
library("car")
library("Hmisc")
library("corrplot")
library(png)
library(normtest)
library(nortest)
library(moments)
library(readr)
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

ggplot(data=FREQUENCY.FLORA, aes(x=`Park Name`, y=n, fill=Group)) + geom_bar(stat="identity", position=position_identity()) + scale_fill_manual(values=c("darkblue")) + coord_flip() + ggtitle("Riqueza Plantas en Parques Nacionales") + labs(x = "Parques Nacionales", y = "Cantidad de Especies")
ggplot(data=FREQUENCY.FAUNA, aes(x=`Park Name`, y=n, fill=Group)) + geom_bar(stat="identity", position=position_identity()) + scale_fill_manual(values=c("turquoise")) + coord_flip() + ggtitle("Distribucion de la Biodiversidad de Fauna en los Parques Nacionales de EEUU") + labs(x = "Parques Nacionales", y = "Cantidad de Especies")
ggplot(data=FREQUENCY.FUNGI, aes(x=`Park Name`, y=n, fill=Group)) + geom_bar(stat="identity", position=position_identity()) + scale_fill_manual(values=c("blue")) + coord_flip() + ggtitle("Distribucion de la Biodiversidad de Especies Fungicas en los Parques Nacionales de EEUU") + labs(x = "Parques Nacionales", y = "Cantidad de Especies")

PRUEBA
ggplot(data=FREQUENCY.FLORA, aes(x=reorder('Park Name', -n), y=n, fill=Group)) + geom_bar(stat="identity", position=position_identity()) + scale_fill_manual(values=c("darkblue")) + coord_flip() + ggtitle("Riqueza Plantas en Parques Nacionales") + labs(x = "Parques Nacionales", y = "Cantidad de Especies")


ggplot(species, aes(x=Category,y=reorder(choices,ratings), fill = choices)) +
  geom_bar(position = "dodge", stat = "identity") + coord_flip() +
  scale_x_discrete(limits = rev(levels(species$Category)))

MAPA
map <- get_map(location = 'USA', zoom = 4, maptype = "terrain")
Map.Parks.Flora <- ggmap(map) + geom_point(data=ACRES.FLORA, aes(x=Longitude, y=Latitude, colour=`n`), alpha = 0.8, show_guide = TRUE, size=4) + scale_colour_continuous(low = "yellow", high = "green", space = "Lab", guide = "colorbar")
Map.Parks.Flora

map <- get_map(location = 'USA', zoom = 4, maptype = "terrain")
Map.Parks.Flora <- ggmap(map) + geom_point(data=ACRES.FAUNA, aes(x=Longitude, y=Latitude, colour=`n`), alpha = 0.8, show_guide = TRUE, size=4) + scale_colour_continuous(low = "yellow", high = "green", space = "Lab", guide = "colorbar")
Map.Parks.Flora

group_by(Total.species, n_distinct(`Scientific Name`))

s <- Total.species %>% group_by(`Scientific Name`) %>% n_distinct()
c <- Total.Category %>% group_by(`Category`) %>% n_distinct()
p <- Total.Parks %>% group_by(`Park Name`) %>% n_distinct()
f <- Total.Family %>% group_by(`Family`) %>% n_distinct()

SPECIES_GROUP %>% count(`Group`)
Total.Category %>% group_by(`Category`) %>% n_distinct()

SPECIES_GROUP %>% group_by(`Group`) %>% n_distinct()



Total.Fungi.Native <- count(SPECIES_FUNGI, `Nativeness`)

==============================================
MAPA PLOT RIQUEZA ESPECIES PLANTAS 

parks <- read_csv("parks.csv")
species <- read_csv("species.csv")
species.summary <- select(species, `Park Name`, `Category`, `Family`, `Scientific Name`, `Nativeness`)
parks.summary <- select(parks, "Park Name", "Acres", "Longitude", "Latitude")    
summarise(Total.Parks, n_distinct(`Park Name`))
summarise(Total.species, n_distinct(`Scientific Name`))
summarise(Total.Parks, n_distinct(`Park Name`))
Total.Category <- count(species.summary, Category)
Total.species <- count(species.summary, `Scientific Name`) 
Total.Parks <- count(species.summary, `Park Name`)
Total.Family <- count(species.summary, `Family`)
PLANTS1 <- subset(species.summary, Category %in% "Nonvascular Plant")
PLANTS2 <- subset(species.summary, Category %in% "Vascular Plant")
PLANTS3 <- subset(species.summary, Category %in% "Algae")
FLORA1 <- mutate(PLANTS1, Group = "Flora")
FLORA2 <- mutate(PLANTS2, Group = "Flora")
FLORA3 <- mutate(PLANTS3, Group = "Flora")
SPECIES_FLORA <- bind_rows(FLORA1, FLORA2, FLORA3)
FREQUENCY.FLORA <- SPECIES_FLORA %>% count(`Park Name`, `Group`)
ACRES.FLORA <- full_join(FREQUENCY.FLORA, parks.summary)
map <- get_map(location = 'USA', zoom = 4, maptype = "terrain")
Map.Parks.Flora <- ggmap(map) + geom_point(data=ACRES.FLORA, aes(x=Longitude, y=Latitude, colour=`n`), alpha = 0.8, show_guide = TRUE, size=4) + scale_colour_continuous(low = "yellow", high = "dark green", space = "Lab", guide = "colorbar") + ggtitle("Riqueza Plantas en Parques Nacionales") + labs(x = "Longitud", y = "Latitud")
Map.Parks.Flora



MAPA PLOT RIQUEZA ESPECIES ANIMALES

parks <- read_csv("parks.csv")
species <- read_csv("species.csv")
species.summary <- select(species, `Park Name`, `Category`, `Family`, `Scientific Name`, `Nativeness`)
parks.summary <- select(parks, "Park Name", "Acres", "Longitude", "Latitude")    
summarise(Total.Parks, n_distinct(`Park Name`))
summarise(Total.species, n_distinct(`Scientific Name`))
summarise(Total.Parks, n_distinct(`Park Name`))
Total.Category <- count(species.summary, Category)
Total.species <- count(species.summary, `Scientific Name`) 
Total.Parks <- count(species.summary, `Park Name`)
Total.Family <- count(species.summary, `Family`)
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
SPECIES_FAUNA <- bind_rows(FAUNA1, FAUNA2, FAUNA3, FAUNA4, FAUNA5, FAUNA6, FAUNA7, FAUNA8, FAUNA9, FAUNA10)
FREQUENCY.FAUNA <- SPECIES_FAUNA %>% count(`Park Name`, `Group`)
ACRES.FAUNA <- full_join(FREQUENCY.FLORA, parks.summary)
map <- get_map(location = 'USA', zoom = 4, maptype = "terrain")
Map.Parks.Fauna <- ggmap(map) + geom_point(data=ACRES.FAUNA, aes(x=Longitude, y=Latitude, colour=`n`), alpha = 0.8, show_guide = TRUE, size=4) + scale_colour_continuous(low = "yellow", high = "blue", space = "Lab", guide = "colorbar") + ggtitle("Riqueza Animales en Parques Nacionales") + labs(x = "Longitud", y = "Latitud")
Map.Parks.Fauna

MAPA PLOT RIQUEZA ESPECIES HONGOS

parks <- read_csv("parks.csv")
species <- read_csv("species.csv")
species.summary <- select(species, `Park Name`, `Category`, `Family`, `Scientific Name`, `Nativeness`)
parks.summary <- select(parks, "Park Name", "Acres", "Longitude", "Latitude")    
summarise(Total.Parks, n_distinct(`Park Name`))
summarise(Total.species, n_distinct(`Scientific Name`))
summarise(Total.Parks, n_distinct(`Park Name`))
Total.Category <- count(species.summary, Category)
Total.species <- count(species.summary, `Scientific Name`) 
Total.Parks <- count(species.summary, `Park Name`)
Total.Family <- count(species.summary, `Family`)
FUNGI <- subset(species.summary, Category %in% "Fungi")
FUNGI.F <- mutate(FUNGI, Group = "Fungi")
SPECIES_FUNGI <- bind_rows(FUNGI.F)
FREQUENCY.FUNGI <- SPECIES_FUNGI %>% count(`Park Name`, `Group`)
ACRES.FUNGI <- inner_join(FREQUENCY.FUNGI, parks.summary)
map <- get_map(location = 'USA', zoom = 4, maptype = "terrain")
Map.Parks.Fungi <- ggmap(map) + geom_point(data=ACRES.FUNGI, aes(x=Longitude, y=Latitude, colour=`n`), alpha = 0.8, show_guide = TRUE, size=4) + scale_colour_continuous(low = "yellow", high = "brown", space = "Lab", guide = "colorbar") + ggtitle("Riqueza Hongos en Parques Nacionales") + labs(x = "Longitud", y = "Latitud")
Map.Parks.Fungi


MODELO
ggplot(ACRES.FLORA, aes(x=Acres, y=n)) + geom_point() + ggtitle("Diagrama de Dispersion Flora por Hectareas") + xlab("Hectareas") + ylab("Riqueza de Especies") 
CORR.FLORA <- round(cor(ACRES.FLORA[,3:4], method = "spearman"),2)
corrplot(CORR.FLORA, method = "ellipse")

ggplot(ACRES.FAUNA, aes(x=Acres, y=n)) + geom_point() + ggtitle("Diagrama de Dispersion Fauna por Hectareas") + xlab("Hectareas") + ylab("Riqueza de Especies") 
CORR.FAUNA <- round(cor(ACRES.FAUNA[,3:4], method = "spearman"),2)
corrplot(CORR.FAUNA, method = "ellipse")

ggplot(ACRES.FUNGI, aes(x=Acres, y=n)) + geom_point() + ggtitle("Diagrama de Dispersion Hongos por Hectareas") + xlab("Hectareas") + ylab("Riqueza de Especies") 
CORR.FUNGI <- round(cor(ACRES.FUNGI[,3:4], method = "spearman"),2)
corrplot(CORR.FUNGI, method = "ellipse")



CHUNK DISPERSIÓN FUNGI
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(tidyr)
library(readr)
library(rmarkdown)
parks <- read_csv("parks.csv")
species <- read_csv("species.csv")
species.summary <- select(species, `Park Name`, `Category`, `Family`, `Scientific Name`, `Nativeness`)
parks.summary <- select(parks, "Park Name", "Acres", "Longitude", "Latitude")    
summarise(Total.Parks, n_distinct(`Park Name`))
summarise(Total.species, n_distinct(`Scientific Name`))
summarise(Total.Parks, n_distinct(`Park Name`))
Total.Category <- count(species.summary, Category)
Total.species <- count(species.summary, `Scientific Name`) 
Total.Parks <- count(species.summary, `Park Name`)
Total.Family <- count(species.summary, `Family`)
FUNGI <- subset(species.summary, Category %in% "Fungi")
FUNGI.F <- mutate(FUNGI, Group = "Fungi")
SPECIES_FUNGI <- bind_rows(FUNGI.F)
FREQUENCY.FUNGI <- SPECIES_FUNGI %>% count(`Park Name`, `Group`)
ACRES.FUNGI <- inner_join(FREQUENCY.FUNGI, parks.summary)
ggplot(ACRES.FUNGI, aes(x=Acres, y=n)) + geom_point() + ggtitle("Diagrama de Dispersion") + xlab("Hectareas") + ylab("Riqueza de Especies") 


CHUNK DISPERSIÓN FAUNA
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(tidyr)
library(readr)
library(rmarkdown)
parks <- read_csv("parks.csv")
species <- read_csv("species.csv")
species.summary <- select(species, `Park Name`, `Category`, `Family`, `Scientific Name`, `Nativeness`)
parks.summary <- select(parks, "Park Name", "Acres", "Longitude", "Latitude")    
summarise(Total.Parks, n_distinct(`Park Name`))
summarise(Total.species, n_distinct(`Scientific Name`))
summarise(Total.Parks, n_distinct(`Park Name`))
Total.Category <- count(species.summary, Category)
Total.species <- count(species.summary, `Scientific Name`) 
Total.Parks <- count(species.summary, `Park Name`)
Total.Family <- count(species.summary, `Family`)
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
SPECIES_FAUNA <- bind_rows(FAUNA1, FAUNA2, FAUNA3, FAUNA4, FAUNA5, FAUNA6, FAUNA7, FAUNA8, FAUNA9, FAUNA10)
FREQUENCY.FAUNA <- SPECIES_FAUNA %>% count(`Park Name`, `Group`)
ACRES.FAUNA <- full_join(FREQUENCY.FLORA, parks.summary)
ggplot(ACRES.FAUNA, aes(x=Acres, y=n)) + geom_point() + ggtitle("Diagrama de Dispersion") + xlab("Hectareas") + ylab("Riqueza de Especies") 


CHUNK DISPERSIÓN FLORA
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(tidyr)
library(readr)
library(rmarkdown)
parks <- read_csv("parks.csv")
species <- read_csv("species.csv")
species.summary <- select(species, `Park Name`, `Category`, `Family`, `Scientific Name`, `Nativeness`)
parks.summary <- select(parks, "Park Name", "Acres", "Longitude", "Latitude")    
summarise(Total.Parks, n_distinct(`Park Name`))
summarise(Total.species, n_distinct(`Scientific Name`))
summarise(Total.Parks, n_distinct(`Park Name`))
PLANTS1 <- subset(species.summary, Category %in% "Nonvascular Plant")
PLANTS2 <- subset(species.summary, Category %in% "Vascular Plant")
PLANTS3 <- subset(species.summary, Category %in% "Algae")
FLORA1 <- mutate(PLANTS1, Group = "Flora")
FLORA2 <- mutate(PLANTS2, Group = "Flora")
FLORA3 <- mutate(PLANTS3, Group = "Flora")
SPECIES_FLORA <- bind_rows(FLORA1, FLORA2, FLORA3)
FREQUENCY.FLORA <- SPECIES_FLORA %>% count(`Park Name`, `Group`)
ACRES.FLORA <- full_join(FREQUENCY.FLORA, parks.summary)
ggplot(ACRES.FLORA, aes(x=Acres, y=n)) + geom_point() + ggtitle("Diagrama de Dispersion Flora por Hectareas") + xlab("Hectareas") + ylab("Riqueza de Especies") 

=============================================
  
NORMAL DISTRIBUTION
library(normtest)
library(nortest)
library(moments)
A <- c(ACRES.FUNGI$n)
B <- c(ACRES.FUNGI$Acres)
lillie.test(A)
lillie.test(B)
cor(ACRES.FUNGI$n, ACRES.FUNGI$Acres)
round(cor.test(A, B, method = "pearson")$p.value, 2)

C <- c(ACRES.FAUNA$n)
D <- c(ACRES.FAUNA$Acres)
lillie.test(C)
lillie.test(D)
cor(ACRES.FAUNA$n, ACRES.FAUNA$Acres)
round(cor.test(C, D, method = "pearson")$p.value, 2)


Z <- c(ACRES.FLORA$n)
W <- c(ACRES.FLORA$Acres)
lillie.test(Z)
lillie.test(W)
cor(ACRES.FLORA$n, ACRES.FLORA$Acres)
round(cor.test(Z, W, method = "pearson")$p.value, 2)

CORRELACIÓN rho spearman de cada uno chunk inline code
Fungi <- cor(A, B, method= "pearson")
Fauna <- cor(C, D, method= "pearson")
Flora <- cor(Z, W, method= "pearson")



round(cor.test(C, D)$p.value, 2)
