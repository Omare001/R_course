#install.packages("dplyr")
library(dplyr)
getwd()
penguins <- read.csv(file = "data/penguins.csv")
head(penguins)
dim(penguins)
str(penguins)
summary(penguins)

#selecting colums we want
penguins_select <-  penguins |> 
  select(species,sex,year)

#removing columns we dont want
penguins_remove <- penguins |> 
  select(-bill_length_mm, -species, -year)
#chosing a column
penguins_filter_chinstrap <- penguins |> 
  filter(species =="Chinstrap")

#chosing everythin but Chinstrap
penguins_filter_chinstrap <- penguins |> 
  filter(species!="Chinstrap")
#chosing rows
penguins_filter_chinstrap <- penguins |> 
  filter(species %in% c("Chinstrap","Adelie"))

#filtering numeric values
penguins_filter_chinstrap_200 <- penguins |> 
  filter(flipper_length_mm >= 200)

#filtering multiple condition
penguins_filter_chinstrap_200_adelie <- penguins |> 
  filter(flipper_length_mm >= 200 & species == "Adelie")

#creating a new variable
penguins_ratio <- penguins |> 
  mutate(bill_body_mass_ratio = bill_depth_mm/body_mass_g,
         fixed_weight = body_mass_g + 5)

