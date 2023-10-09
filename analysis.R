library(tidyverse)
DATASET <- read_csv("DATASET.csv",show_col_types = FALSE)
Required <- c("SteamSpyOwners","GenreIsStrategy", "GenreIsRPG")
newDataset = DATASET[Required]
final<-newDataset[(newDataset$GenreIsStrategy=="TRUE" | newDataset$GenreIsRPG=="TRUE"),]


d<-final[!(final$GenreIsStrategy=="TRUE" & final$GenreIsRPG=="TRUE"),]
d <- d %>%
  add_column(Genre = 
               if_else(.$GenreIsRPG =="TRUE", "RPG", "strategy"),
             .after="SteamSpyOwners")
d<-d[!(d$SteamSpyOwners==0),]
d <- d %>%
  add_column(Log_steamplayer = 
               log10(d$SteamSpyOwners))
wilcox.test(d$Log_steamplayer[d$Genre =="strategy"],d$Log_steamplayer[d$Genre =="RPG"])

