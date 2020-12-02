library(genius)
library(dplyr)
library(spotifyr)

id <- "355e1587681d43b9b387908de7f303df"
secret <- "d9096909cf974815af9f855efbe48445"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token