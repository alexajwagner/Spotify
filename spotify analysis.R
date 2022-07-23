library(jsonlite)
library(lubridate)
library(gghighlight)
library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)
library(Rspotify)

###Authorization stuffs###
spotifyOAuth("doctor",
             "6b67b58626a54f3aaac7922ed3d55d5e",
             "9b9e9e225ebd4abd8fc08dd8309f8731")

get_spotify_authorization_code(client_id = ("6b67b58626a54f3aaac7922ed3d55d5e"),
                               client_secret = ("9b9e9e225ebd4abd8fc08dd8309f8731"),
                               scope = scopes())
#finallyyyyyy
get_my_currently_playing(
  authorization = 
    get_spotify_authorization_code(client_id = ("6b67b58626a54f3aaac7922ed3d55d5e"),
                                   client_secret = ("9b9e9e225ebd4abd8fc08dd8309f8731"),
                                   scope = ("user-modify-playback-state")))


autho <- get_spotify_authorization_code(client_id = ("6b67b58626a54f3aaac7922ed3d55d5e"),
                                        client_secret = ("9b9e9e225ebd4abd8fc08dd8309f8731"),
                                        scope = ("user-modify-playback-state"))
#cool this worked
get_my_currently_playing(authorization = autho)

myplaylists <- get_user_playlists("lexijewell", limit = 20, offset = 0, autho)
view(mylists)

mediumtops <- get_my_top_artists_or_tracks(type = "tracks", 
                                           limit = 50, 
                                           offset = 0, 
                                           time_range = "medium_term", 
                                           autho)
view(mediumtops)
longtops <- get_my_top_artists_or_tracks(type = "tracks", 
                                         limit = 50, 
                                         offset = 0, 
                                         time_range = "long_term", 
                                         autho)
view(longtops)
shorttops <- get_my_top_artists_or_tracks(type = "tracks", 
                                          limit = 50, 
                                          offset = 0, 
                                          time_range = "short_term", 
                                          autho)
view(shorttops)

#creating short term playlist
create_playlist("lexijewell", 
                "shortterm from R analysis", 
                public = T, 
                collaborative = FALSE,
                description = "omg i did this in R// top songs from 4 weeks", 
                authorization =autho)

#have to remove spotify:track: so r can read uri
shorturis<-gsub("spotify:track:","",as.character(shorttops$uri))
view(shorturis)

#adding shortterm tops songs to a playlists
add_tracks_to_playlist("0vrtV0a5NWGt5hs1WoWeux",
                       shorturis,
                       authorization = autho)
# medium playlist
mediumuris<-gsub("spotify:track:","",as.character(mediumtops$uri))
view(mediumuris)

create_playlist("lexijewell", 
                "medium_term from R analysis", 
                public = T, 
                collaborative = FALSE,
                description = "omg i did this in R// top songs from 6 months", 
                authorization =autho)

add_tracks_to_playlist("5mp7ucvWbuEnCv3bke5UXt",
                       mediumuris,
                       authorization = autho)

#longterm playlist
longuris<-gsub("spotify:track:","",as.character(longtops$uri))
view(longuris)

create_playlist("lexijewell", 
                "long_term from R analysis", 
                public = T, 
                collaborative = FALSE,
                description = "omg i did this in R// top songs from a few years", 
                authorization =autho)

add_tracks_to_playlist("0Sk1QHQUUtjREzARcQPkXx",
                       longuris,
                       authorization = autho)

###plotting keys of short, medium, and long term###
#medium term key plot
#track feats of medium range playlist 
medfeats <- get_track_audio_features(mediumtops$id, get_spotify_access_token(
  client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
  client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))

#track vs key for medium range
medfeats$key <- as.factor(medfeats$key)
ggplot(medfeats, aes(x = key, fill = key))+
  theme_bw()+
  geom_bar()+
  scale_fill_discrete(name="Key", labels=c("0-C", "1-C# Db", "2-D","3-D# Eb","4-E","5-F","6-F# Gb","7-G","8-G# Ab","9-A","10-A# Bb","11-B"))+
  labs(x = "Keys of tracks",
       y = "Number of tracks",
       title = "Tracks Vs. Keys -Medium Term")

#long term key plot
#track feats of medium range playlist 
longfeats <- get_track_audio_features(longtops$id, get_spotify_access_token(
  client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
  client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))

#track vs key for medium range
longfeats$key <- as.factor(longfeats$key)
ggplot(longfeats, aes(x = key, fill = key))+
  theme_bw()+
  geom_bar()+
  scale_fill_discrete(name="Key", labels=c("0-C", "1-C# Db", "2-D","3-D# Eb","4-E","5-F","6-F# Gb","7-G","8-G# Ab","9-A","10-A# Bb","11-B"))+
  labs(x = "Keys of tracks",
       y = "Number of tracks",
       title = "Tracks Vs. Keys -Long Term")

#short term key plot
#track feats of medium range playlist 
shortfeats <- get_track_audio_features(shorttops$id, 
                                       get_spotify_access_token(
                                         client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
                                         client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))

#track vs key for medium range
shortfeats$key <- as.factor(shortfeats$key)
ggplot(shortfeats, aes(x = key, fill = key))+
  theme_bw()+
  geom_bar()+
  scale_fill_discrete(name="Key", labels=c("0-C", "1-C# Db", "2-D","3-D# Eb","4-E","5-F","6-F# Gb","7-G","8-G# Ab","9-A","10-A# Bb","11-B"))+
  labs(x = "Keys of tracks",
       y = "Number of tracks",
       title = "Tracks Vs. Keys -Short Term")

#new releases// ehh not working
new <- get_new_releases(country = NULL,
                        limit = 20,
                        offset = 0,
                        get_spotify_access_token(
                          client_id = "6b67b58626a54f3aaac7922ed3d55d5e",
                          client_secret = "9b9e9e225ebd4abd8fc08dd8309f8731"))
view(new)

newuris<-gsub("spotify:album:","",as.character(new$uri))
view(newuris)

create_playlist("lexijewell", 
                "new songs from code", 
                public = T, 
                collaborative = FALSE,
                description = "blah blah blah words", 
                authorization =autho)

add_tracks_to_playlist("2qNU369NcdPvxZduAjsG0a",
                       newuris,
                       authorization = autho)
myplaylists <- get_user_playlists("lexijewell", limit = 40, offset = 0, autho)
view(myplaylists)

get_album("new$id", 
          market = NULL,
          authorization = autho)




#long term top artists
longterm_top_artists <- get_my_top_artists_or_tracks(type = "artists", 
                                                     limit = 50, 
                                                     offset = 0, 
                                                     time_range = "long_term", 
                                                     autho)
view(longterm_top_artists)

#medium term top artists
mediumterm_top_artists <- get_my_top_artists_or_tracks(type = "artists", 
                                                       limit = 50, 
                                                       offset = 0, 
                                                       time_range = "medium_term", 
                                                       autho)
view(mediumterm_top_artists)

#short term top artists
shortterm_top_artists <- get_my_top_artists_or_tracks(type = "artists", 
                                                      limit = 50, 
                                                      offset = 0, 
                                                      time_range = "short_term", 
                                                      autho)
view(shortterm_top_artists)
