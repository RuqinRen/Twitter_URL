ds <- read.csv("total7&8.csv", header = T)
names(ds)
length(levels(ds[,26])) #around 13029 unique url/ some share two ore more urls
13029/174893 # 0.074
summary(ds[,18])

#remove original content, only keep RT
library(dplyr)
ds_allurl <- ds %>% 
  filter(str_detect(Text, "RT ")) %>% 
  filter( !(X.M..url_mention_expanded.=="")  ) %>%
  select(3,18,24,26)

ds_onlytwitter <- ds_allurl %>%
  filter(  str_detect(X.M..url_mention_expanded., "https://twitter.com") )  %>%
  rename( mention_twitter_url =X.M..url_mention_expanded.)

ds_onlyexternal<- anti_join(ds_allurl, ds_onlytwitter, by = c("X.M..url_mention_expanded." = "mention_twitter_url") )
colnames(ds_onlyexternal)[4] <- "mention_ext_url"

ds_onlytwitter$mention_ext_url <- NA
ds_onlyexternal$mention_twitter_url <- NA
both_url <- rbind(ds_onlyexternal, ds_onlytwitter)

write.csv(urlds, "url_list.csv")
write.csv(ds_onlytwitter, "url_onlytwitter.csv")
write.csv(ds_onlyexternal, "url_onlyexternal.csv")
write.csv(both_url, "both_url.csv")

table(unlist(Map(grepl, "https://twitter.com", urlds)))
#FALSE  TRUE 
#1224   11805

1224/13029 
#0.09 are from outside of Twitter links. Nearly 90% are linking to another tweet url.

