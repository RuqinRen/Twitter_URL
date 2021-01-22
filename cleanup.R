ds <- read.csv("total7&8.csv", header = T)
ds2 <- read.csv("totalOct.csv", header = T)
ds3<- read.csv("total11&12.csv", header = T)

names(ds)
colnames(ds2) <- colnames(ds)
colnames(ds3) <- colnames(ds)

ds_large <- rbind(ds, ds2, ds3)
remove(ds, ds2, ds)

length(levels(ds_large[,26])) #around 13029 unique url/ some share two ore more urls

summary(ds_large[,18]) #retweet count

#remove original content, only keep RT
library(dplyr)

ds_allurl <- ds_large %>% 
  filter(str_detect(Text, "RT ")) %>%
  select(3,24,26) %>%
  filter( !(X.M..url_mention_expanded.=="")  ) 
  
ds_onlyexternal <- ds_allurl %>%
  filter( !str_detect(X.M..url_mention_expanded., "https://twitter.com") )  %>%
  rename( mention_ext_url =X.M..url_mention_expanded.) %>%
  distinct(mention_ext_url)

#ds_onlyexternal<- anti_join(ds_allurl, ds_onlytwitter, by = c("X.M..url_mention_expanded." = "mention_twitter_url") )
#colnames(ds_onlyexternal)[4] <- "mention_ext_url"

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
1224/13029 #0.09 are from outside of Twitter links. Nearly 90% are linking to another tweet url.
head(ds[16])
