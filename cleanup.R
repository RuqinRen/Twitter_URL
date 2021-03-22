library(dplyr)
library(stringr)
library(Matrix)
library(readxl)
library(igraph)
library(lubridate)
# 
# ds <- read.csv("total7&8.csv", header = T)
# ds2 <- read.csv("totalOct.csv", header = T)
# ds3<- read.csv("total11&12.csv", header = T)
# 
# colnames(ds2) <- colnames(ds)
# colnames(ds3) <- colnames(ds)
# 
# ds_large <- rbind(ds, ds2, ds3)
# remove(ds, ds2, ds3)
# names(ds_large)
# 
# write.csv(ds_large, "twitter_ds_large.csv")
# 
# #extract unique urls
# length(levels(as.factor(ds_large[,26]))) #around 59428 unique url/ some share two ore more urls
# 
# summary(ds_large[,18]) #retweet count
# 
# #remove original content, only keep RT
# library(dplyr)
# 
# ds_allurl <- ds_large %>% 
#   filter(str_detect(Text, "RT ")) %>%
#   filter( !(X.M..url_mention_expanded.=="")  ) %>%
#   select(3,24,26)
#   
# 
# ds_onlyexternal <- ds_allurl %>%
#   filter( !str_detect(X.M..url_mention_expanded., "https://twitter.com") )  %>%
#   rename( mention_ext_url =X.M..url_mention_expanded.) %>%
#   distinct(mention_ext_url)
# 
# 
# ds_onlytwitter <- ds_allurl %>%
#   filter( str_detect(X.M..url_mention_expanded., "https://twitter.com") )  %>%
#   rename( mention_twitter_url =X.M..url_mention_expanded.) %>%
#   distinct(mention_twitter_url)
# 
# #ds_onlyexternal<- anti_join(ds_allurl, ds_onlytwitter, by = c("X.M..url_mention_expanded." = "mention_twitter_url") )
# #colnames(ds_onlyexternal)[4] <- "mention_ext_url"
# # 
# # ds_onlytwitter$mention_ext_url <- NA
# # ds_onlyexternal$mention_twitter_url <- NA
# # both_url <- rbind(ds_onlyexternal, ds_onlytwitter)
# 
# write.csv(urlds, "url_list.csv")
# write.csv(ds_onlytwitter, "url_onlytwitter.csv")
# write.csv(ds_onlyexternal, "url_onlyexternal.csv")
# write.csv(both_url, "both_url.csv")
# 
# table(unlist(Map(grepl, "https://twitter.com", urlds)))
# #FALSE  TRUE 
# #1224   11805
# 1224/13029 #0.09 are from outside of Twitter links. Nearly 90% are linking to another tweet url.
# head(ds[16])
# 
# # a <- ds_large %>% 
#  # filter(str_detect(X.M..created_at., "9/"))  #look for september data
# 
# #### add politician, ngo, elite media name lists
# ngo <- read_excel("ngo_unique.xlsx")
# politician <- read_excel("politicianlist.xlsx")
# media <- read_excel("mediaelite.xlsx")
# 
# #keep only twitter handle
# media <- media %>% select(TwitterHandler) %>% rename(TwitterHandle = TwitterHandler)%>% 
#   mutate(entity_type = "media") 
# 
# ngo <- ngo %>% select(`Twitter ID`) %>% slice(-c(25,28,55,84,107)) %>% rename(TwitterHandle =`Twitter ID` ) %>% 
#   mutate(entity_type = "ngo")
#   
# politician <- politician %>% select(TwitterHandle) %>% 
#   mutate(entity_type = "politican") 
# 
bigv_namelist <- rbind(ngo, politician)

#read twitter ds
ds_large <- read.csv("twitter_ds_large.csv")

#get RT or mention edgelist
ds_large <- ds_large %>% rename(receiver_handle = X.M..screen_name., 
                                sender_handle = X.M..user_mention_screen_name.,
                                time_creation = X.M..created_at.)
names(ds_large)

#convert to datetime type
#ds_large$time_creation <- mdy_hm(ds_large$time_creation)

#divide ds into t1 and t2, before 9/31 and after 9/31
#ds_large_beforeSep30 <- ds_large %>% filter(time_creation <= "2019-09-30")
#ds_large_afterSep30 <- ds_large %>% filter(time_creation > "2019-09-30")

#only keep bigv_politician
bigv_politician <- bigv_namelist %>% filter(entity_type == 'politican')
bigv_ngo <- bigv_namelist %>% filter(entity_type == 'ngo')
bigv_ngopolitician <- bigv_namelist %>% filter(entity_type != 'media')
#type: mention edgelist only by politician
#filter names that are in the bigV list
mention_edgelist_onlypolitician <- ds_large %>% select(receiver_handle, sender_handle) %>%
  filter(sender_handle %in% bigv_politician$TwitterHandle) %>%
  filter(receiver_handle %in% bigv_politician$TwitterHandle)

a <- ds_large %>% select(receiver_handle, sender_handle) %>%
  filter(sender_handle %in% bigv_politician$TwitterHandle) %>%
  filter(receiver_handle %in% bigv_ngo$TwitterHandle) %>%
  rename(politician = sender_handle,
         ngo = receiver_handle)

b <- ds_large %>% select(receiver_handle, sender_handle) %>%
  filter(sender_handle %in% bigv_ngo$TwitterHandle) %>%
  filter(receiver_handle %in% bigv_politician$TwitterHandle) %>%
  rename(ngo = sender_handle,
         politician = receiver_handle)

mention_edgelist_ngoandpolitician <- rbind(a,b)
#this gets ngo * politician, non-directed mentionig edgelist
#now convert to politician * politician edgelist

input_twomode_get_onemode_edgelist <- function(twomode_input) {
  net <- graph.edgelist(as.matrix(twomode_input))
  V(net)$type <- bipartite.mapping(net)$type
  onemode_net <- bipartite.projection(net)$proj1  #proj1 for first mode, proj2 for second mode, can modify
  #plot(bipartite.projection(net)$proj2,main="Affilitaton Network") #url one-mode net
  onemode_edgelist <- get.data.frame(onemode_net)
}

a <- input_twomode_get_onemode_edgelist(mention_edgelist_ngoandpolitician)
a <- a %>% select(from,to) %>% rename(receiver_handle = from,
                                     sender_handle = to) 
mention_edgelist_onlypolitician <- rbind(mention_edgelist_onlypolitician, a)

#type: co-share edgelist
#filter names with a co-sharing of URL relationship
inBigV <- ds_large %>% select(receiver_handle, X.M..url_mention_expanded.) %>%
  filter(receiver_handle %in% bigv_namelist$TwitterHandle) %>% 
  filter( !(X.M..url_mention_expanded.=="")  ) %>%
  rename(url_expanded = X.M..url_mention_expanded.)

#from politician * url to politician*politician matrix
a <- input_twomode_get_onemode_edgelist(inBigV) # no politicians shared same url
#still no ngo * politician co-sharing interaction

#write
write.csv(coshare_url_edgelist_t1, "coshare_url_edgelist_t1.csv")
write.csv(coshare_url_edgelist_t2, "coshare_url_edgelist_t2.csv")
write.csv(mention_edgelist_t1, "mention_edgelist_t1.csv")
write.csv(mention_edgelist_t2, "mention_edgelist_t2.csv")

