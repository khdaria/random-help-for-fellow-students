
scrobble <- read.csv("all_scrobble.csv")[,-1]
vkaudio <- read.csv("all_added.csv")[,-1]
users <- read.csv("vk_last_links.csv")[-1] 



vkaudio$artist <- tolower(vkaudio$artist)
ad <- vkaudio %>% group_by(owner_id, artist) %>% tally 
ad <- ad %>% ungroup %>% group_by(owner_id) %>% 
  mutate(prop = n/ sum(n)) %>% 
  arrange(prop) %>% mutate (ord = 1/length(owner_id)) %>%
  mutate(cums = cumsum(prop)) %>% mutate(cums_ord = cumsum(ord))


ggplot(ad, aes(cums_ord, cums, group=owner_id)) + geom_line(alpha = 0.2)

counts <- vkaudio %>% group_by(owner_id) %>% tally %>% arrange(desc(n))
ggplot(counts, aes(n)) + geom_histogram(binwidth = 200)

ginis <- data.frame()
for(i in unique(ad$owner_id)){
  ginis <- rbind(ginis, data.frame(owner_id=i, gini_vk = ineq(filter(ad, owner_id == i)$n, type='Gini')))
  
}


sc <- scrobble %>% group_by(user) %>% 
  mutate(prop = count/ sum(count)) %>% 
  arrange(prop) %>% mutate (ord = 1/length(user)) %>%
  mutate(cums = cumsum(prop)) %>% mutate(cums_ord = cumsum(ord))

ggplot(sc, aes(cums_ord, cums, group=user)) + geom_line(alpha = 0.2)
names(sc)[2:3] <- c("n", "owner_id")
sc$owner_id <- users$uid[match(sc$owner_id, users$lastfm)]
ad$type <- "vk"
sc$type <- "lastfm"

ids <- intersect(ad$owner_id, sc$owner_id)
ad1 <- filter(ad, owner_id %in% ids)
sc1 <- sc[(sc$owner_id %in% ids) ,]

all <- rbind(ad1, sc1)

ggplot(all, aes(cums_ord, cums, group=owner_id, color = type)) + geom_line(alpha = 0.1)+ xlab("Исполнители по возрастанию популярности") +
  ylab("Совокупное количество песен / прослушиваний")


scrobble$user <-  users$uid[match(scrobble$user, users$lastfm)]
ginis$gini_lastfm <- NA
for(i in 1:nrow(ginis)){
  ginis$gini_lastfm[i] <- ineq(filter(scrobble, user == ginis$owner_id[i])$count, type='Gini')
}

ginis <- na.omit(ginis)
t.test(ginis$gini_vk, ginis$gini_lastfm)

##########################################
## С ЖАНРАМИ #############################
##########################################


lfm_tags <- read.csv("tags_last_fm.csv")[,-1]
gn_tags <- read.csv("tags_gn.csv")[,-1]



scrobble$artist <- tolower(scrobble$artist)
scrobble$artist <- str_replace_all(scrobble$artist, fixed("'"), "")
scrobble$artist <- str_replace_all(scrobble$artist, fixed("/"), " ")
scrobble$artist <- str_replace_all(scrobble$artist, fixed("|"), " ")
scrobble$artist <- str_replace_all(scrobble$artist, fixed("\\"), " ")
scrobble$artist <- str_replace_all(scrobble$artist, fixed("#"), " ")
scrobble$artist <- str_replace_all(scrobble$artist, fixed("&"), " ")
scrobble$artist <- str_replace(gsub("\\s+", " ", str_trim(scrobble$artist)), "B", "b")


vkaudio$artist <- tolower(vkaudio$artist)
vkaudio$artist <- str_replace_all(vkaudio$artist, fixed("'"), "")
vkaudio$artist <- str_replace_all(vkaudio$artist, fixed("/"), " ")
vkaudio$artist <- str_replace_all(vkaudio$artist, fixed("|"), " ")
vkaudio$artist <- str_replace_all(vkaudio$artist, fixed("\\"), " ")
vkaudio$artist <- str_replace_all(vkaudio$artist, fixed("#"), " ")
vkaudio$artist <- str_replace_all(vkaudio$artist, fixed("&"), " ")
vkaudio$artist <- str_replace(gsub("\\s+", " ", str_trim(vkaudio$artist)), "B", "b")

lfm_tags$artist <- tolower(lfm_tags$artist)
lfm_tags$tag <- tolower(lfm_tags$tag)
lfm_tags$artist <- str_replace(gsub("\\s+", " ", str_trim(lfm_tags$artist)), "B", "b")


scrobble_l <- left_join(scrobble, lfm_tags, by = "artist")
scrobble_l <- na.omit(scrobble_l)

vkaudio_l <- vkaudio %>% group_by(owner_id, artist) %>% summarise(count = length(artist)) %>% ungroup()
vkaudio_l <- left_join(vkaudio_l, lfm_tags, by = "artist")
vkaudio_l <- na.omit(vkaudio_l)

sc <- scrobble_l %>%  group_by(user, tag) %>% summarise(n = sum(count)) %>% 
  mutate(prop = n/ sum(n)) %>% 
  arrange(prop) %>% mutate (ord = 1/length(user)) %>%
  mutate(cums = cumsum(prop)) %>% mutate(cums_ord = cumsum(ord))

ad <- vkaudio_l %>%  group_by(owner_id, tag) %>% summarise(n = sum(count)) %>% 
  mutate(prop = n/ sum(n)) %>% 
  arrange(prop) %>% mutate (ord = 1/length(owner_id)) %>%
  mutate(cums = cumsum(prop)) %>% mutate(cums_ord = cumsum(ord))

names(sc)[1] <- "owner_id"
ad$type <- "vk"
sc$type <- "lastfm"

ids <- intersect(ad$owner_id, sc$owner_id)
ad1 <- filter(ad, owner_id %in% ids)
sc1 <- sc[(sc$owner_id %in% ids) ,]

all2 <- rbind(ad1, sc1)
all2$gr <- str_c(all2$owner_id, all2$type)
ggplot(all2, aes(cums_ord, cums, group=gr, color = type)) + geom_line(alpha = 0.05)+ xlab("Исполнители по возрастанию популярности") +
  ylab("Совокупное количество песен / прослушиваний")

all2 <- ungroup(all2)
ginis <- data.frame()
for(i in unique(all2$owner_id)){
  ginis <- rbind(ginis, data.frame(user=i, gini_last = ineq(filter(all2, owner_id == i & type == "lastfm")$n, type='Gini'),
                                   gini_vk = ineq(filter(all2, owner_id == i & type == "vk")$n, type='Gini')))
  
}


########################

gn_tags <- read.csv("tags_gn.csv")[,-1]
gn_tags <- rbind(gn_tags, read.csv("tags_gn2.csv")[,-1])
gn_tags <- distinct(gn_tags)


gn_tags$genre1 <- tolower(gn_tags$genre1)

gn_tags$genre2 <- tolower(gn_tags$genre2)
gn_tags$genre3 <- tolower(gn_tags$genre3)
gn_tags$artist <- tolower(gn_tags$artist)


scrobble_g <- left_join(scrobble, gn_tags, by = "artist")
scrobble_g <- na.omit(scrobble_g)



sc_g1 <- scrobble_g %>%  group_by(user, genre1) %>% summarise(n = sum(count)) %>% 
  mutate(prop = n/ sum(n)) %>% 
  arrange(prop) %>% mutate (ord = 1/length(user)) %>%
  mutate(cums = cumsum(prop)) %>% mutate(cums_ord = cumsum(ord))


sc_g2 <- scrobble_g %>%  group_by(user, genre2) %>% summarise(n = sum(count)) %>% 
  mutate(prop = n/ sum(n)) %>% 
  arrange(prop) %>% mutate (ord = 1/length(user)) %>%
  mutate(cums = cumsum(prop)) %>% mutate(cums_ord = cumsum(ord))


sc_g1 <- scrobble_g %>%  group_by(user, genre1) %>% summarise(n = sum(count)) %>% 
  mutate(prop = n/ sum(n)) %>% 
  arrange(prop) %>% mutate (ord = 1/length(user)) %>%
  mutate(cums = cumsum(prop)) %>% mutate(cums_ord = cumsum(ord))

sc_g1$type <- "genre1"
sc_g2$type <- "genre2"
sc_g3$type <- "genre3"
sc$type <- "lastfm"
sc_all <- rbind(sc[,-2], rbind(sc_g1[,-2], rbind(sc_g3[,-2], sc_g2[,-2])))
ggplot(sc_all, aes(cums_ord, cums, group=user, color=type)) + geom_line(alpha = 0.3)+facet_grid(~type)
sc_all$gr <- str_c(sc_all$user, sc_all$type)
ggplot(sc_all, aes(cums_ord, cums, group=gr, color=type)) + geom_line(alpha = 0.1)
ggplot(filter(sc_all, type == "genre1"), aes(cums_ord, cums, group=gr, color=type)) + geom_line(alpha = 0.3)

sc_all <- ungroup(sc_all)

ggplot(filter(sc_all, type %in% c("genre1", "lastfm")), aes(cums_ord, cums, group=gr, color=type)) + geom_line(alpha = 0.3) 
ggplot(filter(sc_all, type %in% c("genre3", "lastfm")), aes(cums_ord, cums, group=gr, color=type)) + geom_line(alpha = 0.2)+ 
  xlab("Жанры по возрастанию популярности") +
  ylab("Совокупное количество песен / прослушиваний")
ggplot(filter(sc_all, type %in% c("genre2", "genre3")), aes(cums_ord, cums, group=gr, color=type)) + geom_line(alpha = 0.3) +  xlab("Жанры по возрастанию популярности") +
  ylab("Совокупное количество песен / прослушиваний")

ggplot(filter(sc_all, type %in% c("genre1", "genre2", "genre3")), aes(cums_ord, cums, group=gr, color=type)) + geom_line(alpha = 0.2)

sc_all <- ungroup(sc_all)
ginis <- data.frame()
for(i in unique(sc_all$user)){
  ginis <- rbind(ginis, data.frame(user=i, gini_lfm = ineq(filter(sc_all, user == i & type == "lastfm")$n, type='Gini'),
                                   gini_g1 = ineq(filter(sc_all, user == i & type == "genre1")$n, type='Gini'),
                                   gini_g2 = ineq(filter(sc_all, user == i & type == "genre2")$n, type='Gini'),
                                   gini_g3 = ineq(filter(sc_all, user == i & type == "genre3")$n, type='Gini')))
  
}
ginis$user <- as.character(ginis$user)
t.test(ginis$gini_lfm, ginis$gini_g1)
t.test(ginis$gini_lfm, ginis$gini_g2)
t.test(ginis$gini_lfm, ginis$gini_g3)
t.test(ginis$gini_g1, ginis$gini_g2)
t.test(ginis$gini_g1, ginis$gini_g3)
t.test(ginis$gini_g2, ginis$gini_g3)


#####################




scrobble_l <- left_join(scrobble, gn_tags, by = "artist")
scrobble_l <- na.omit(scrobble_l)

vkaudio_l <- vkaudio %>% group_by(owner_id, artist) %>% summarise(count = length(artist)) %>% ungroup()
vkaudio_l <- left_join(vkaudio_l, gn_tags, by = "artist")
vkaudio_l <- na.omit(vkaudio_l)

sc <- scrobble_l %>%  group_by(user, genre2) %>% summarise(n = sum(count)) %>% 
  mutate(prop = n/ sum(n)) %>% 
  arrange(prop) %>% mutate (ord = 1/length(user)) %>%
  mutate(cums = cumsum(prop)) %>% mutate(cums_ord = cumsum(ord))

ad <- vkaudio_l %>%  group_by(owner_id, genre2) %>% summarise(n = sum(count)) %>% 
  mutate(prop = n/ sum(n)) %>% 
  arrange(prop) %>% mutate (ord = 1/length(owner_id)) %>%
  mutate(cums = cumsum(prop)) %>% mutate(cums_ord = cumsum(ord))

names(sc)[1] <- "owner_id"
ad$type <- "vk"
sc$type <- "lastfm"

sc$owner_id <- users$uid[match(sc$owner_id, users$lastfm)]
ids <- intersect(ad$owner_id, sc$owner_id)
ad1 <- filter(ad, owner_id %in% ids)
sc1 <- sc[(sc$owner_id %in% ids) ,]

all2 <- rbind(ad1, sc1)
all2$gr <- str_c(all2$owner_id, all2$type)
ggplot(all2, aes(cums_ord, cums, group=gr, color = type)) + geom_line(alpha = 0.1)+ xlab("Исполнители по возрастанию популярности") +
  ylab("Совокупное % песен / прослушиваний")

all2 <- ungroup(all2)
ginis <- data.frame()
for(i in unique(all2$owner_id)){
  ginis <- rbind(ginis, data.frame(user=i, gini_last = ineq(filter(all2, owner_id == i & type == "lastfm")$n, type='Gini'),
                                   gini_vk = ineq(filter(all2, owner_id == i & type == "vk")$n, type='Gini')))
  
}


ggplot(a, aes(cums_ord, cums, label = genre1)) + geom_abline(color = "darkgray", linetype = 2)+ 
  geom_line(color = "darkgray") + 
  geom_point() + geom_text(angle = 45, size = 3, hjust = c(rep(0, times = 6),-0.1,0), vjust = c(rep(-1.5, times = 6),1,-1.5)) +
  xlim(0, 1.3) + ylim(0,1.3) + xlab("Жанры по возрастанию популярности") +
  ylab("Накопленное количество\nпрошлушанных песен ") 

ineq(a$cums, type="Gini")
