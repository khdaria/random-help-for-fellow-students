library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

players <- read.csv("players.csv", fileEncoding = "Windows-1251")
teams_hst <- read.csv("players_team_hstry_clean.csv")
earnings <- read.csv("players_earn2.csv")[,-1]
earnings <- distinct(earnings)
players$player <- str_sub(players$player, start=8) 
players$earnings <- tidyr::extract_numeric(players$Approx..Total.Earnings.)
players$Country. <- str_trim(players$Country.)


players_regions <- read.table("players_regions.txt", sep=",", header=T)
players_regions$region <- as.character(players_regions$region)
players_regions$region[is.na(players_regions$region)] <- "NA"
players$reg <- players_regions$region[match(players$Country., players_regions$country)]
players$earnings_log <- log(players$earnings)

players <- filter(players, reg!="Other")
players$reg <- reorder(players$reg, -players$earnings, FUN=median)

players <- select(players, reg, earnings, earnings_log,no_transfers, player) %>% na.omit()

png("reg_inc.png", height = 1000, width=1800,res=300)
ggplot(players, aes(reorder(reg,-earnings_log, FUN=median), earnings)) + geom_boxplot()  +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) + 
  xlab("Регион происхождения игроков") + ylab("Доходы ($)") + theme(text=element_text(size=15))
dev.off()
png("reg_trans.png", height = 1000, width=1800,res=300)
ggplot(players, aes(reorder(reg,-no_transfers, FUN=median), no_transfers)) + geom_boxplot() +
  xlab("Регион происхождения игроков") + ylab("Количество трансферов") + theme(text=element_text(size=15))
dev.off()
png("reg_inc_bar.png", height = 1000, width=1800,res=300)
ggplot(players, aes(earnings)) +geom_histogram() + facet_wrap(~reg)+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  xlab("Доход ($)") + ylab("Количество игроков\nс таким доходом")+ theme(text=element_text(size=15))
dev.off()

teams_hst_sum <- group_by(teams_hst, player) %>% tally
names(teams_hst_sum)[2] <- "no_transfers"
players <- left_join(players, teams_hst_sum)


earnings$date <- as.POSIXct(as.character(earnings$date), origin="1970-01-01")

teams_hst$start <- as.POSIXct(teams_hst$start, origin="1970-01-01")
teams_hst$end <- as.POSIXct(teams_hst$end, origin="1970-01-01")

library(lubridate)
library(igraph)
teams <- unique(teams_hst$team)
intersects <- data.frame()
for(i in teams){
  print(which(teams==i))
  x <- filter(teams_hst, team==i)
  if(nrow(x)>1){
    y = list()
    for(j in 1:nrow(x)){
      y[[length(y)+1]] <-interval(x$start[j], x$end[j], tzone="UTC")
      
    }
    for(k in 1:(length(y)-1)){
      for(m in 2:length(y)){
        intersects <- rbind(intersects, data.frame(player1 = x$player[k], player2 = x$player[m],len = day(as.period(intersect(y[[k]], y[[m]]), "days"))))
      }
    }
  }
}

pth_ints <- na.omit(intersects)
pth_ints <- filter(pth_ints, len >0)


g <- graph.data.frame(pth_ints, directed = F)
g <- simplify(g, edge.attr.comb="sum")

pth_ints <- get.edgelist(g) %>% as.data.frame()
pth_ints$n <- E(g)$len


write.csv(pth_ints, "full.network.csv")

mem <- components(g,mode="strong")
g <- induced.subgraph(g, mem$membership==1)
plot(g, vertex.label=NA, vertex.size=2)
head(V(g)$name)
V(g)$region <- as.character(players$Country.[match(V(g)$name, players$player)])
V(g)$region <- str_trim(V(g)$region)

players_regions <- read.table("players_regions.txt", sep=",", header=T)
players_regions$region <- as.character(players_regions$region)
players_regions$region[is.na(players_regions$region)] <- "NA"
V(g)$reg <- players_regions$region[match(V(g)$region, players_regions$country)]

plot(g, vertex.label=NA, vertex.size=2, vertex.color=factor(V(g)$reg), edge.width=(E(g)$len/300))

test_g <- data.frame(a=1:4,b=5:8)
test_g <- graph.data.frame(test_g)
plot(test_g, vertex.color=factor(V(test_g)$name)) 

V(g)$prize <- players$earnings[match(V(g)$name, as.character(players$player))]
V(g)$income <-  players$earnings[match(V(g)$name, as.character(players$player))]
V(g)$prize <- log(V(g)$prize)/2
V(g)$prize[is.na(V(g)$prize)] <- 1
V(g)$income[is.na(V(g)$income)] <- 1

V(g)$reg_f <- as.numeric(as.factor(V(g)$reg))
## NA 4 синий, EU 3 зелёный, SEA 7 красный, SA 6 оранжевый, 
### CIS 2 желтый, China 1 фиолетовый, Other 5
V(g)$col <- ifelse(V(g)$reg_f == 1, "#660066", ifelse(V(g)$reg_f ==3, "#158443", ifelse(V(g)$reg_f == 4, "#003366", 
                                                                                        ifelse(V(g)$reg_f ==5, "#006633",
                                                                                               ifelse(V(g)$reg_f == 6, "#F26435",ifelse(V(g)$reg_f == 2,"#CC9900","#AF291D") )))))
comms <- fastgreedy.community(g)$membership
comms <- ifelse(comms %in% c(6,7),11, comms) %>% as.character() %>% as.factor() %>% as.numeric()%>% as.factor()

l <-layout.kamada.kawai(g)
png("full.network_comm.png", width=1000, height=1000)
plot(g, vertex.label=NA, vertex.size=V(g)$prize, vertex.color=factor(comms), edge.width=(E(g)$len/300),
     layout=l, vertex.frame.color="white")
dev.off()


png("full.network.png", width=1000, height=1000)
plot(g, vertex.label=NA, vertex.size=V(g)$prize, vertex.color=factor(V(g)$reg), edge.width=(E(g)$len/300),
     layout=l, vertex.frame.color="white")
dev.off()

vcd::assoc(comms ~ V(g)$reg, shade=T)


for(i in unique(V(g)$reg)){
  x <- induced.subgraph(g, V(g)$reg==i)
  print(i)
  print(assortativity.nominal(x, as.integer(as.factor(V(x)$region)), F))
}
earnings2 <- earnings %>% group_by(player) %>% summarise(prize=sum(prize))


props_income <- data.frame(player=V(g)$name, region=V(g)$reg, betweenness= betweenness(g), closeness=closeness(g),
                           income = V(g)$income)
props_income$income_log <- log(props_income$income)
props_income <- filter(props_income, region !="Other")
ggplot(props_income, aes(income_log, log(betweenness))) + geom_point() +geom_smooth(method="lm")

ggplot(props_income, aes(income_log, closeness)) + geom_point() +
  geom_smooth(method="lm")

ggplot(props_income, aes(income_log, log(betweenness))) + 
  geom_point() +geom_smooth(method="lm") +
  facet_wrap(~region)

ggplot(props_income, aes(income_log, closeness)) + geom_point() +
  geom_smooth(method="lm")+facet_wrap(~region)  + xlab("Доход игрока (log)") +
  ylab("Closeness Centrality")

pth_ints_by_year <- data.frame()
benchmarks_by_year <- data.frame()
for(year in c(2012, 2013, 2014, 2015, 2016)){
  print(year)
  teams_hst_sub <- filter(teams_hst, start < str_c(year,"-12-31"))
  teams_hst_sub$end <- ifelse(teams_hst_sub$end>str_c(year,"-12-31"),str_c(year,"-12-31"),teams_hst_sub$end)
  teams_hst_sub$end <- as.POSIXct(as.numeric(teams_hst_sub$end), origin="1970-01-01")
  
  teams_hst_sub <- filter(teams_hst_sub, (teams_hst_sub$end-teams_hst_sub$start)>0)
  
  teams <- unique(teams_hst_sub$team)
  print(str_c("TEAMS ", length(teams)))
  intersects2 <- data.frame()
  for(i in teams){
    if(which(teams==i)%%10==0){print(which(teams==i))}
    
    x <- filter(teams_hst_sub, team==i)
    if(nrow(x)>1){
      y = list()
      for(j in 1:nrow(x)){
        y[[length(y)+1]] <-interval(x$start[j], x$end[j], tzone="UTC")
        
      }
      for(k in 1:(length(y)-1)){
        for(m in 2:length(y)){
          intersects2 <- rbind(intersects, data.frame(player1 = x$player[k], player2 = x$player[m],len = day(as.period(intersect(y[[k]], y[[m]]), "days"))))
        }
      }
    }
  }
  
  pth_ints_sub <- na.omit(intersects2)
  pth_ints_sub <- filter(pth_ints_sub, len >0)
  
  
  g_sub <- graph.data.frame(pth_ints_sub, directed = F)
  g_sub <- simplify(g_sub, edge.attr.comb="sum")
  
  pth_ints_sub <- get.edgelist(g_sub) %>% as.data.frame()
  pth_ints_sub$n <- E(g_sub)$len
  names(pth_ints_sub)<-c("player1","player2","days")
  pth_ints_sub$year <- year
  pth_ints_by_year <- rbind(pth_ints_by_year, pth_ints_sub)
  
  earnings2 <- filter(earnings, date > str_c(year,"-01-01") & date < str_c(year,"-12-31")) %>%
    group_by(player) %>% summarise(prize=sum(prize))
  V(g_sub)$prize <- earnings2$prize[match(V(g_sub)$name, as.character(earnings2$player))]
  V(g_sub)$prize[is.na(V(g_sub)$prize )] <- 1
  df <- data.frame(player = V(g_sub)$name, betweenness = betweenness(g_sub),
                   closeness = closeness(g_sub), income=V(g_sub)$prize)
  df$year <- year
  benchmarks_by_year <- rbind(benchmarks_by_year, df)
}
write.csv(benchmarks_by_year,"benchmarks_by_year.csv")
write.csv(pth_ints_by_year,"pth_ints_by_year.csv")



library(tidyr)

benchmarks_by_year <- read.csv("benchmarks_by_year.csv")[,-1]
bby <- benchmarks_by_year

bby <- filter(bby, income != 1)
bby_2013 <- filter(bby, year==2012) %>% select(player, betweenness,closeness)
bby_2013 <- filter(bby, year==2013) %>% select(player, income) %>% left_join(bby_2013)
bby_2013$year <- "2013"


bby_2014 <- filter(bby, year==2013) %>% select(player, betweenness,closeness)
bby_2014 <- filter(bby, year==2014) %>% select(player, income) %>% left_join(bby_2014)
bby_2014$year <- "2014"


bby_2015 <- filter(bby, year==2014) %>% select(player, betweenness,closeness)
bby_2015 <- filter(bby, year==2015) %>% select(player, income) %>% left_join(bby_2015)
bby_2015$year <- "2015"


bby_2016 <- filter(bby, year==2015) %>% select(player, betweenness,closeness)
bby_2016 <- filter(bby, year==2016) %>% select(player, income) %>% left_join(bby_2016)
bby_2016$year <- "2016"

bby_mod <- rbind(bby_2013, bby_2014, bby_2015, bby_2016)
bby_mod$income_log <- log(bby_mod$income)
bby_mod$betweenness_log <- log(bby_mod$betweenness)
bby_mod$betweenness_log[is.infinite(bby_mod$betweenness_log)] <- 0
bby_mod$b2 <- bby_mod$betweenness_log*bby_mod$betweenness_log
fit <- lm(income_log ~ betweenness_log + closeness, data=bby_mod)
summary(fit)

ggplot(bby_mod, aes(betweenness_log,income_log)) + geom_point() + geom_smooth(method="lm") + 
  ylab('Доход игрока в следующем сезоне (log)') +
  xlab("Betweenness centrality в текущем сезоне (log)")+ theme(text=element_text(size=15))
assortativity.nominal(g, V(g)$reg) 

sea <- induced_subgraph(g, V(g)$reg=="SEA")
plot(sea, vertex.label=NA, vertex.color=factor(V(sea)$color))

V(sea)$color <- ifelse(V(sea)$region=="Philippines",1,ifelse(V(sea)$region=="South Korea",2,ifelse(V(sea)$region=="Malaysia",3,4)))

df <- data.frame(a=1:4,b=5:8)
df <- graph.data.frame(df)
plot(df, vertex.color=factor(V(df)$name))


cis <- induced_subgraph(g, V(g)$reg=="CIS")
V(cis)$color <- ifelse(V(cis)$region=="Russia",1,ifelse(V(cis)$region=="Ukraine",2,3))
plot(cis, vertex.label=NA, vertex.color=factor(V(cis)$color))


tail(sort(V(g)$prize),68)[1]
V(g)$prize2 <- ifelse(V(g)$prize>tail(sort(V(g)$prize),68)[1],1,0)
plot(g, vertex.label=NA, vertex.color=as.factor(V(g)$prize2), vertex.size=4, layout=layout.kamada.kawai)

nms <- V(g)$name[V(g)$prize2==1]

pth_ints2 <- filter(pth_ints, V1 %in% nms | V2 %in% nms)
g2 <- graph.data.frame(pth_ints2, directed = F)
V(g2)$name2 <- ifelse(V(g2)$name %in% nms,V(g2)$name,NA)
png("rich_club.png", width=2500, height=2500, res=100)

plot(g2, vertex.frame.color="white",vertex.label=V(g2)$name2, vertex.label.cex=0.8, 
     vertex.label.color="black",
     vertex.label.family="Ubuntu",vertex.size=3, vertex.color=as.factor(ifelse(V(g2)$name %in% nms,1,2)))
dev.off()


plot(g2, vertex.frame.color="white",vertex.label=NA, vertex.label.cex=0.8, 
     vertex.label.color="black",
     vertex.label.family="Ubuntu",vertex.size=3, vertex.color=as.factor(ifelse(V(g2)$name %in% nms,1,2)))

transfers <- filter(transfers, reg!="Other")
transfers <- teams_hst %>% group_by(player) %>% tally
transfers$reg <- V(g)$reg[match(transfers$player, V(g)$name)]
transfers$income <- V(g)$prize[match(transfers$player, V(g)$name)]
ggplot(transfers, aes(n, income,color=reg)) + geom_point() + facet

ggplot(na.omit(transfers), aes(reorder(reg,-n, FUN=median),n)) + geom_boxplot() + ylab("Количество команд на игрока") +
  xlab("Регион")
ggplot(filter(na.omit(transfers),income !=1), aes(reorder(reg,-income, FUN=median),income)) + geom_boxplot()+ 
  ylab("Доход на игрока (log)") + xlab("Регион")

library(linkcomm)
  
lcoms <- getLinkCommunities(pth_ints)
plot(lcoms,type = "graph",shownodesin = 30)
plot(lcoms, type = "dend")
