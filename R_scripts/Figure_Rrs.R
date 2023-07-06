#### Rrs plot for the data paper
####
####

library(tidyr)
library(dplyr)
library(ggplot2)
library(latex2exp)

##### load data sets 
cops <- read.csv(file = "/data/homeData/Insitu/AquatelDatabase/DB/Rrs_cops_long_aquatel.csv", header = TRUE)
hocr <- read.csv(file = "/data/homeData/Insitu/AquatelDatabase/DB/Rrs_hocr_long_chone.csv", header = TRUE)
asd <- read.csv(file = "/data/homeData/Insitu/AquatelDatabase/DB/Rrs_asd_long_aquatel.csv", header = TRUE)
psr <- read.csv(file = "/data/homeData/Insitu/AquatelDatabase/DB/Rrs_psr_long_wiseman.csv", header = TRUE)


hocr %>%
  ggplot(aes(x=wavelength, y=Rrs, group=station_alt)) +
  geom_line()


## compare HOCR and COPS ar the only station where both are available: PT-01 on June 4 2019
###### IMPORTANT COPS cast 4 from PT-01 sample on June 4 at 11:00 should not have been included in the final data base. 


#get mean Rrs from the 2 HOCR replicates
pt1_hocr <- hocr %>% filter(station=="PT-01") %>%  group_by(wavelength) %>% summarise(RRSm = mean(Rrs), RRSsd= sd(Rrs))
pt1_hocr$instrument = "HOCR"

#get COPS from the data... 
#pt1_cops <- cops %>% filter(station=="PT-01" & date == "2019-06-04") %>%  group_by(wavelength) %>% summarise(RRSm = mean(Rrs), RRSsd= sd(Rrs))
#pt1_cops <- cops %>% filter(station_id=="chone-PT-01-2019-06-04T11:00:00Z")
#pt1_cops <- pt1_cops[9:11]
#names(pt1_cops) <- c("wavelength", "RRSm", "RRSsd")
#
################### cast 4 was included but very bad!!!
load("/data/homeData/Insitu/CHONe/L2/199/COPS/BIN/BSI_CAST_001_190604_111335_URC.csv.RData")
cast1<-cops
load("/data/homeData/Insitu/CHONe/L2/199/COPS/BIN/BSI_CAST_002_190604_111601_URC.csv.RData")
cast2<-cops
load("/data/homeData/Insitu/CHONe/L2/199/COPS/BIN/BSI_CAST_003_190604_111937_URC.csv.RData")
cast3<-cops


R1<-cast1$Rrs.0p.linear
R2<-cast2$Rrs.0p.linear
R3<-cast3$Rrs.0p.linear

Rm <- cbind(R1,R2,R3)
RRSm  <- apply(Rm, 1, mean, na.rm=T)
RRSsd <-apply(Rm, 1, sd, na.rm=T)

pt1_cops <- data.frame(wavelength = cast1$LuZ.waves, RRSm, RRSsd)
pt1_cops$instrument = "C-OPS"

df = rbind(pt1_hocr,pt1_cops)

df %>% 
  ggplot(aes(x=wavelength, y=RRSm, colour=instrument)) + 
  geom_point() + 
  scale_x_continuous(limits = c(400,800))+
  geom_errorbar(aes(ymin=RRSm-RRSsd, ymax=RRSm+RRSsd), width=.2, position=position_dodge(.9))+
  scale_color_manual(values=c("black", "darkgrey"))+
  theme_bw()+
  xlab(TeX("$\\lambda$")) +
  ylab(TeX("$R_{rs}(sr^{-1})$"))+
  annotate("text", y= 0.016, x = 470, label="Bay of Sept-Îles", color='black') +
  annotate("text", y= 0.015, x = 470, label="Station PT-01", color='black') +
  annotate("text", y= 0.014, x = 470, label="June 4th 2019 @ 11:00 UTC", color='black') 
ggsave("./Figures/TEST.png", width = 20, height = 15, units = "cm")
