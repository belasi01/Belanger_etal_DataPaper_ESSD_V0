# This script is to plot the data to explain how the bottom reflectance is extracted from COPS
# I choose an arbitrary station of WISE-Man (MAN-F5) visited on Aug 18 2019
library(lubridate)
library(data.table)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(latex2exp)

# qplot(1, "A")+
#   ylab(TeX("Formula: $\\frac{2hc^2}{\\lambda^\\beta}$"))+
#   xlab(TeX("$\\alpha$"))

con <- file("./data/WISE_MAN_F5_CAST_003_190818_181835_URC.tsv", "r")
line = readLines(con,1)
if (line == "Start of Header") {
  nhead = 1
  while(line != "End of Header") {
    line = readLines(con,1)
    nhead = nhead + 1
  }
  close(con)
  print("Header detected")
  print(nhead)
}


df = read.table(file = "./data/WISE_MAN_F5_CAST_003_190818_181835_URC.tsv",
                header = TRUE, as.is = TRUE, sep = "\t",
                check.names = FALSE,
                skip = nhead)

df <- df[,c(2,5)]
df$DateTime <- mdy_hms(df$DateTime)
names(df) <- c("Time", "Depth")

arrow <- tibble(
  x = df$Time[200],
  xend = df$Time[355],
  y = 4.9,
  yend = 5.2
)

arrow2 <- tibble(
  x = df$Time[145],
  xend = df$Time[50],
  y = 0.5,
  yend = 0.4
)

p1 <- ggplot(df, aes(x=Time, y=Depth)) +
  geom_point() +
  scale_x_datetime(date_labels = "%H:%M:%S") +
  scale_y_reverse() +
  theme_bw() +
  ylab("Depth (m)") +
  annotate("text", y= 4.5, x = df$Time[200], 
           label="C-OPS reaching\n the bottom", color='black') +
  geom_curve(
    data = arrow,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    curvature = 0.2,
    size = 1,
    arrow = arrow(length = unit(0.2, "inch"))
  ) +
  annotate("text", y= 0.5, x = df$Time[220], label="C-OPS at surface") +
  geom_curve(
    data = arrow2,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    curvature = -0.1,
    size = 1,
    arrow = arrow(length = unit(0.2, "inch"))
  )



load("./data/WISE_MAN_F5_CAST_003_190818_181835_URC.tsv.RData")
# extract EdZ data
EdZ<-cops$EdZ[,c(7, 12, 14)]  # Keep 443, 560 and 625 nm to plot
df <- as.data.frame(cbind(cops$Depth-0.05, EdZ))
names(df) <- c("Depth", "443","560","625")
dfm <- reshape2::melt(df, id.vars="Depth")
names(dfm) <- c("Depth", "Wavelength","EdZ")

EdZf <- cops$EdZ.fitted[,c(7, 12, 14)]
dff <- as.data.frame(cbind(cops$depth.fitted, EdZf))
names(dff) <- c("Depth", "443","560","625")
dfm2 <- reshape2::melt(dff, id.vars="Depth")
names(dfm2) <- c("Depth", "Wavelength","EdZ")

bot = data.frame(x=c(0,200), y=c(cops$bottom.depth, cops$bottom.depth))
rb.z=cops$bottom.depth - cops$Rb.depth.over.bottom
rbZ = data.frame(x=c(0,200), y=c(rb.z,rb.z))

p2 <- ggplot() +
  geom_point(data=dfm, aes(x=EdZ, y=Depth, color=Wavelength),size=0.3, shape = 21) +
  geom_path(data=dfm2, aes(x=EdZ, y=Depth, color=Wavelength), size=1) +
  geom_line(data=bot, aes(x=x, y=y), size=0.8) +
  geom_line(data=rbZ, aes(x=x, y=y), size=0.8, color="grey", linetype = 2) +
  scale_x_log10() +
  scale_y_reverse() +
  theme_bw() +
  ylab("Depth (m)")+
  xlab(TeX("$E_d(z,\\lambda)\\,\\mu\\,W\\,cm^{-2}\\,nm^{-1}$"))+
  scale_color_manual(values = c("443" = "blue", "560" = "darkgreen", "625" = "red"))+
  theme(
    legend.position = c(.3, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )




# extract LuZ data
LuZ<-cops$LuZ[,c(7, 12, 14)]  # Keep 443, 560 and 625 nm to plot
df <- as.data.frame(cbind(cops$Depth+0.253, LuZ))
names(df) <- c("Depth", "443","560","625")
dfm <- reshape2::melt(df, id.vars="Depth")
names(dfm) <- c("Depth", "Wavelength","LuZ")

LuZf <- cops$LuZ.fitted[,c(7, 12, 14)]
dff <- as.data.frame(cbind(cops$depth.fitted, LuZf))
names(dff) <- c("Depth", "443","560","625")
dfm2 <- reshape2::melt(dff, id.vars="Depth")
names(dfm2) <- c("Depth", "Wavelength","LuZ")

# 
bot = data.frame(x=c(0,1), y=c(cops$bottom.depth, cops$bottom.depth))
rb.z=cops$bottom.depth - cops$Rb.depth.over.bottom
rbZ = data.frame(x=c(0,1), y=c(rb.z,rb.z))


arrow3 <- tibble(
  x = 0.07,
  xend = 0.08,
  y = 4,
  yend = 5.2
)

arrow4 <- tibble(
  x = 0.4,
  xend = 0.4,
  y = 4.8,
  yend = 5.45
)

p3 <- ggplot() +
  geom_point(data=dfm, aes(x=LuZ, y=Depth, color=Wavelength),size=0.3, shape = 21) +
  geom_path(data=dfm2, aes(x=LuZ, y=Depth, color=Wavelength), size=1) + 
  geom_line(data=bot, aes(x=x, y=y), size=0.8) +
  geom_line(data=rbZ, aes(x=x, y=y), size=0.8, color="grey", linetype = 2) +
  scale_x_log10() +
  scale_y_reverse() +
  theme_bw() +
  ylab("Depth (m)")+
  xlab(TeX("$L_u(z,\\lambda)\\,\\mu\\,W\\,cm^{-2}\\,nm^{-1}\\,sr^{-1}$"))+
  scale_color_manual(values = c("443" = "blue", "560" = "darkgreen", "625" = "red"))+
  theme(legend.position = "none") +
  annotate("text", y= 4.2, x = 0.4, label="Bottom\ndepth") +
  annotate("text", y= 3.8, x = 0.07, label="Depth for Rb") +
  geom_curve(
    data = arrow3,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    curvature = -0.05,
    size = 1,
    color = "grey",
    arrow = arrow(length = unit(0.15, "inch"))) +
  geom_curve(
    data = arrow4,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    curvature = 0.05,
    size = 1,
    arrow = arrow(length = unit(0.15, "inch"))
  )


##### Plot of spectral Rb





ggarrange(p1,p2,p3,labels=c("(a)", "(b)", "(c)"), ncol = 2, nrow=2, widths = 1, heights = 2)
ggsave("./Figures/Fig_Rb.png", width = 20, height = 15, units = "cm")
