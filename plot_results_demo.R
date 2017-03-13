# Plotting from Jan 1st thru march 12th
# This my first go with xkcd package, sloppy cannibalizing level 9000%
# so, super uncommented, sorry peepz. Just for funz

# PLOT 4

library(ggplot2)
library(xkcd)
library(plyr)

theme_adjust <- theme(axis.text.x = element_text(size = 20),
                      axis.text.y = element_text(size = 20),
                      axis.title.x = element_text(size = 20),
                      axis.title.y = element_text(size = 20),
                      title = element_text(size = 23),
                      strip.text = element_text(size = 20),
                      legend.title = element_text(size = 20),
                      legend.text = element_text(size = 20))

load("testdata.RData")

df71 <- fulldata[(fulldata$Month <= 2) | (fulldata$Month == 3 & fulldata$Day <= 12), ]
yrRange <- range(df71$Year)
df71$Year <- factor(df71$Year)


xrange <- range(df71$DOY)
yrange <- range(df71$Mean)
ratioxy <- diff(xrange) / diff(yrange)
mapping <- aes(x, y, scale, ratioxy, angleofspine,anglerighthumerus, anglelefthumerus,
anglerightradius, angleleftradius,anglerightleg, angleleftleg, angleofneck)
dataman <- data.frame(x= c(21), y=25, scale = 4, ratioxy = ratioxy,
angleofspine = -pi/2 ,anglerighthumerus = c(-pi/6),anglelefthumerus = c(pi +pi/6),
anglerightradius = c(-pi/3),angleleftradius = c(-pi/3), anglerightleg = 3*pi/2 - pi / 12,
angleleftleg = 3*pi/2 + pi / 12 ,angleofneck = 3*pi/2)
datalines <- data.frame(xbegin=c(24),ybegin=c(25),
                        xend=c(25.5), yend=c(26.5))
p4 <- ggplot() + geom_smooth(mapping = aes(x = DOY, y = Mean, colour = Year), 
                            data = df71, method = "loess", se = FALSE) + 
  xkcdaxis(xrange, yrange) + 
  labs(x = "Day of Year", y = "Daily Average Temperature (F)", title = "Daily Average Temp",
       subtitle = "Chapel Hill, NC") + 
  xkcdman(mapping, dataman) + annotate("text", x = 27, y = 27.5,
label = "oh", family="xkcd", size = 5) + 
  xkcdline(aes(xbegin=xbegin,ybegin=ybegin,xend=xend,yend=yend),
datalines, xjitteramount = 0.25) + theme_adjust
p4
ggsave("p4.png", p4)

# PLOT 3


mapping <- aes(x, y, scale, ratioxy, angleofspine,
anglerighthumerus, anglelefthumerus,
anglerightradius, angleleftradius,
anglerightleg, angleleftleg, angleofneck)


get_snow_stats <- function (df) {
  df2 <- data.frame("Avg.snow" = mean(df$Precip * df$Snow))
  df2$LastSnow <- df$DOY[max(which(df$Snow))]
  return(df2)
}
ss71 <- ddply(df71, c("Month", "Year"), get_snow_stats)
ss71$Month <- factor(ss71$Month, labels = c("Jan", "Feb", "Mar"))
ss71$Year <- factor(ss71$Year, labels = 
                      as.character(seq(yrRange[1], yrRange[2], by = 1) - 2000))


data <- ss71[ , -4]
data$Year <- seq(yrRange[1], yrRange[2], by = 1) - 2000
data$YearF <- factor(data$Year + 2000)
Year <- data$YearF
names(data) <- c("Month", "year", "number", "YearF")
data$xmin <- data$year - 0.25
data$xmax <- data$year + 0.25
data$ymin <- 0
data$ymax <- data$number
xrange <- range(min(data$xmin)-.25, max(data$xmax) + .25)
yrange <- range(0, max(data$number) + .01)
mapping <- aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax, fill = Year)
p3 <- ggplot() + xkcdrect(mapping,data) +
xkcdaxis(xrange,yrange) + facet_grid(~Month) + 
  xlab("Year (20--)") + ylab("Average Snowfall (in)") + 
  labs(title = "Average Snowfall, per month, by year",
       subtitle = "Chapel Hill, NC") + theme_adjust
ggsave("p3.png", p3)

# PLOT 1
datalines <- data.frame(xbegin=c(2012.25),ybegin=c(65),
                        xend=c(2012.8), yend=c(67.75))
ss71_2 <- ddply(df71, "Year", get_snow_stats)
ss71_2$Year <- as.numeric(levels(ss71_2$Year))
xrange <- range(ss71_2$Year)
yrange <- range(ss71_2$LastSnow)
ratioxy <- diff(xrange) / diff(yrange)
p1 <- ggplot(ss71_2, aes(x = Year, y = LastSnow)) + geom_path() + 
  xkcdaxis(xrange, yrange) + 
  labs(x = "Year", y = "Day of Year", title = "Last Snow Day Of The Year",
       subtitle = "Chapel Hill, NC") + 
  geom_point()
mapping <- aes(x, y, scale, ratioxy, angleofspine,
anglerighthumerus, anglelefthumerus,
anglerightradius, angleleftradius,
anglerightleg, angleleftleg, angleofneck)
dataman <- data.frame(x= c(2011.5), y=c(65),
scale = 4,
ratioxy = ratioxy,
angleofspine = -pi/2 ,
anglerighthumerus = c(-pi/6),
anglelefthumerus = c(-pi/2 - pi/6),
anglerightradius = c(-pi/5),
angleleftradius = c(-pi/5),
angleleftleg = 3*pi/2 + pi / 12 ,
anglerightleg = 3*pi/2 - pi / 12,
angleofneck = 3*pi/2-pi/10)
p1 <- p1 + xkcdman(mapping, dataman) + 
  annotate("text", x=2013, y = 70,
label = "heh, global cooling more like", family="xkcd", size = 6) + 
  xkcdline(aes(xbegin=xbegin,ybegin=ybegin,xend=xend,yend=yend),
datalines, xjitteramount = 0.35) + theme_adjust + 
  annotate("text", x = 2017, y = 73, label = "(today)",
           family = "xkcd", size = 5)
ggsave("p1.png", p1)

# PLOT 2

datalines <- data.frame(xbegin=c(2013.75),ybegin=c(45),
                        xend=c(2014.5), yend=c(46.5))
mapping <- aes(x, y, scale, ratioxy, angleofspine,
anglerighthumerus, anglelefthumerus,
anglerightradius, angleleftradius,
anglerightleg, angleleftleg, angleofneck)
dataman <- data.frame( x= c(2013), y=c(45),
scale = 5,
ratioxy = ratioxy,
angleofspine = -pi/2 ,
anglerighthumerus = c(-pi/6),
anglelefthumerus = c(-pi/2 - pi/6),
anglerightradius = c(pi/5),
angleleftradius = c(pi/5),
angleleftleg = 3*pi/2 + pi / 12 ,
anglerightleg = 3*pi/2 - pi / 12,
angleofneck = 3*pi/2+pi/10)
get_first12 <- function (df) {
  df2 <- data.frame("AvgTemp" = mean(df$Mean[df$Month == 3 & df$Day == 12]))
}
ss71_3 <- ddply(df71, "Year", get_first12)
ss71_3$Year <- as.numeric(levels(ss71_3$Year))
xrange <- range(ss71_3$Year)
yrange <- range(ss71_3$AvgTemp)
ratioxy <- diff(xrange) / diff(yrange)
p2 <- ggplot(ss71_3, aes(x = Year, y = AvgTemp)) + geom_path() + 
  xkcdaxis(xrange, yrange) + 
  labs(x = "Year", y = "Average Temp (F)", title = "Average Temp on March 12th",
       subtitle = "Chapel Hill, NC") + 
  geom_point() + xkcdman(mapping, dataman) +
  annotate("text", x=2014.5, y = 48,
label = "I mean, C'MON.", family="xkcd", size = 7) + 
  xkcdline(aes(xbegin=xbegin,ybegin=ybegin,xend=xend,yend=yend),
datalines, xjitteramount = 0.30)  + theme_adjust
ggsave("p2.png", p2)

