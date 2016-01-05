# this script only needs to be run once on a machine, to make sure all the
# required libararies are installed
install.packages("plyr")
install.packages("ggplot2")
install.packages("maps")
install.packages("mapdata")
install.packages("fBasics")
install.packages("RColorBrewer")
install.packages("reshape")
install.packages("data.table")

## Wd aaand Library 
setwd("~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis")
library(data.table)
library(bit64)
library(ggplot2)

## Common Code
#source('Common_Code.r')

# read the raw input
cols <- c("iyear", "imonth", "iday", "country_txt", "region_txt", "city","motive",
          "attacktype1","attacktype1_txt", "nkill", "nwound")
# gtd_70to91_0615 <- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/GTD Files/CSV Dataset/gtd_70to91_0615dist.csv', select = cols)
# gtd_92to10_0615 <- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/GTD Files/CSV Dataset/gtd_92to10_0615dist.csv', select = cols)
# gtd1993_0615dist.csv <- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/GTD Files/CSV Dataset/gtd1993_0615dist.csv', select = cols)
 gtd_11to14_0615 <- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/GTD Files/CSV Dataset/gtd_11to14_0615dist.csv', select = cols)

### data : 1970-2014
 gtd_11to14_0615 <- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/GTD Files/CSV Dataset/globalterrorismdb_0615dist.csv', select = cols)

# factor modification for attack type 
gtd_11to14_0615$attacktype1 <- as.factor(gtd_11to14_0615$attacktype1)

# sanity check that import was ok (there should be ~141k rows and 123 columns)
dim(gtd_11to14_0615)
library(plyr)
gtd_11to14_0615 <- rename(gtd_11to14_0615, c("iyear" = "year", "imonth" = "month", "iday" = "day",
                                        "country_txt" = "country", "region_txt" = "region", "attacktype1_txt" = "attacktype"))

## Checking motive values 
levels(gtd_11to14_0615$motive) # Need to remove NA  need to create classes 
gtd_11to14_0615$motive <- factor(gtd_11to14_0615$motive)

# wounded and killed by each attack type overall
attacktypeDamage <- ddply(gtd_11to14_0615, ~attacktype1, function(x){
  df <- data.frame(c("nkill", "nwound"), c(sum(x$nkill), sum(x$nwound)));
  colnames(df)<-c("stat","value");
  return(df)
})

# Plot data  - Attack type 

ggplot(attacktypeDamage, aes(x = attacktype1, y = value, fill = stat)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.9)) +
  coord_flip() +
  ggtitle("Number of People Wounded or Killed\nby Terrorist Attacks Since 1970") +
  xlab("attack") +
  ylab("# of People") +
  scale_fill_manual(name = "Injury Type", values = c("black", "red"), labels = c('Killed', 'Wounded')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(face="bold"))

## plot region wise 
plyrFxCount <- function(x, name="count") {
  df <- data.frame( nrow(x) )
  colnames(df)[1] <- name
  return(df)
}

attach(gtd_11to14_0615)
attach(regionTotal)

regionTotal <- ddply(gtd_11to14_0615, ~region, plyrFxCount)
levels(regionTotal$region) <-c(factor(regionTotal$region))
#regionTotal < regionTotal[order(regionTotal$count)]
ggplot(regionTotal, aes(x = regionTotal$region, y = regionTotal$count)) + ## , fill = region
  geom_bar(stat="identity", show_guide=FALSE,fill = levels(regionTotal$region) ) + #, fill = regionTotal$region
  coord_flip() +
  ggtitle("Terrorist Attacks in World Regions Since 2011") +
  xlab("") +
  ylab("# of Attacks") +
  scale_fill_manual(values = region) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(face="bold"))

resultsDir <-"~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/results/"

ggsave(paste0(resultsDir, 'globalCasualtiesSince2011.png'))
dev.off()


# It looks like overall since 1970, there hasn't been one major region that suffered more
# than others. Every successvie region has less terror attacks than its previous, but the
# gap is never massive.
# Next we should zoom in and see what happens when we look at different years rather than combined history.


# calculate the number of attacks in each region per year
regionYear <- ddply(gtd_11to14_0615, region ~ year, plyrFxCount, "nattacks")

# fix a little "problem" (well, a good problem), where some regions have years with 0 attacks
# this will cause some missing points in the plots which doesn't look nice, so we will
# just add a value of 0 for every region/year pair that doesn't exist
regionYearPossibilities <- merge(regionYear$region, unique(gtd_11to14_0615$year))
regionYear <- merge(regionYear, regionYearPossibilities,
                    by.x = c('region','year'), by.y = c("x","y"), all.y = TRUE)
regionYear$nattacks[is.na(regionYear$nattacks)] <- 0

# let's look at the number of attacks per year in each world region
levels(regionYear$region) <-c(factor(unique(regionYear$region)))

ggplot(regionYear, aes(x = year, y = nattacks, color = regionYear$region)) +
  geom_line(show_guide=FALSE) +
  geom_point(show_guide=FALSE) +
  xlab("Year") + 
  ggtitle("Number of Terrorist Attacks in World Regions Since 2011") + 
  ylab("# of Attacks") +
  facet_wrap(~region) +
  scale_color_manual(values = regionCol) + 
  theme(strip.text = element_text(face="bold"),
        plot.title = element_text(face="bold"))

ggsave(paste0(resultsDir, 'terrorismPerRegionYears.png'))




# EDA 

