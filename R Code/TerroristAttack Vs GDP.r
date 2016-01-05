## In this script, we integrate the terrorism data with country information from
## the GapMinder data to see if there are any interesting patterns to do with gdp per capita

source('Common_Code.R')

# read the gap minder data
gDat <- read.delim("gapminderDataFiveYear.txt")

# we're mostly interested in the GDP of the countries at just one timepoint,
# so keep a subset of gapminder data with only the latest year
attach(gDat)
gDatLast <- subset(gapminder, year == 2007)
max(gDat$year)



countryAttacks <- ddply(globalterrorismdb_0615, ~ country + region, plyrFxCount, "totAttacks")
countryAttacks <- merge(countryAttacks,
                        subset(gDatLast, select = c('country', 'pop')),
                        by.x = 'country',
                        by.y = 'country')
countryAttacks$popPerAttack <- round(countryAttacks$pop / countryAttacks$totAttacks)
countryAttacks <- arrange(countryAttacks, popPerAttack)
print(head(countryAttacks, n = 5))
write.table(countryAttacks, paste0(resultsDir, "countriesMostAttacksPerPop.txt"),
            quote = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE)


# Iraq seems to be the only country that is both in the top 10 most attacked and top 10 most
# attacked per population. We see that all these countries are either in the Middle East or
# South America.

# Now let's see if there's any correlation between a country's GDP and its terrorism history
countryAttacksGapMinder <-
  merge(countryAttacks,
        gDatLast,
        by.x = c('country'),
        by.y = c('country'))
ggplot(countryAttacksGapMinder, aes(x = gdpPercap, y = totAttacks, color = region)) +
  geom_point() +
  geom_point(aes(cex = 1.5), show_guide = FALSE) +  # little hack necessary to now show cex in legend
  xlab("GDP / Capita") + 
  ggtitle("Terrorist Attacks Since 1970 vs 2007 GDP of Attacked Country") + 
  ylab("# of Attacks") +
  scale_color_manual(name = 'Region', values = regionCol)
ggsave(paste0(resultsDir, 'numAttacksVsGDP.png'))
dev.off()