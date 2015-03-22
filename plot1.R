loadNamespace("plyr")
library(dplyr)
library(ggplot2)

with.output.to.png <- function(base, thunk) {
        path <- paste0(base, ".png")
        png(path)
        thunk
        dev.off()
}

# 1.  Have total emissions from PM2.5 decreased in the United States from 1999
# to 2008? Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# Using a linear regression model, we can see that PM2.5 levels decreased in the
# United States during the period in question.

plot1 <- function(NEI, SCC) {
        data <- NEI %>%
                group_by(year) %>%
                summarize(total=sum(Emissions))

        plot(data, pch=19, yaxt="n",
             main="Total U.S. PM2.5 Emissions",
             xlab="Year", ylab="PM2.5 (million tons)")
        axis(2, at=axTicks(2), labels=axTicks(2)/1e6)
        abline(lm(total ~ year, data))
        data
}

if (!interactive()) {
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        with.output.to.png("plot1", plot1(NEI, SCC))
}
