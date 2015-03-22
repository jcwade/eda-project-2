loadNamespace("plyr")
library(dplyr)
library(ggplot2)

with.output.to.png <- function(base, thunk) {
        path <- paste0(base, ".png")
        png(path)
        thunk
        dev.off()
}

# 2.  Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a
# plot answering this question.

# Using a linear regression model, we can see that PM2.5 has decreased in
# Baltimore City, Maryland during the period in question.

plot2 <- function(NEI, SCC) {
        data <- NEI %>%
                filter(fips == "24510") %>%
                group_by(year) %>%
                summarize(total=sum(Emissions))

        plot(data, pch=19,
             main="Total PM2.5 Emissions in Baltimore City, Maryland",
             xlab="Year", ylab="PM2.5 (tons)"
             )
        abline(lm(total ~ year, data))
}

if (!interactive()) {
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        with.output.to.png("plot2", plot2(NEI, SCC))
}
