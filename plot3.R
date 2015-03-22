loadNamespace("plyr")
library(dplyr)
library(ggplot2)

with.output.to.png <- function(base, thunk) {
        path <- paste0(base, ".png")
        png(path)
        thunk
        dev.off()
}

# Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
# answer this question.

# We plot each type in a separate panel and fit with a linear regression model.
# We can see that nonroad, nonpoint, and onroad sources have decreased and point
# sources have increased.

plot3 <- function(NEI, SCC) {
        data <- NEI %>%
                filter(fips == "24510") %>%
                group_by(type, year) %>%
                summarize(total=sum(Emissions))

        ggplot(data, aes(year, total)) +
                labs(title="Total PM2.5 Emissions by Type in Baltimore City, Maryland",
                     x="Year", y="PM2.5 Emissions (tons)") +
                geom_point() + facet_grid(~ type) +
                stat_smooth(method="lm")
}

if (!interactive()) {
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        with.output.to.png("plot3", print(plot3(NEI, SCC)))
}
