loadNamespace("plyr")
library(dplyr)
library(ggplot2)

with.output.to.png <- function(base, thunk) {
        path <- paste0(base, ".png")
        png(path)
        thunk
        dev.off()
}

# 6.  Compare emissions from motor vehicle sources in Baltimore City
# with emissions from motor vehicle sources in Los Angeles County,
# California (fips == "06037"). Which city has seen greater changes
# over time in motor vehicle emissions?

# Using the same definition of a "motor vehicle" as in Problem 5, we plot the
# Baltimore and Los Angeles emissions and fit with a linear regression model
#
# We can see that the emission levels in both cities have remained largly
# constant.  Baltimore has a slight trend downward, and Los Angeles has a slight
# trend upward.

plot6 <- function(NEI, SCC) {
        motor.vehicles <- SCC %>%
                filter(grepl("^Mobile - .* Vehicles$", EI.Sector))

        cities <- NEI %>% filter(fips %in% c("24510", "06037"))

        cities.vehicles <- merge(cities, motor.vehicles, by="SCC")

        data <- cities.vehicles %>%
                group_by(fips, year) %>%
                summarize(total=sum(Emissions))

        data$City <- plyr::mapvalues(data$fips,
                               c("24510", "06037"),
                               c("Baltimore", "Los Angeles"))

        ggplot(data, aes(x=year, y=total, color=City)) +
                geom_point() + geom_smooth(method="lm") +
                labs(x="Year", y="PM2.5 Emissions (tons)",
                     title="Motor Vehicle Emissions")
}

if (!interactive()) {
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        with.output.to.png("plot6", print(plot6(NEI, SCC)))
}
