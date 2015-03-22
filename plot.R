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

plot.1 <- function() {
        data <- NEI %>%
                group_by(year) %>%
                summarize(total=sum(Emissions))

        plot(data, pch=19, yaxt="n", main="PM2.5 Total Emissions", xlab="Year", ylab="PM2.5 (million tons)")
        axis(2, at=axTicks(2), labels=axTicks(2)/1e6)
        abline(lm(total ~ year, data))
        data
}

# 2.  Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a
# plot answering this question.

# Using a linear regression model, we can see that PM2.5 has decreased in
# Baltimore City, Maryland during the period in question.

plot.2 <- function() {
        data <- NEI %>%
                filter(fips == "24510") %>%
                group_by(year) %>%
                summarize(total=sum(Emissions))

        plot(data, pch=19,
             main="PM2.5 Total Emissions in Baltimore City, Maryland",
             xlab="Year", ylab="PM2.5 (tons)"
             )
        abline(lm(total ~ year, data))
}

# Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
# answer this question.

# We plot each type in a separate panel and fit with a linear regression model.
# We can see that nonroad, nonpoint, and onroad sources have decreased and point
# sources have increased.

plot.3 <- function() {
        data <- NEI %>%
                filter(fips == "24510") %>%
                group_by(type, year) %>%
                summarize(total=sum(Emissions))

        ggplot(data, aes(year, total)) +
                labs(title="PM2.5 Emission in Baltimore City, Maryland",
                     x="Year", y="PM2.5 Emissions (tons)") +
                geom_point() + facet_grid(~ type) +
                stat_smooth(method="lm")
}

# 4.  Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

# We select coal combustion-related sources by taking EI.Sector column values
# starting with "Fuel Comb" and ending with "Coal".
#
# We can see a downward trend in emission levels.

plot.4 <- function() {
        coal <- SCC %>% filter(grepl("^Fuel Comb - .* - Coal$", EI.Sector))

        data <- merge(NEI, coal, by="SCC") %>%
                group_by(year) %>%
                summarize(total=sum(Emissions))

        ggplot(data, aes(x=year, y=total)) +
                labs(title="Coal Combustion-related Emissions",
                     x="Year", y="PM2.5 Emissions (tons)") +
                geom_point() + geom_smooth(method="lm")
}

# 5.  How have emissions from motor vehicle sources changed from
# 1999–2008 in Baltimore City?

# For a the definition of a "motor vehicle", we take the following
# EI.Sector entries:
#
# - "Mobile - On-Road Gasoline Light Duty Vehicles"
# - "Mobile - On-Road Gasoline Heavy Duty Vehicles"
# - "Mobile - On-Road Diesel Light Duty Vehicles"
# - "Mobile - On-Road Diesel Heavy Duty Vehicles"
#
# This includes vehicles such as cars and trucks but excludes aircraft, marine
# vessels, and locomotives.
#
# Our assumption is that the intent is show the pollution arising from cars and
# trucks on the streets of Baltimore.
#
# Using a linear regression model, we see a downward trend for emission levels.

plot.5 <- function() {
        motor.vehicles <- SCC %>%
                filter(grepl("^Mobile - .* Vehicles$", EI.Sector))

	baltimore <- NEI %>% filter(fips == "24510")

	baltimore.vehicles <- merge(baltimore, motor.vehicles, by="SCC")

        data <- baltimore.vehicles %>%
                group_by(year) %>%
                summarize(total=sum(Emissions))

        ggplot(data, aes(x=year, y=total)) +
                geom_point() + geom_smooth(method="lm") +
                labs(x="Year", y="PM2.5 Emissions (tons)",
                     title="Motor Vehicle Emissions")
}

# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips ==
# "06037"). Which city has seen greater changes over time in motor vehicle
# emissions?

# Using the same definition of a "motor vehicle" as in Problem 5, we plot the
# Baltimore and Los Angeles emissions and fit with a linear regression model
#
# We can see that the emission levels in both cities have remained largly
# constant.  Baltimore has a slight trend downward, and Los Angeles has a slight
# trend upward.

plot.6 <- function() {
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