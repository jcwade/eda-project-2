loadNamespace("plyr")
library(dplyr)
library(ggplot2)

with.output.to.png <- function(base, thunk) {
        path <- paste0(base, ".png")
        png(path)
        thunk
        dev.off()
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

plot5 <- function(NEI, SCC) {
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
                     title="Total Motor Vehicle PM2.5 Emissions in Baltimore City, Maryland")
}

if (!interactive()) {
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        with.output.to.png("plot5", print(plot5(NEI, SCC)))
}
