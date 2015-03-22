loadNamespace("plyr")
library(dplyr)
library(ggplot2)

with.output.to.png <- function(base, thunk) {
        path <- paste0(base, ".png")
        png(path)
        thunk
        dev.off()
}

# 4.  Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

# We select coal combustion-related sources by taking EI.Sector column values
# starting with "Fuel Comb" and ending with "Coal".
#
# We can see a downward trend in emission levels.

plot4 <- function(NEI, SCC) {
        coal <- SCC %>% filter(grepl("^Fuel Comb - .* - Coal$", EI.Sector))

        data <- merge(NEI, coal, by="SCC") %>%
                group_by(year) %>%
                summarize(total=sum(Emissions))

        ggplot(data, aes(x=year, y=total)) +
                labs(title="Coal Combustion-related Emissions",
                     x="Year", y="PM2.5 Emissions (tons)") +
                geom_point() + geom_smooth(method="lm")
}

if (!interactive()) {
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        with.output.to.png("plot4", print(plot4(NEI, SCC)))
}
