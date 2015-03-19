with.output.to.png <- function(base, thunk) {
        path <- paste0(base, ".png")
        png(path)
        thunk
        dev.off()
}

# 1.  Have total emissions from PM2.5 decreased in the United States from 1999
# to 2008? Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

plot.1 <- function() {
        data <- nei %>% group_by(year) %>% summarize(total=sum(Emissions))
        plot(data, pch=19, yaxt="n", main="PM2.5 Total Emissions", xlab="Year", ylab="PM2.5 (million tons)")
        axis(2, at=axTicks(2), labels=axTicks(2)/1e6)
        abline(lm(total ~ year, data))
        data
}

# 2.  Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a
# plot answering this question.
plot.2 <- function() {
        data <- nei %>%
                filter(fips == "24510") %>%
                group_by(year) %>%
                summarize(total=sum(Emissions))
        plot(data, pch=19,
             main="PM2.5 Total Emissions in Baltimore City, Maryland",
             xlab="Year", ylab="PM2.5 (tons)"
             )
        #axis(2, at=axTicks(2), labels=axTicks(2)/1e3)
        abline(lm(total ~ year, data))
        data
}
