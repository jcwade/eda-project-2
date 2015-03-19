with.output.to.png <- function(base, thunk) {
        path <- paste0(base, ".png")
        png(path)
        thunk
        dev.off()
}

plot.1 <- function() {
        data <- nei %>% group_by(year) %>% summarize(total=sum(Emissions))
        plot(data, pch=19, yaxt="n", main="PM2.5 Total Emissions", xlab="Year", ylab="PM2.5 (million tons)")
        axis(2, at=axTicks(2), labels=axTicks(2)/1e6)
        abline(lm(total ~ year, data))
        data
}
