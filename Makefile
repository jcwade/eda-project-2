all: plot1.png plot2.png plot3.png plot4.png plot5.png plot6.png

plot%.png: plot%.R
	R CMD BATCH $<

clean:
	$(RM) *.png *.Rout

.PHONY: clean
