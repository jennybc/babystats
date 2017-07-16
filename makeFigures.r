fIntake <-
  read.csv("/Users/jenny/Documents/personal/TwinDocuments/formulaIntake.csv")

x11(width = 6* 2.5, height = 4*2.5)
jMult <- 200
png("/Users/jenny/Documents/personal/TwinDocuments/formulaIntake.png",
     width = 6 * jMult, height = 4 * jMult, res = 200)
#     quality = 100)

pdf("/Users/jenny/Documents/personal/TwinDocuments/formulaIntake.pdf",
    width = 6, height = 4, family = "Times")
par(las = 1, mar = c(1, 6, 3, 0.5) + 0.1)
matplot(1:nrow(fIntake), fIntake[,c("Wesley","Eli")], type = "l",
        xlab = "", ylab = "", axes = FALSE, col = c(3,4),
        lty = "dashed", cex.main = 1.3,
        main = "Managing Inputs ... Nicer than Outputs")
lines(ksmooth(1:nrow(fIntake), fIntake$Wesley, "normal", bandwidth=5),
      col=3, lwd = 2)
lines(ksmooth(1:nrow(fIntake), fIntake$Eli, "normal", bandwidth=5),
      col=4,
      lwd = 2)
box()
mtext(side = 2, "Daily\nformula\nintake\nin ounces", line = 4, adj = 0.5)
mtext(c("February", "March", "April"), adj = 0,
      side = 1, line = 0, at = c(1,23,53))
axis(side = 2)
#     at = fIntake$Date[0:8 * 7 + 1],
#     at = c(1,23,53),
#     labels = c("February", "March", "April"),
#     tick = FALSE)

legend(x = "bottomright",
       legend = c("Wesley","Eli"),
       col = c(3,4),  lwd = 2, bty = "n", cex = 1.3)

dev.off()

bStats <- read.table("/Users/jenny/Documents/personal/TwinDocuments/birthStats.txt")

bData <- as.matrix(bStats[,c("WeightGms","LengthCm","HeadCm")])
bData <- bData / rep(colMeans(bData), each = 3)

pdf("/Users/jenny/Documents/personal/TwinDocuments/birthStats.pdf",
    width = 6, height = 4, family = "Times")
par(las = 1)
foo <- barplot(bData,
               beside = T, horiz = T, xlim = c(0,1.8),
               main = "Bryan Kids' Birth Statistics",
               names.arg = c("Weight","Length","Head"),
               legend.text = c("Reed","Wesley","Eli"),
               col = c("tomato4","green3","slateblue3"), axes = FALSE)
text(bData[,1] + 0.025, foo[,1], 
     labels = paste(bStats$WeightLbs,"lbs,",bStats$WeightOzs,"ozs"),
     adj = 0)
text(bData[,2] + 0.025, foo[,2], 
     labels = paste(bStats$LengthIn,"inches"),
     adj = 0)
text(bData[,3] + 0.025, foo[,3], 
     labels = paste(bStats$HeadIn,"inches"),
     adj = 0)

dev.off()
