setwd("/Users/vicentediegoortegadelvecchyo/Dropbox/Documents/Berkeley/DStatisticEmilia/FiguresRScripts")

Sims <- read.table("TableConfigurations.txt")
DStatisticStuff <- read.table("DStatisticValues.txt")

TP3 <- 0.5 * 40000
TGF <- 0.1 * 40000
TP2 <- 0.25 * 40000
N <- 10000

DifOne <- TP3 - TP2
DifTwo <- TP3 - TGF

fValues <- c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)
Table <- c()
for (f in fValues){
    Line <- c()
    
    OtherPrAAAB <- (1-f)*(2*N + TP2 - exp(-DifOne/(2*N))*(2*N/3)) + f * ( 2*N + TP3 - exp(-DifTwo/(2*N))*(2*N/3) )
    OtherPrAAAB
    OtherPrAABA <- (1-f)*(2*N + TP2 - exp(-DifOne/(2*N))*(2*N/3)) + f * ( 2*N + TGF - exp(-DifTwo/(2*N))*(2*N/3) )
    OtherPrAABA
    Pr_ABBA <- f * (TP3 - TGF) + (1 - f)* ((1 - 1 / (2*N))**(TP3 - TP2)) * (2*N/3) + f*((1 - 1/(2*N))**(TP3 - TGF))*(2*N/3)
    Pr_BABA <- (1 - f)* ((1 - 1 / (2*N))**(TP3 - TP2)) * (2*N/3) + f*((1 - 1/(2*N))**(TP3 - TGF))*(2*N/3)
    Dif_ABBA_BABA = f*(TP3 - TGF)
    Dif_AAAB_AABA = f*(TP3 - TGF)
    Line <- c(OtherPrAAAB, OtherPrAABA, Pr_ABBA, Pr_BABA,Dif_ABBA_BABA,Dif_AAAB_AABA)
    Sum_ABBA_BABA = Pr_ABBA + Pr_BABA
    Sum_AAAB_AABA = 2 * (1-f)*(2*N + TP2 - exp(-DifOne/(2*N))*(2*N/3)) + 2 * f * ( 2*N - exp(-DifTwo/(2*N))*(2*N/3) ) + f * TP3 + f * TGF
    D <- (Pr_ABBA - Pr_BABA) / (Pr_ABBA + Pr_BABA)
    D1 <- (Pr_ABBA - Pr_BABA) / (Pr_ABBA + Pr_BABA) + (OtherPrAAAB - OtherPrAABA) / (OtherPrAAAB + OtherPrAABA)
    D2 <- (Pr_ABBA - Pr_BABA + OtherPrAAAB - OtherPrAABA) / (2*(Pr_ABBA + Pr_BABA))
    D3 <- (Pr_ABBA - Pr_BABA + OtherPrAAAB - OtherPrAABA) / ((Pr_ABBA + Pr_BABA + OtherPrAAAB + OtherPrAABA))
    Line <- c(OtherPrAAAB, OtherPrAABA, Pr_ABBA, Pr_BABA,Dif_ABBA_BABA,Dif_AAAB_AABA,Sum_AAAB_AABA,Sum_ABBA_BABA,D,D1,D2,D3)
    Table <- rbind(Table,Line)
}

pdf("Figure2.pdf",width=14)
par(mar=c(5.1,5.1,1.1,1.1))
par(mfrow = c(1,2))
plot(1:8,Sims[,1],pch=19,ylim=c(20000,37500), cex=2, ylab = "Expected Branch Length", xlab = "f",xaxt="n",cex.lab=2,cex.axis=1.5)
lines(1:8,Table[,1])
points(1:8,Sims[,2],col = "red",pch = 19, cex=2)
lines(1:8,Table[,2],col = "red")
axis(1, at=c(1:8), labels=c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),cex.axis=1.5)
legend("top",c("Points.- Simulations","Lines.- Analytical Solutions",expression("T"[BAAA]),expression("T"[ABAA])),pch=19,col=c("white","white","black","red"),cex=1.5)
# legend("topleft",c("Points - Simulations","Lines - Analytical solutions"),bty="n",cex=0.9)

# dev.off()


# pdf("SimulationsAnalyticalResultsABBA_BABA.pdf")
par(mar=c(5.1,5.1,1.1,1.1))
plot(1:8,Sims[,3],pch=19,ylim=c(2500,20000), cex=2, ylab = "Expected Branch Length", xlab = "f",xaxt="n",cex.lab=2,cex.axis=1.5)
lines(1:8,Table[,3])
points(1:8,Sims[,4],col = "red",pch = 19, cex=2)
lines(1:8,Table[,4],col = "red")
axis(1, at=c(1:8), labels=c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),cex.axis=1.5)
legend("top",c("Points.- Simulations","Lines.- Analytical Solutions",expression("T"[ABBA]),expression("T"[BABA])),pch=19,col=c("white","white","black","red"),cex=1.5)
#legend("topleft",c("Points - Simulations","Lines - Analytical solutions"),bty="n",cex=0.9)
dev.off()


pdf("Figure3.pdf",width=14)
par(mfrow = c(1,2))
par(mar=c(5.1,5.1,1.1,1.1))
plot(1:8,Sims[,3] - Sims[,4],pch=19,ylim=c(0,16000), cex=2, ylab = "Expected Branch Length", xlab = "f",xaxt="n",cex.lab=2,cex.axis=1.5)
lines(1:8,Table[,5])
# points(1:8,Sims[,1] - Sims[,2],col = "red",pch = 19, cex=2)
# lines(1:8,Table[,6],col = "red")
axis(1, at=c(1:8), labels=c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),cex.axis=1.5)
legend("top",c("Points.- Simulations","Lines.- Analytical Solutions",expression("T"[ABBA]*" - T"[BABA]*" branch difference")), pch=19, col=c("white","white","black"), cex=1.5)
#legend("topleft",c("Points - Simulations","Lines - Analytical solutions"),bty="n",cex=0.9)
# dev.off()

# pdf("SimulationsAnalyticalResultsDifferencesBAAA_ABAA.pdf")
par(mar=c(5.1,5.1,1.1,1.1))
plot(1:8,Sims[,1] - Sims[,2],pch=19,ylim=c(0,16000), cex=2, ylab = "Expected Branch Length", xlab = "f",xaxt="n",cex.lab=2,cex.axis=1.5)
lines(1:8,Table[,6])
# points(1:8,Sims[,1] - Sims[,2],col = "red",pch = 19, cex=2)
# lines(1:8,Table[,6],col = "red")
axis(1, at=c(1:8), labels=c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),cex.axis=1.5)
legend("top",c("Points.- Simulations","Lines.- Analytical Solutions",expression("T"[BAAA]*" - T"[ABAA]*" branch difference")), pch=19, col=c("white","white","black"), cex=1.5)
#legend("topleft",c("Points - Simulations","Lines - Analytical solutions"),bty="n",cex=0.9)
dev.off()


pdf("Figure4.pdf")
par(mar=c(5.1,5.1,1.1,1.1))
plot(1:8,Sims[,1] + Sims[,2],pch=19,ylim=c(0,60000), cex=2, ylab = "Expected Branch Length", xlab = "f",xaxt="n",cex.lab=2,cex.axis=1.5)
lines(1:8,Table[,7])
points(1:8,Sims[,3] + Sims[,4],col = "red",pch = 19, cex=2)
lines(1:8,Table[,8],col = "red")
axis(1, at=c(1:8), labels=c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),cex.axis=1.5)
legend("center",c("Points - Simulations","Lines - Analytical Solutions",expression("T"[BAAA]*" + T"[ABAA]*" branch sum"),expression("T"[ABBA]*" + T"[BABA]*" branch sum")), pch=19, col=c("white","white","black","red"), cex=1.5)
#legend("topleft",c("Points - Simulations","Lines - Analytical solutions"),bty="n",cex=0.9)
dev.off()



pdf("Figure5.pdf",width=14)
par(mar=c(5.1,5.1,1.1,1.1))
par(mfrow = c(1,2))
plot(1:8,DStatisticStuff[,5],pch=19,ylim=c(0,1), cex=2, ylab = "D statistics comparisons", xlab = "f",xaxt="n",cex.lab=2,cex.axis=1.5)
lines(1:8,Table[,9])
points(1:8,DStatisticStuff[,6],col = "red",pch = 19, cex=2)
lines(1:8,Table[,10],col = "red")
axis(1, at=c(1:8), labels=c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),cex.axis=1.5)
legend("top",c("Points.- Simulations","Lines.- Analytical Solutions","D","D1"), pch=19, col=c("white","white","black","red"), cex=1.5)
#legend("topleft",c("Points - Simulations","Lines - Analytical solutions"),bty="n",cex=0.9)
# dev.off()

# pdf("SimulationsAnalyticalResultsDStatisticsThreeFour.pdf")
par(mar=c(5.1,5.1,1.1,1.1))
plot(1:8,DStatisticStuff[,7],pch=19,ylim=c(0,1), cex=2, ylab = "D statistics comparisons", xlab = "f",xaxt="n",cex.lab=2,cex.axis=1.5)
lines(1:8,Table[,11])
points(1:8,DStatisticStuff[,8],col = "red",pch = 19, cex=2)
lines(1:8,Table[,12],col = "red")
axis(1, at=c(1:8), labels=c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),cex.axis=1.5)
legend("top",c("Points.- Simulations","Lines.- Analytical Solutions","D2","D3"), pch=19, col=c("white","white","black","red"), cex=1.5)
#legend("topleft",c("Points - Simulations","Lines - Analytical solutions"),bty="n",cex=0.9)
dev.off()


################################################### Analysis changing Ne ##################################

################################################## Analysis changing Ne

TP3 <- 0.5 * 40000
TGF <- 0.1 * 40000
TP2 <- 0.25 * 40000
NValues <- 10000
f <- 0.1

NValues <- c(100, 1000, 10000, 100000, 1000000)
Table <- c()
for (N in NValues){
    Line <- c()
    
    OtherPrAAAB <- (1-f)*(2*N + TP2 - exp(-DifOne/(2*N))*(2*N/3)) + f * ( 2*N + TP3 - exp(-DifTwo/(2*N))*(2*N/3) )
    OtherPrAAAB
    OtherPrAABA <- (1-f)*(2*N + TP2 - exp(-DifOne/(2*N))*(2*N/3)) + f * ( 2*N + TGF - exp(-DifTwo/(2*N))*(2*N/3) )
    OtherPrAABA
    Pr_ABBA <- f * (TP3 - TGF) + (1 - f)* ((1 - 1 / (2*N))**(TP3 - TP2)) * (2*N/3) + f*((1 - 1/(2*N))**(TP3 - TGF))*(2*N/3)
    Pr_BABA <- (1 - f)* ((1 - 1 / (2*N))**(TP3 - TP2)) * (2*N/3) + f*((1 - 1/(2*N))**(TP3 - TGF))*(2*N/3)
    Dif_ABBA_BABA = f*(TP3 - TGF)
    Dif_AAAB_AABA = f*(TP3 - TGF)
    Line <- c(OtherPrAAAB, OtherPrAABA, Pr_ABBA, Pr_BABA,Dif_ABBA_BABA,Dif_AAAB_AABA)
    Sum_ABBA_BABA = Pr_ABBA + Pr_BABA
    Sum_AAAB_AABA = 2 * (1-f)*(2*N + TP2 - exp(-DifOne/(2*N))*(2*N/3)) + 2 * f * ( 2*N - exp(-DifTwo/(2*N))*(2*N/3) ) + f * TP3 + f * TGF
    D <- (Pr_ABBA - Pr_BABA) / (Pr_ABBA + Pr_BABA)
    D1 <- (Pr_ABBA - Pr_BABA) / (Pr_ABBA + Pr_BABA) + (OtherPrAAAB - OtherPrAABA) / (OtherPrAAAB + OtherPrAABA)
    D2 <- (Pr_ABBA - Pr_BABA + OtherPrAAAB - OtherPrAABA) / (2*(Pr_ABBA + Pr_BABA))
    D3 <- (Pr_ABBA - Pr_BABA + OtherPrAAAB - OtherPrAABA) / ((Pr_ABBA + Pr_BABA + OtherPrAAAB + OtherPrAABA))
    Line <- c(OtherPrAAAB, OtherPrAABA, Pr_ABBA, Pr_BABA,Dif_ABBA_BABA,Dif_AAAB_AABA,Sum_AAAB_AABA,Sum_ABBA_BABA,D,D1,D2,D3)
    Table <- rbind(Table,Line)
}

pdf("Figure6.pdf",width = 14)
par(mar=c(5.1,5.1,1.1,1.1))
par(mfrow = c(1,2))
plot(1:5,Table[,9],pch=19,ylim=c(0,1.1), cex=2, ylab = "D statistics comparisons", xlab = "Ne",xaxt="n",cex.lab=2,cex.axis=1.5,type="l")
#lines(1:5,Table[,9])
lines(1:5,Table[,10],col = "red",pch = 19, cex=2)
#lines(1:5,Table[,10],col = "red")
axis(1, at=c(1:5), labels=c(100, 1000, 10000, 100000, 1000000),cex.axis=1.5)
legend("topright",c("Lines.- Analytical Solutions","D","D1"), pch=19, col=c("white","black","red"), cex=1.5)
#legend("topleft",c("Points - Simulations","Lines - Analytical solutions"),bty="n",cex=0.9)
# dev.off()


# pdf("SimulationsAnalyticalResultsNeChangesDStatisticsThreeFour.pdf")
par(mar=c(5.1,5.1,1.1,1.1))
plot(1:5,Table[,11],pch=19,ylim=c(0,1.1), cex=2, ylab = "D statistics comparisons", xlab = "Ne",xaxt="n",cex.lab=2,cex.axis=1.5,type="l")
# lines(1:5,Table[,11])
lines(1:5,Table[,12],col = "red",pch = 19, cex=2)
# lines(1:5,Table[,12],col = "red")
axis(1, at=c(1:5), labels=c(100, 1000, 10000, 100000, 1000000),cex.axis=1.5)
legend("topright",c("Lines.- Analytical Solutions","D2","D3"), pch=19, col=c("white","black","red"), cex=1.5)
#legend("topleft",c("Points - Simulations","Lines - Analytical solutions"),bty="n",cex=0.9)
dev.off()

############### Plot Fg

TP3 <- 0.5 * 40000
TGF <- 0.1 * 40000
TP2 <- 0.25 * 40000
N <- 10000

DifOne <- TP3 - TP2
DifTwo <- TP3 - TGF

fValues <- c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)
Table <- c()
for (f in fValues){
    Line <- c()
    
    OtherPrAAAB <- (1-f)*(2*N + TP2 - exp(-DifOne/(2*N))*(2*N/3)) + f * ( 2*N + TP3 - exp(-DifTwo/(2*N))*(2*N/3) )
    OtherPrAAAB
    OtherPrAABA <- (1-f)*(2*N + TP2 - exp(-DifOne/(2*N))*(2*N/3)) + f * ( 2*N + TGF - exp(-DifTwo/(2*N))*(2*N/3) )
    OtherPrAABA
    Pr_ABBA <- f * (TP3 - TGF) + (1 - f)* ((1 - 1 / (2*N))**(TP3 - TP2)) * (2*N/3) + f*((1 - 1/(2*N))**(TP3 - TGF))*(2*N/3)
    Pr_BABA <- (1 - f)* ((1 - 1 / (2*N))**(TP3 - TP2)) * (2*N/3) + f*((1 - 1/(2*N))**(TP3 - TGF))*(2*N/3)
    Dif_ABBA_BABA = f*(TP3 - TGF)
    Dif_AAAB_AABA = f*(TP3 - TGF)
    Line <- c(OtherPrAAAB, OtherPrAABA, Pr_ABBA, Pr_BABA,Dif_ABBA_BABA,Dif_AAAB_AABA)
    Sum_ABBA_BABA = Pr_ABBA + Pr_BABA
    Sum_AAAB_AABA = 2 * (1-f)*(2*N + TP2 - exp(-DifOne/(2*N))*(2*N/3)) + 2 * f * ( 2*N - exp(-DifTwo/(2*N))*(2*N/3) ) + f * TP3 + f * TGF
    D <- (Pr_ABBA - Pr_BABA) / (Pr_ABBA + Pr_BABA)
    D1 <- (Pr_ABBA - Pr_BABA) / (Pr_ABBA + Pr_BABA) + (OtherPrAAAB - OtherPrAABA) / (OtherPrAAAB + OtherPrAABA)
    D2 <- (Pr_ABBA - Pr_BABA + OtherPrAAAB - OtherPrAABA) / (2*(Pr_ABBA + Pr_BABA))
    D3 <- (Pr_ABBA - Pr_BABA + OtherPrAAAB - OtherPrAABA) / ((Pr_ABBA + Pr_BABA + OtherPrAAAB + OtherPrAABA))
    Line <- c(OtherPrAAAB, OtherPrAABA, Pr_ABBA, Pr_BABA,Dif_ABBA_BABA,Dif_AAAB_AABA,Sum_AAAB_AABA,Sum_ABBA_BABA,D,D1,D2,D3)
    Table <- rbind(Table,Line)
}

DStatisticStuff <- read.table("DStatisticValues.txt")
SStatisticStuff <- read.table("S_StatisticValues.txt")

OldFg <- (DStatisticStuff[,9] - DStatisticStuff[,10]) / (SStatisticStuff[,9] - SStatisticStuff[,10])

NewFg <- (DStatisticStuff[,9] - DStatisticStuff[,10]) / (SStatisticStuff[,9] - SStatisticStuff[,10]) + (DStatisticStuff[,11] - DStatisticStuff[,12]) / (SStatisticStuff[,9] - SStatisticStuff[,10])

AnalyticalSolutionOldFg <- c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)*(TP3-TGF)/TP3
AnalyticalSolutionNewFg <- c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)*(TP3-TGF)/TP3 + c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)*(TP3-TGF)/TP3

pdf("Figure7.pdf")
par(mar=c(5.1,5.1,1.1,1.1))
plot(1:8,AnalyticalSolutionOldFg,pch=19,ylim=c(0,1.7), cex=2, ylab = "Fg statistics comparisons", xlab = "f",xaxt="n",cex.lab=2,cex.axis=1.5,type="l")
lines(1:8,AnalyticalSolutionNewFg,col = "red")
points(1:8,OldFg,col = "black",pch = 19, cex=2)
points(1:8,NewFg,col = "red",pch = 19, cex=2)
axis(1, at=c(1:8), labels=c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1),cex.axis=1.5)
legend("top",c("Points - Simulations","Lines - Analytical Solutions","Fg","Fg\'"), pch=19, col=c("white","white","black","red"), cex=1.5)
#legend("topleft",c("Points - Simulations","Lines - Analytical solutions"),bty="n",cex=0.9)
dev.off()



