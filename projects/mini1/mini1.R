setwd("~/RStudioProjects/MVE190/projects/mini1/")

TVdat <- read.table("TV.dat",sep = "\t")
plot(ppTV~ppDr,TVdat,xlab = "People per doctor",ylab = "People per TV")
title("People per TV vs People per doctors")

plot(log10(ppTV)~log10(ppDr),TVdat,xlab = "log(People per Dr)",ylab = "log(People per TV)")
title("log10 transform: People per TV vs People per doctor")

mm1 <- lm(ppTV~ppDr,data = TVdat)
mm2 <- lm(log10(ppTV)~log10(ppDr),data = TVdat)

#with(TVdat,plot(ppDr,ppTV))
abline(mm1, col=2)
#with(TVdat,plot(log10(ppDr),log10(ppTV)))
abline(mm2, col=2)

plot(1/ppTV~1/ppDr,TVdat,xlab = "1/Dr per capita", ylab = "1/TV per capita")
title("1/TV/capita vs 1/Doctors/capita")

mm3 <- lm(I(1/ppTV)~I(1/ppDr), data = TVdat)
abline(mm3, col=2)

# disregard missing values
#lines(sort(1/TVdat$ppDr[is.na(TVdat$ppTV)==F]),sort(mm3$fit), lwd=2, col=2)

plot(sqrt(1/ppTV)~sqrt(1/ppDr), TVdat, xlab = "sqrt Dr per capita", ylab = "sqrt TV per capita")
title("Sqrt transform: TV/capita vs Dr/capita")

mm4 <- lm(sqrt(1/ppTV)~sqrt(1/ppDr), data = TVdat)
abline(mm4, col=2)
