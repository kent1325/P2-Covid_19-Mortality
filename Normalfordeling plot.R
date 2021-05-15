library(ggplot2)
library(tidyverse)
library(latex2exp)

curve(dnorm, -3.5, 3.5, axes=FALSE, lwd=1, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c(TeX(r'($\mu - 3\sigma$)'), TeX(r'($\mu - 2\sigma$)'), TeX(r'($\mu - \sigma$)'), TeX(r'($\mu$)'), TeX(r'($\mu + \sigma$)'), TeX(r'($\mu + 2\sigma$)'), TeX(r'($\mu + 3\sigma$)')), pos=0, cex.lab=2)
arrows(1, ycs, -1, ycs, length = 0.1, xpd=TRUE)
arrows(-1, ycs, 1, ycs, length = 0.1, xpd=TRUE)
arrows(2, ycs+yco, -2, ycs+yco, length = 0.1, xpd=TRUE)
arrows(-2, ycs+yco, 2, ycs+yco, length = 0.1, xpd=TRUE)
arrows(3, ycs+2*yco, -3, ycs+2*yco, length = 0.1, xpd=TRUE)
arrows(-3, ycs+2*yco, 3, ycs+2*yco, length = 0.1, xpd=TRUE)
text(x=0, y=ycs2, label="68%", cex = 1, xpd=TRUE)
text(x=0, y=ycs2+yco, label="95%", cex =1, xpd=TRUE)
text(x=0, y=ycs2+2*yco, label="99.7%", cex = 1, xpd=TRUE)

ycs2 <- -0.07


ycs <- -0.055
yco <- -0.035
