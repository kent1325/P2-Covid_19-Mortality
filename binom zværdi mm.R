library(ggplot2)
library(tidyverse)
library(latex2exp)
library(cowplot)
binom_data <- cbind(c(1,2,3,4,5,6,7,8,9),c(2,5,10,16,20,16,10,5,2))
head(binom_data)
colnames(df) <- c("Number")
rownames(df) <- c(1,2,3,4,5,6,7,8,9)
ggplot(binom_data, aes(x=Number)) + geom_histogram()
df <- data.frame(c(rep(1,2),rep(2,5),rep(3,10),rep(4,16),rep(5,20),rep(6,16),rep(7,10),rep(8,5),rep(9,2)))
ggplot(df, aes(x=Number)) + geom_histogram(bins = 9,color="black", fill="white")+theme_histo
theme_histo <- theme_bw()+theme(panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.background = element_blank(),
                                     axis.title.y = element_blank(),
                                     axis.title=element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.text.y = element_text(size=20,colour = "black")
                                     )

ggplot(data.frame(x = c(0, 40)), aes(x = x)) + stat_function(fun = dchisq, args = list(df = 10))

p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + ylab("") +
  scale_y_continuous(breaks = NULL)
p1 +geom_segment(aes(x = -3, y = 0, xend = 3, yend = 0))+geom_segment(aes(x = 1.96, y = 0, xend = 1.96, yend = 0.058))+geom_segment(aes(x = -1.96, y = 0, xend = -1.96, yend = 0.058))+
                                                                                                                                        theme(panel.grid.major = element_blank(),
                                                                                                                                        panel.grid.minor = element_blank(),
                                                                                                                                        panel.background = element_blank(),
                                                                                                                                        axis.title.x = element_blank(),
                                                                                                                                        axis.text.x = element_text(size=20,colour = "black"))


p2 <- ggplot(data = data.frame(x = c(0, 10)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + ylab("") +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4))
p2+
  geom_segment(aes(x = -4, y = 0.3997, xend = 0, yend = 0.3997), linetype="dashed") +
  geom_segment(aes(x = 0, y = -1, xend = 0, yend = 0.3997), linetype="dashed") +
  coord_cartesian(ylim = c(0, 0.45),xlim = c(-3,3)) +
  theme_bw()+
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size=20,colour = "black"),
          axis.text.y = element_text(size=20,colour = "black"))


library(car)
library(mosaic)
library(ggplot2)
set.seed(34)
n <- 1:6
d <- 1/6

df1 <- data.frame(n=n,prob=d)
p3 <- ggplot(df1,aes(x=n, y=prob))+scale_x_continuous(breaks = c(1,2,3,4,5,6))
p3 + geom_segment(aes(xend = n, yend = 0), size =4, color="white") + coord_cartesian(ylim = c(0, 1))+ylab(expression(P(X==n))) + theme(axis.text.x=element_text(colour="black", size=20,face="plain"), axis.text.y=element_text(colour="black", size=20,angle=0,hjust=1,vjust=0,face="plain"), axis.title.x=element_text(colour="black",size=20,angle=0,hjust=.5,vjust=0,face="bold"), axis.title.y=element_text(colour="black",size=20,angle=90,hjust=.5,vjust=.5,face="bold")) + theme(legend.position="none")

p3 + geom_col(aes(x = n, y=prob), fill="white", color = "black")+coord_cartesian(ylim = c(0, 1))+theme_bw()+ylab(expression(P(X==n)))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size=20,colour = "black"),
        axis.text.y = element_text(size=20,colour = "black"),
        axis.title.y = element_text(size=20)
        )


x <- seq(0, 40, length = 10000)
dfs <- c(1, 5, 10, 20)
plot(x, dchisq(x, df = dfs[2]), col = "gray40", type = "l", lwd = 2, xlab = expression(chi^2), ylab = "", cex.lab =1.5, cex.axis = 1.3)
lines(x, dchisq(x, df = dfs[1]), col = "gray0", lwd = 2)
lines(x, dchisq(x, df = dfs[3]), col = "gray60", lwd = 2)
lines(x, dchisq(x, df = dfs[4]), col = "gray80", lwd = 2)
legend("topright", legend = paste("df = ", dfs), col = c("gray0","gray40","gray60","gray80"), lwd = 2, cex = 2)



















              

                                                                                                                