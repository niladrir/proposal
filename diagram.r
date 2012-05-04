library(ggplot2)

x<-seq(-4,4,0.01)
y<-dnorm(x)
x.sub<-c(2,x[x>=2])
y.sub<-c(0,y[x>=2])
x.reject<-c(2,2,5,5)
y.reject<-c(0,0.5,0.5,0)
qplot(x, y, geom="line", xlab="", ylab="") + geom_hline(yintercept=0) + scale_x_continuous(breaks=c(0,2), labels=c(0,expression(t[n-1](alpha)))) + scale_y_continuous(breaks=NULL) + geom_polygon(mapping=aes(x=x.sub, y=y.sub),fill=I("red"), alpha=I(0.3)) + geom_vline(xintercept=2, color=I("red"), alpha=I(0.8)) + geom_text(mapping=aes(x=3.2, y=0.18, label="Reject"), color=I("red"), size=4) + geom_text(mapping=aes(x=3.2, y=0.15, label="H[0]"), parse=TRUE, color=I("red"), size=4) + geom_text(mapping=aes(x=3.2, y=0.12, label="t>t[n-1](alpha)"), parse=TRUE, color=I("red"), size=4) + geom_text(mapping=aes(x=0, y=0.13, label="Fail to reject"), color=I("blue"), size=4) + geom_text(mapping=aes(x=0, y=0.1, label="H[0]"), parse=TRUE, color=I("blue"), size=4) + geom_text(mapping=aes(x=0, y=0.08, label="t<=t[n-1](alpha)"), parse=TRUE, color=I("blue"), size=4) + geom_text(mapping=aes(x=-2.5, y=0.4, label="Sampling distribution if"), color=I("black"), size=4) + geom_text(mapping=aes(x=-2.5, y=0.37, label="H[0]"), parse=TRUE, color=I("black"), size=4) + geom_text(mapping=aes(x=-2.5, y=0.34, label="is true"), color=I("black"), size=4) + geom_text(mapping=aes(x=2.2, y=0.01, label="alpha"), parse=TRUE, color=I("red"), size=4)
