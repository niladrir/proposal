#setwd("/Users/Niladri/Documents/Research/Proposal")

library(ggplot2)

x<-seq(-4,4,0.01)
y<-dnorm(x)
x.sub<-c(2,x[x>=2])
y.sub<-c(0,y[x>=2])
x.reject<-c(2,2,5,5)
y.reject<-c(0,0.5,0.5,0)
qplot(x, y, geom="line", xlab="", ylab="") + geom_hline(yintercept=0) + scale_x_continuous(breaks=c(-2,0,2), labels=c("",0,expression(t[n-1](alpha)))) + scale_y_continuous(breaks=NULL) + geom_polygon(mapping=aes(x=x.sub, y=y.sub),fill=I("red"), alpha=I(0.3)) + geom_vline(xintercept=2, color=I("red"), alpha=I(0.8)) + geom_text(mapping=aes(x=3.2, y=0.18, label="Reject"), color=I("red"), size=3) + geom_text(mapping=aes(x=3.2, y=0.15, label="H[0]"), parse=TRUE, color=I("red"), size=3) + geom_text(mapping=aes(x=3.2, y=0.12, label="t>t[n-1](alpha)"), parse=TRUE, color=I("red"), size=3) + geom_text(mapping=aes(x=0, y=0.13, label="Fail to reject"), color=I("blue"), size=3) + geom_text(mapping=aes(x=0, y=0.1, label="H[0]"), parse=TRUE, color=I("blue"), size=3) + geom_text(mapping=aes(x=0, y=0.08, label="t<=t[n-1](alpha)"), parse=TRUE, color=I("blue"), size=3) + geom_text(mapping=aes(x=-2.5, y=0.4, label="Sampling distribution if"), color=I("black"), size=3) + geom_text(mapping=aes(x=-2.5, y=0.37, label="H[0]"), parse=TRUE, color=I("black"), size=3) + geom_text(mapping=aes(x=-2.5, y=0.34, label="is true"), color=I("black"), size=3) + geom_text(mapping=aes(x=2.2, y=0.01, label="alpha"), parse=TRUE, color=I("red"), size=3)

## two sided rejection region

x<-seq(-4,4,0.01)
y<-dnorm(x)
x.sub1<-c(2,x[x>=2])
y.sub1<-c(0,y[x>=2])
x.sub2<-c(-2,x[x<=-2])
y.sub2<-c(0,y[x<=-2])
qplot(x, y, geom="line", xlab="", ylab="") + geom_hline(yintercept=0) + scale_x_continuous(breaks=c(-2,0,2), labels=c(expression(-t[n-1](alpha/2)),0,expression(t[n-1](alpha/2)))) + scale_y_continuous(breaks=NULL)+ geom_polygon(mapping=aes(x=x.sub1, y=y.sub1),fill=I("red"), alpha=I(0.3)) + geom_polygon(mapping=aes(x=x.sub2, y=y.sub2),fill=I("red"), alpha=I(0.3)) + geom_vline(xintercept=c(2,-2), color=I("red"), alpha=I(0.8))
#ggsave("two-sided-rejection.pdf",height=6,width=6)

###Visual Inference 

##A random example

x<-seq(-4,4,0.01)
y<-dnorm(x)
z<-sample(x,19)
#qplot(x, y, geom="line", xlab="", ylab="", size=I(1)) 
ggplot() + geom_line(aes(x=x,y=y), size=I(1))+ geom_hline(yintercept=0)  + scale_y_continuous("",breaks=0)+ scale_x_continuous("",breaks=c(0,2),labels=c(0,expression(t[observed]))) + geom_linerange(aes(ymin=0,ymax=0.03,x=z),col="blue",size=0.8,alpha=I(0.6))  + geom_linerange(aes(ymin=0,ymax=dnorm(2),x=2),col="red",size=0.8,alpha=I(0.8))
ggsave("visual-inference-plot.pdf",height=6,width=6)


##Plot corresponding to the lineup used to explain

lineup<-read.csv("conc-lineup.csv")
qplot(label, conc, data=lineup, colour=label, size=I(3), alpha=I(0.5), ylab="Conc (mg/kg)", xlab="Site") + facet_wrap(~.sample)

conc<-read.csv("conc.csv")
qplot(label, conc, data=conc, colour=label, size=I(5), alpha=I(0.5), ylab="Conc (mg/kg)", xlab="Site") 

t_act<-function(cl,x){
	t<-t.test(x[cl==levels(cl)[2]],x[cl==levels(cl)[1]])
    t$statistic[[1]]
    }
    
 t_dat<-ddply(lineup,.(.sample),summarize, t=t_act(label, conc))   
 
x<-seq(-4,4,0.01)
y<-dnorm(x)
z<-sample(x,19)
#qplot(x, y, geom="line", xlab="", ylab="", size=I(1)) 
ggplot() + geom_line(aes(x=x,y=y), size=I(1))+ geom_hline(yintercept=0)  + scale#_y_continuous("",breaks=0)+ scale_x_continuous("",breaks=c(0,t_dat$t[16]),labels=c(0,expression(t[observed]))) + geom_linerange(aes(ymin=0,ymax=0.03,x=t_dat$t[-16]),col="blue",size=0.8,alpha=I(0.6))  + geom_linerange(aes(ymin=0,ymax=0.05,x=t_dat$t[16]),col="red",size=0.8,alpha=I(0.8)) + geom_text(mapping=aes(x=t_dat$t[c(1,4:6,8:12,15,17:20)],label=c(1,4:6,8:12,15,17:20)),y=0.05,color="black",size=I(3))+ geom_text(mapping=aes(x=t_dat$t[-c(1,4:6,8:12,15:16,17:20)],label=c(2,3,7,13,14)),y=0.07,color="black",size=I(3))
ggplot() + geom_line(aes(x=x,y=y))+ geom_hline(yintercept=0)  + scale_y_continuous("",breaks=0)+ scale_x_continuous("",breaks=c(-2,0,t_dat$t[16]),labels=c("",0,expression(t[obs]))) + geom_linerange(aes(ymin=0,ymax=0.03,x=t_dat$t[-16]),col="blue",size=0.8,alpha=I(0.6))  + geom_linerange(aes(ymin=0,ymax=0.05,x=t_dat$t[16]),col="red",size=0.8,alpha=I(0.8)) + geom_text(mapping=aes(x=t_dat$t[c(1:15,17:20)],label=c(1:15,17:20),y=runif(19)*0.09+0.05),color="blue",size=I(3)) + geom_text(mapping=aes(x=t_dat$t[16],label="16",y=0.08),color="red",size=I(3))

ggsave("visual-inference-plot-1.pdf",height=6,width=6)
