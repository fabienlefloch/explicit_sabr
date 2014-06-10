m<-read.table('/home/fabien/mypapers/explicit_sabr/smart_initialguess_sabr_input.txt', header=TRUE)
m$alpha <- as.numeric(as.character(m$alpha))
m$nu <- as.numeric(as.character(m$nu))
m$rho <- as.numeric(as.character(m$rho))
m$Error <- as.numeric(as.character(m$Error))
ref <- m[m$Method=="Reference",]
m$alphae <- 0
m$rhoe <- 0
m$nue <- 0
smart1 <- m[m$Method=="Guess-1",]
smart1$rhoe <- smart1$rho - ref$rho
smart1$alphae <- smart1$alpha - ref$alpha
smart1$nue <- smart1$nu - ref$nu

smart0 = m[m$Method=="Guess-0",]
smart0$rhoe <- smart0$rho - ref$rho
smart0$alphae <- smart0$alpha - ref$alpha
smart0$nue <- smart0$nu - ref$nu

smart0a =m[m$Method=="Guess-0-alpha1",]
smart0a$rhoe <- smart0a$rho - ref$rho
smart0a$alphae <- smart0a$alpha - ref$alpha
smart0a$nue <- smart0a$nu - ref$nu

n <- rbind(smart0,smart1)
qplot(Expiry, abs(alphae), data=n, geom="line", color=Method, ylab="alpha error")+scale_y_log10(breaks=c(1e-2, 1e-3, 1e-4, 1e-5, 1e-6))+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/smart_initialguess_sabr_input_alpha.eps",width=4,height=4)
qplot(Expiry, abs(rhoe), data=n, geom="line", color=Method, ylab="rho error")+scale_y_log10(breaks=c(1e-2, 1e-3, 1e-4, 1e-5, 1e-6))+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/smart_initialguess_sabr_input_rho.eps",width=4,height=4)
qplot(Expiry, abs(nue), data=n, geom="line", color=Method, ylab="nu error")+scale_y_log10(breaks=c(1e-2, 1e-3, 1e-4, 1e-5, 1e-6))+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/smart_initialguess_sabr_input_nu.eps",width=4,height=4)
n <- rbind(smart0, smart0a, smart1)
qplot(Expiry, Error, data=n, geom="line", color=Method, ylab="root mean square error")+scale_y_log10(breaks=c(1e-2, 1e-3, 1e-4, 1e-5, 1e-6))+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/smart_initialguess_sabr_input_error.eps",width=4,height=4)

my.labs <- list("vol RMS", bquote(alpha),bquote(rho), bquote(nu))

qplot(Expiry, abs(alphae), data=smart, geom="line", color="alpha", linetype="alpha", ylab="Error")+geom_line(aes(y=abs(rhoe), color="rho", linetype="rho"))+geom_line(aes(y=abs(nue), color="nu", linetype="nu"))+geom_line(aes(y=abs(Error),color="vol RMS", linetype="vol RMS"))+scale_y_log10(breaks=c(1e-2, 1e-3, 1e-4, 1e-5, 1e-6))+scale_colour_manual("parameter",values=c(4,2,3,1),breaks=c("vol RMS","alpha", "rho", "nu"), labels=my.labs)+scale_linetype_manual("parameter",values=c(4,2,3,1),breaks=c("vol RMS","alpha", "rho", "nu"), labels=my.labs)+theme(legend.position=c(0.85, 0.4))
ggsave(file="/home/fabien/mypapers/explicit_sabr/smart_initialguess_sabr_input.eps",width=6,height=5)



m<-read.table('/home/fabien/mypapers/explicit_sabr/sabr_smart_fit.txt', header=TRUE)
m$alpha <- as.numeric(as.character(m$alpha))
m$error <- as.numeric(as.character(m$error))
qplot(expiry, error, data=m[m$asset != "1",], color=guess, geom="line", ylab="implied volatility RMSE")+scale_y_log10()+facet_wrap(~  asset)
#+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/explicit_de_equity_error.eps",width=10,height=5)

qplot(expiry, alpha, data=m[m$asset != "1",], color=guess, geom="line")+facet_wrap(~  asset)
#+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/explicit_de_equity_alpha.eps",width=10,height=5)

m<-read.table('/home/fabien/mypapers/explicit_sabr/explicit_fit_parabola_5_equity_all_alpha1.txt', header=TRUE)
m$alpha <- as.numeric(as.character(m$alpha))
m$RMSE <- as.numeric(as.character(m$RMSE))
qplot(Expiry, RMSE, data=m, color=Method, geom="line", ylab="implied volatility RMSE")+scale_x_log10()+scale_y_log10()+facet_wrap(~  Asset)
alpha1 <- m[m$Method == "alpha_1",]
alpharhonu1 <- m[m$Method == "alpharhonu_1",]
alpha1$RMSEDifference <- 0
alpha1$RMSEDifference = alpha1$RMSE - alpharhonu1$RMSE
qplot(Expiry, RMSEDifference, data=alpha1, color=Method, geom="line", ylab="implied volatility RMSE")+facet_wrap(~  Asset)



m<-read.table('/home/fabien/mypapers/explicit_sabr/explicit_fit_1m_beta05.txt', header=TRUE)
m <- m[m$Method != "Explicit-0",]
m$Strike <- as.numeric(as.character(m$Strike))
m$bpvol <- as.numeric(as.character(m$bpvol))
qplot(Strike, bpvol, data=m[m$Method != "Reference",], color=Method, geom="line")+geom_point(data=m[m$Method=="Reference",])+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/explicit_fit_1m_beta05.eps",width=5,height=5)

m<-read.table('/home/fabien/mypapers/explicit_sabr/explicit_0a_fit_1m1y_beta05.txt', header=TRUE)
m$Strike <- as.numeric(as.character(m$Strike))
m$bpvol <- as.numeric(as.character(m$bpvol))
qplot(Strike, bpvol, data=m[m$Method != "Reference",], color=Method, geom="line")+geom_point(data=m[m$Method=="Reference",])+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/explicit_fit_1m1y_beta05.eps",width=5,height=5)

m<-read.table('/home/fabien/mypapers/explicit_sabr/explicit_fit_2y_beta05.txt', header=TRUE)
m$Strike <- as.numeric(as.character(m$Strike))
m$bpvol <- as.numeric(as.character(m$bpvol))
qplot(Strike, bpvol, data=m[m$Method != "Reference",], color=Method, geom="line")+geom_point(data=m[m$Method=="Reference",])+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/explicit_fit_2y_beta05.eps",width=5,height=5)

m<-read.table('/home/fabien/mypapers/explicit_sabr/explicit_fit_10y_beta05.txt', header=TRUE)
m$Strike <- as.numeric(as.character(m$Strike))
m$bpvol <- as.numeric(as.character(m$bpvol))
qplot(Strike, bpvol, data=m[m$Method != "Reference",], color=Method, geom="line")+geom_point(data=m[m$Method=="Reference",])+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/explicit_fit_10_beta05.eps",width=5,height=5)

m<-read.table('/home/fabien/mypapers/explicit_sabr/explicit_fit_sabr_0153_beta1.txt', header=TRUE)
m$Strike <- as.numeric(as.character(m$Strike))
m$Volatility <- as.numeric(as.character(m$Volatility))
qplot(Strike, Volatility, data=m[m$Method != "Reference",], color=Method, geom="line")+geom_point(data=m[m$Method=="Reference",])+theme(legend.position="bottom")

m<-read.table('/home/fabien/mypapers/explicit_sabr/explicit_fit_sabr_0479_beta1.txt', header=TRUE)
m$Strike <- as.numeric(as.character(m$Strike))
m$Volatility <- as.numeric(as.character(m$Volatility))
my.labs <- list(bquote(alpha_0),bquote(alpha_1), "Parabola", "Reference")

qplot(Strike, Volatility, data=m[m$Method != "Reference",], color=Method, linetype=Method, geom="line")+geom_point(data=m[m$Method=="Reference",])+scale_color_manual("Stage", values=c(3,1,4,2), breaks=c("alpha_0","alpha_1","Parabola","Reference"), labels=c(expression(paste(alpha[0]," guess")), expression(paste(alpha[1]," guess")), "Parabola", "Reference"))+scale_linetype_manual("Stage", values=c(2,1,4,0), breaks=c("alpha_0","alpha_1","Parabola","Reference"), labels=c(expression(paste(alpha[0]," guess")), expression(paste(alpha[1]," guess")), "Parabola", "Reference"))+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/explicit_fit_sabr_0479_beta1.eps",width=7,height=7)

m<-read.table('/home/fabien/mypapers/explicit_sabr/two_sabr_1_smile.txt', header=TRUE)
m$Strike <- as.numeric(as.character(m$Strike))
m$Volatility <- as.numeric(as.character(m$Volatility))
qplot(Strike, Volatility, data=m[m$alpha != "Reference",], color=alpha, linetype=alpha, geom="line")+geom_point(data=m[m$alpha=="Reference",])+scale_color_manual("Parameters", values=c(3,1,2), breaks=c("Low","High","Reference"), labels=c(expression(paste(alpha," = 0.237")), expression(paste(alpha," = 0.850")), "reference vols"))+scale_linetype_manual("Parameters", values=c(2,1,0), c("Low","High","Reference"), labels=c(expression(paste(alpha," = 0.237")), expression(paste(alpha," = 0.850")), "reference vols"))+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/two_sabr_1_smile.eps",width=7,height=7)

m<-read.table('/home/fabien/mypapers/explicit_sabr/explicit_fit_1m_beta05_arb.txt', header=TRUE)
m$Strike <- as.numeric(as.character(m$Strike))
m$bpvol <- as.numeric(as.character(m$Volatility))
qplot(Strike, bpvol, data=m[m$Method != "Reference",], color=Method, geom="line")+geom_point(data=m[m$Method=="Reference",])+theme(legend.position="bottom")

m<-read.table('/home/fabien/mypapers/explicit_sabr/normal_vs_arbfree_fit_10y10y.txt', header=TRUE)
m$Strike <- as.numeric(as.character(m$Strike))
m$bpvol <- as.numeric(as.character(m$Volatility))
qplot(Strike, bpvol, data=m[m$Method != "Reference",], color=Method, geom="line")+geom_point(data=m[m$Method=="Reference",])+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/normal_vs_arbfree_fit_10y10y.eps",width=7,height=7)

