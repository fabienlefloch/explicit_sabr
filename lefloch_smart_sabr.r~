m<-read.table('/home/fabien/mypapers/explicit_sabr/smart_initialguess_sabr_input.txt', header=TRUE)
m$alpha <- as.numeric(as.character(m$alpha))
m$nu <- as.numeric(as.character(m$nu))
m$rho <- as.numeric(as.character(m$rho))
m$Error <- as.numeric(as.character(m$Error))
ref <- m[m$Method=="Reference",]
smart <- m[m$Method=="Smart",]
m$alphae <- 0
smart$alphae <- smart$alpha - ref$alpha
m$rhoe <- 0
smart$rhoe <- smart$rho - ref$rho
m$nue <- 0
smart$nue <- smart$nu - ref$nu

my.labs <- list("vol RMS", bquote(alpha),bquote(rho), bquote(nu))

qplot(Expiry, abs(alphae), data=smart, geom="line", color="alpha", linetype="alpha", ylab="Error")+geom_line(aes(y=abs(rhoe), color="rho", linetype="rho"))+geom_line(aes(y=abs(nue), color="nu", linetype="nu"))+geom_line(aes(y=abs(Error),color="vol RMS", linetype="vol RMS"))+scale_y_log10(breaks=c(1e-2, 1e-3, 1e-4, 1e-5, 1e-6))+scale_colour_manual("parameter",values=c(4,2,3,1),breaks=c("vol RMS","alpha", "rho", "nu"), labels=my.labs)+scale_linetype_manual("parameter",values=c(4,2,3,1),breaks=c("vol RMS","alpha", "rho", "nu"), labels=my.labs)+theme(legend.position=c(0.85, 0.4))
ggsave(file="/home/fabien/mypapers/explicit_sabr/smart_initialguess_sabr_input.eps",width=6,height=5)

m<-read.table('/home/fabien/mypapers/explicit_sabr/sabr_smart_fit.txt', header=TRUE)
m$alpha <- as.numeric(as.character(m$alpha))
m$error <- as.numeric(as.character(m$error))
qplot(expiry, error, data=m[m$asset != "1",], color=guess, geom="line")+scale_y_log10()+facet_wrap(~  asset)+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/explicit_de_equity_error.eps",width=8,height=8)

qplot(expiry, alpha, data=m[m$asset != "1",], color=guess, geom="line")+facet_wrap(~  asset)+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/explicit_de_equity_alpha.eps",width=8,height=8)

m<-read.table('/home/fabien/mypapers/explicit_sabr/explicit_fit_1m_beta05.txt', header=TRUE)
m$Strike <- as.numeric(as.character(m$Strike))
m$bpvol <- as.numeric(as.character(m$bpvol))
qplot(Strike, bpvol, data=m[m$Method != "Reference",], color=Method, geom="line")+geom_point(data=m[m$Method=="Reference",])+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/explicit_fit_1m_beta05.eps",width=5,height=5)

m<-read.table('/home/fabien/mypapers/explicit_sabr/explicit_fit_2y_beta05.txt', header=TRUE)
m$Strike <- as.numeric(as.character(m$Strike))
m$bpvol <- as.numeric(as.character(m$bpvol))
qplot(Strike, bpvol, data=m[m$Method != "Reference",], color=Method, geom="line")+geom_point(data=m[m$Method=="Reference",])+theme(legend.position="bottom")
ggsave(file="/home/fabien/mypapers/explicit_sabr/explicit_fit_2y_beta05.eps",width=5,height=5)

m<-read.table('/home/fabien/mypapers/explicit_sabr/explicit_fit_sabr_0153_beta1.txt', header=TRUE)
m$Strike <- as.numeric(as.character(m$Strike))
m$Volatility <- as.numeric(as.character(m$Volatility))
qplot(Strike, Volatility, data=m[m$Method != "Reference",], color=Method, geom="line")+geom_point(data=m[m$Method=="Reference",])+theme(legend.position="bottom")

m<-read.table('/home/fabien/mypapers/explicit_sabr/explicit_fit_sabr_0479_beta1.txt', header=TRUE)
m$Strike <- as.numeric(as.character(m$Strike))
m$Volatility <- as.numeric(as.character(m$Volatility))
qplot(Strike, Volatility, data=m[m$Method != "Reference",], color=Method, geom="line")+geom_point(data=m[m$Method=="Reference",])+theme(legend.position="bottom")
