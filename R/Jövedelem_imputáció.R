
library(foreign)
library(dplyr)
library(Amelia)
library(mice)
library(haven)

adat <- as_tibble(read.spss("ADATBÁZIS NEVE", to.data.frame = T, use.value.labels = F))



adathiany.nagysag=round(nrow(adat)*0.3,0) #JÖVEDELEM ADATHIÁNY MÉRTÉKE


iteracio=100

res.matrix <- as_tibble(matrix(0, nrow=iteracio, ncol=34))
colnames(res.matrix) <- c("cc.mean","cc.sd","cc.cor","am.abs", "am.mean", "am.sd", "am.cor",
                          "reg.abs","reg.mean","reg.sd","reg.cor","pmm.abs", "pmm.mean", "pmm.sd", "pmm.cor", 
                          "mi.abs","mi.mean","mi.sd","mi.cor", "cc.mean.elt", "cc.sd.elt", "cc.cor.elt",
                          "am.mean.elt", "am.sd.elt", "am.cor.elt", "reg.mean.elt", "reg.sd.elt", "reg.cor.elt",
                          "pmm.mean.elt", "pmm.sd.elt", "pmm.cor.elt", "mi.mean.elt", "mi.sd.elt", "mi.cor.elt")

for (i in 1:iteracio)
{ 


adat$jov_adathany <- adat$jov
kivesz <- sample(1:nrow(adat), adathiany.nagysag, prob = adat$weight)

adat$jov_adathany[kivesz] <- NA

#Complete cases

res.matrix$cc.mean[i] <- mean(adat$jov_adathany, na.rm=T)
res.matrix$cc.sd[i] <- sd(adat$jov_adathany, na.rm=T)
res.matrix$cc.cor[i] <- cor(adat$jov_adathany, adat$eletsz, use = "pairwise.complete.obs")

res.matrix$cc.mean.elt[i] <- abs(mean(adat$jov) - mean(adat$jov_adathany, na.rm=T))
res.matrix$cc.sd.elt[i] <- abs(sd(adat$jov) - sd(adat$jov_adathany, na.rm=T))
res.matrix$cc.cor.elt[i] <- abs(cor(adat$jov, adat$eletsz) - cor(adat$jov_adathany, adat$eletsz, use = "pairwise.complete.obs"))


#Átlag adatpótlás


adat$jov_adathany_am <- adat$jov_adathany
adat$jov_adathany_am[kivesz] <- mean(adat$jov_adathany, na.rm=T)

res.matrix$am.mean[i] <- mean(adat$jov_adathany_am)
res.matrix$am.sd[i] <- sd(adat$jov_adathany_am)
res.matrix$am.cor[i] <- cor(adat$jov_adathany_am ,adat$eletsz)
res.matrix$am.abs[i] <-  mean(abs(adat$jov[kivesz] - mean(adat$jov_adathany, na.rm=T)))

res.matrix$am.mean.elt[i] <- abs(mean(adat$jov) - mean(adat$jov_adathany_am))
res.matrix$am.sd.elt[i] <- abs(sd(adat$jov) - sd(adat$jov_adathany_am))
res.matrix$am.cor.elt[i] <- abs(cor(adat$jov, adat$eletsz) - cor(adat$jov_adathany_am ,adat$eletsz))

#Lineáris regresszió

m1 <- lm(jov_adathany ~ htsz + neme + kor + isk, data=adat)


adat$jov_adathany_reg <- adat$jov_adathany
adat$jov_adathany_reg[kivesz] <- predict.lm(m1,adat[,c("htsz","neme","isk","kor" ,"teltip_1", "teltip_2", "teltip_3", "teltip_4")])[kivesz]

res.matrix$reg.mean[i] <- mean(adat$jov_adathany_reg)
res.matrix$reg.sd[i] <- sd(adat$jov_adathany_reg)
res.matrix$reg.cor[i] <- cor(adat$jov_adathany_reg, adat$eletsz)
res.matrix$reg.abs[i] <-  mean(abs(adat$jov[kivesz] - predict.lm(m1,adat[,c("htsz","neme","isk","kor", "teltip_1", "teltip_2", "teltip_3", "teltip_4")])[kivesz]))

res.matrix$reg.mean.elt[i] <- abs(mean(adat$jov) - mean(adat$jov_adathany_reg))
res.matrix$reg.sd.elt[i] <- abs(sd(adat$jov) - sd(adat$jov_adathany_reg))
res.matrix$reg.cor.elt[i] <- abs(cor(adat$jov, adat$eletsz) - cor(adat$jov_adathany_reg, adat$eletsz))

#PMM

pmm.adat <- adat[,c("htsz","neme","isk","kor", "teltip_2", "teltip_3","teltip_4", "jov_adathany")]


initpmm = mice(pmm.adat, maxit=10) 
predpmm = initpmm$predictorMatrix

mice.imputed_ppm = mice(pmm.adat, method="pmm", predictorMatrix= predpmm, m=1)

pmm <- complete(mice.imputed_ppm, action=1)$jov_adathany


res.matrix$pmm.mean[i] <- mean(pmm)
res.matrix$pmm.sd[i] <-  sd(pmm)
res.matrix$pmm.cor[i] <- cor(pmm, adat$eletsz)

res.matrix$pmm.abs[i] <- mean(abs(adat$jov[kivesz]-pmm[kivesz]))

res.matrix$pmm.mean.elt[i] <- abs(mean(adat$jov) - mean(pmm))
res.matrix$pmm.sd.elt[i] <-  abs(sd(adat$jov) - sd(pmm))
res.matrix$pmm.cor.elt[i] <- abs(cor(adat$jov, adat$eletsz) - cor(pmm, adat$eletsz))

#MI

m.adat <- adat[,c("htsz","neme","isk","kor", "teltip_2", "teltip_3","teltip_4", "jov_adathany")]


init = mice(m.adat, maxit=10) 
meth = "pmm"
predM = init$predictorMatrix

mice.imputed = mice(m.adat, method=meth, predictorMatrix=predM, m=5)

mi1 <- complete(mice.imputed, action=1)$jov_adathany
mi2 <- complete(mice.imputed, action=2)$jov_adathany
mi3 <- complete(mice.imputed, action=3)$jov_adathany
mi4 <- complete(mice.imputed, action=4)$jov_adathany
mi5 <- complete(mice.imputed, action=5)$jov_adathany

res.matrix$mi.mean[i] <-  mean(c(mean(mi1),mean(mi2),mean(mi3),mean(mi4),mean(mi5)))
res.matrix$mi.sd[i] <-  mean(c(sd(mi1),sd(mi2),sd(mi3),sd(mi4),sd(mi5)))
res.matrix$mi.cor[i] <- mean(c(cor(mi1, adat$eletsz),cor(mi2, adat$eletsz),cor(mi3, adat$eletsz),cor(mi4, adat$eletsz), cor(mi5, adat$eletsz)))

mi1.dif <- mean(abs(adat$jov[kivesz]-mi1[kivesz]))
mi2.dif <- mean(abs(adat$jov[kivesz]-mi2[kivesz]))
mi3.dif <- mean(abs(adat$jov[kivesz]-mi3[kivesz]))
mi4.dif <- mean(abs(adat$jov[kivesz]-mi4[kivesz]))
mi5.dif <- mean(abs(adat$jov[kivesz]-mi5[kivesz]))

res.matrix$mi.abs[i] <- mean(c(mi1.dif,mi2.dif,mi3.dif,mi4.dif,mi5.dif))

res.matrix$mi.mean.elt[i] <- abs(mean(adat$jov) - mean(c(mean(mi1),mean(mi2),mean(mi3),mean(mi4),mean(mi5))))
res.matrix$mi.sd.elt[i] <-  abs(sd(adat$jov) - mean(c(sd(mi1),sd(mi2),sd(mi3),sd(mi4),sd(mi5))))
res.matrix$mi.cor.elt[i] <- abs(cor(adat$jov, adat$eletsz) - mean(c(cor(mi1, adat$eletsz),cor(mi2, adat$eletsz),cor(mi3, adat$eletsz),cor(mi4, adat$eletsz), cor(mi5, adat$eletsz))))


}

write_sav(res.matrix, "ADATBÁZIS NEVE")