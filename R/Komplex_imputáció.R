
library(foreign)
library(dplyr)
library(Amelia)
library(mice)
library(haven)

adat <- as_tibble(read.spss("ADATBÁZIS NEVE", to.data.frame = T, use.value.labels = F))



adathiany.nagysag_1=round(nrow(adat)*0.2,0) #JÖVEDELEM ADATHIÁNY MÉRTÉKE
adathiany.nagysag_2=round(nrow(adat)*0.1,0) #DEMOGRÁFIAI ADATHIÁNY MÉRTÉKE


iteracio=100

res.matrix <- as_tibble(matrix(0, nrow=iteracio, ncol=16))
colnames(res.matrix) <- c("cc.mean","cc.sd","cc.cor","mi.abs", "mi.k.abs", "mi.i.abs", "mi.h.abs", "mi.mean",
                          "mi.sd","mi.cor", "cc.mean.elt", "cc.sd.elt", "cc.cor.elt", "mi.mean.elt", "mi.sd.elt", "mi.cor.elt")

for (i in 1:iteracio)
{ 


adat$jov_adathany <- adat$jov
adat$kor_adathany <- adat$kor
adat$isk_adathany <- adat$isk
adat$htsz_adathany <-adat$htsz

kivesz1 <- sample(1:nrow(adat), adathiany.nagysag_1, prob = adat$weight)
kivesz2 <- sample(1:nrow(adat), adathiany.nagysag_2, prob = adat$weight_d)
kivesz3 <- sample(1:nrow(adat), adathiany.nagysag_2, prob = adat$weight_d)
kivesz4 <- sample(1:nrow(adat), adathiany.nagysag_2, prob = adat$weight_d)

adat$jov_adathany[kivesz1] <- NA
adat$kor_adathany[kivesz2] <- NA
adat$isk_adathany[kivesz3] <- NA
adat$htsz_adathany[kivesz4] <- NA

#Complete cases

res.matrix$cc.mean[i] <- mean(adat$jov_adathany, na.rm=T)
res.matrix$cc.sd[i] <- sd(adat$jov_adathany, na.rm=T)
res.matrix$cc.cor[i] <- cor(adat$jov_adathany, adat$eletsz, use = "pairwise.complete.obs")

res.matrix$cc.mean.elt[i] <- abs(mean(adat$jov) - mean(adat$jov_adathany, na.rm=T))
res.matrix$cc.sd.elt[i] <- abs(sd(adat$jov) - sd(adat$jov_adathany, na.rm=T))
res.matrix$cc.cor.elt[i] <- abs(cor(adat$jov, adat$eletsz) - cor(adat$jov_adathany, adat$eletsz, use = "pairwise.complete.obs"))


#MI

m.adat <- adat[,c("htsz_adathany","neme","isk_adathany","kor_adathany", "teltip_2", "teltip_3","teltip_4", "jov_adathany")]


init = mice(m.adat, maxit=15) 
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

mi1.dif <- mean(abs(adat$jov[kivesz1]-mi1[kivesz1]))
mi2.dif <- mean(abs(adat$jov[kivesz1]-mi2[kivesz1]))
mi3.dif <- mean(abs(adat$jov[kivesz1]-mi3[kivesz1]))
mi4.dif <- mean(abs(adat$jov[kivesz1]-mi4[kivesz1]))
mi5.dif <- mean(abs(adat$jov[kivesz1]-mi5[kivesz1]))

res.matrix$mi.abs[i] <- mean(c(mi1.dif,mi2.dif,mi3.dif,mi4.dif,mi5.dif))

res.matrix$mi.mean.elt[i] <- abs(mean(adat$jov) - mean(c(mean(mi1),mean(mi2),mean(mi3),mean(mi4),mean(mi5))))
res.matrix$mi.sd.elt[i] <-  abs(sd(adat$jov) - mean(c(sd(mi1),sd(mi2),sd(mi3),sd(mi4),sd(mi5))))
res.matrix$mi.cor.elt[i] <- abs(cor(adat$jov, adat$eletsz) - mean(c(cor(mi1, adat$eletsz),cor(mi2, adat$eletsz),cor(mi3, adat$eletsz),cor(mi4, adat$eletsz), cor(mi5, adat$eletsz))))

mi1_k <- complete(mice.imputed, action=1)$kor_adathany
mi2_k <- complete(mice.imputed, action=2)$kor_adathany
mi3_k <- complete(mice.imputed, action=3)$kor_adathany
mi4_k <- complete(mice.imputed, action=4)$kor_adathany
mi5_k <- complete(mice.imputed, action=5)$kor_adathany

mi1_k.dif <- mean(abs(adat$kor[kivesz2]-mi1_k[kivesz2]))
mi2_k.dif <- mean(abs(adat$kor[kivesz2]-mi2_k[kivesz2]))
mi3_k.dif <- mean(abs(adat$kor[kivesz2]-mi3_k[kivesz2]))
mi4_k.dif <- mean(abs(adat$kor[kivesz2]-mi4_k[kivesz2]))
mi5_k.dif <- mean(abs(adat$kor[kivesz2]-mi5_k[kivesz2]))

res.matrix$mi.k.abs[i] <- mean(c(mi1_k.dif,mi2_k.dif,mi3_k.dif,mi4_k.dif,mi5_k.dif))

mi1_i <- complete(mice.imputed, action=1)$isk_adathany
mi2_i <- complete(mice.imputed, action=2)$isk_adathany
mi3_i <- complete(mice.imputed, action=3)$isk_adathany
mi4_i <- complete(mice.imputed, action=4)$isk_adathany
mi5_i <- complete(mice.imputed, action=5)$isk_adathany

mi1_i.dif <- mean(abs(adat$isk[kivesz3]-mi1_i[kivesz3]))
mi2_i.dif <- mean(abs(adat$isk[kivesz3]-mi2_i[kivesz3]))
mi3_i.dif <- mean(abs(adat$isk[kivesz3]-mi3_i[kivesz3]))
mi4_i.dif <- mean(abs(adat$isk[kivesz3]-mi4_i[kivesz3]))
mi5_i.dif <- mean(abs(adat$isk[kivesz3]-mi5_i[kivesz3]))

res.matrix$mi.i.abs[i] <- mean(c(mi1_i.dif,mi2_i.dif,mi3_i.dif,mi4_i.dif,mi5_i.dif))

mi1_h <- complete(mice.imputed, action=1)$htsz_adathany
mi2_h <- complete(mice.imputed, action=2)$htsz_adathany
mi3_h <- complete(mice.imputed, action=3)$htsz_adathany
mi4_h <- complete(mice.imputed, action=4)$htsz_adathany
mi5_h <- complete(mice.imputed, action=5)$htsz_adathany

mi1_h.dif <- mean(abs(adat$htsz[kivesz4]-mi1_h[kivesz4]))
mi2_h.dif <- mean(abs(adat$htsz[kivesz4]-mi2_h[kivesz4]))
mi3_h.dif <- mean(abs(adat$htsz[kivesz4]-mi3_h[kivesz4]))
mi4_h.dif <- mean(abs(adat$htsz[kivesz4]-mi4_h[kivesz4]))
mi5_h.dif <- mean(abs(adat$htsz[kivesz4]-mi5_h[kivesz4]))

res.matrix$mi.h.abs[i] <- mean(c(mi1_h.dif,mi2_h.dif,mi3_h.dif,mi4_h.dif,mi5_h.dif))

}

write_sav(res.matrix, "ADATBÁZIS NEVE")