#pca with selaginella

#variables to potentially drop (C, C:N or N keep one, )

sela <- read.csv("raw_data/sela_raw.csv")
sela2 <- sela[,-c(1:2)]

library(vegan)

##look at variances
var(sela$cond)
var(sela$lma)
#variances among variables are orders of magnitude different so we will rescale

sela_dca <- decorana(na.omit(sela2))
summary(sela_dca, display = 'none') #since axis length is less than 3 so we use PCA

#principle compoent analysis with scales variances
sela_rda<- rda(na.omit(sela2),scale=T)
plot(sela_rda)
summary(sela_rda)

#nicer plot

library(scales)
sites <- scores(sela_rda, display='sites')
spp <- scores(sela_rda, display='species')
len <- .75


library(RColorBrewer)
cols <- brewer.pal(7, "Dark2")
cols2 <- c(rep(cols[1],5), rep(cols[2],5),rep(cols[3],5),rep(cols[4],5),rep(cols[5],5),
           rep(cols[6],5),rep(cols[7],5))

windows(7,7)
plot(sites,ylab="PC 2 (26.5 %)", xlab="PC 1 (43.4 %)",
     cex=1.5, col=alpha(cols2, .8), pch=16,xlim=c(-2, 2), ylim=c(-2, 2))
  text(spp,labels=rownames(scores(sela_rda, display='species')),cex=1,
       col="grey20")
  abline(v=0, lty='dashed')
  abline(h=0, lty='dashed')
  arrows(0, 0, len * spp[, 1],  len * spp[, 2], length = 0.05, col = "grey20")
  legend("topright", legend= unique(sela$species),col=cols, pch=16, 
         inset=0.01,bty='n', cex=.8 )
  dev.copy2pdf(file= "output/pca_sela.pdf")
  dev.off()

# use prcomp --------------------------------------------------------------

# sela_prc <- prcomp(na.omit(sela2),scale=T)
# summary(sela_prc)
# sites2 <- scores(sela_prc, display='sites')
# spp2 <- scores(sela_prc, display='species')
# biplot(sela_prc, pch=16, col=c(cols, "blue"))
# 
# plot(sites2,ylab="PC 2 (26.5 %)", xlab="PC 1 (43.4 %)",
#      cex=1, col=alpha(cols2, .8), pch=16,xlim=c(-2.5, 2.5), ylim=c(-1.5, 1.5))
# text(spp,labels=rownames(scores(sela_rda, display='species')),cex=0.8, col="black")
