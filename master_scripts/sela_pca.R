#pca with selaginella
sela <- read.csv("raw_data/sela_raw.csv")
sela2 <- sela[complete.cases(sela),]
sela3 <- sela2[,-c(1:2)]

# sela_noeuro <- droplevels(sela[sela$species != "Sel_eur",])
# sela_noeuro2 <- sela_noeuro[complete.cases(sela_noeuro),]
# sela_noeuro3 <- sela_noeuro2[,-c(1:2)]


library(vegan)
##look at variances
var(sela$cond)
var(sela$lma)
#variances among variables are orders of magnitude different so we will rescale

sela_dca <- decorana(na.omit(sela3))
summary(sela_dca, display = 'none') #since axis length is less than 3 so we use PCA

#principle compoent analysis with scales variances
sela_rda<- rda(sela3,scale=T)
# plot(sela_rda)
summary(sela_rda)

#nicer plot

library(scales)
sites <- scores(sela_rda, display='sites')
spp <- scores(sela_rda, display='species')
#need to rename row names for pretty plotting

row.names(spp) <- c("Anet", "GS","CHL", "N", "C:N", "P", "LMA", "LSD", "LSS", "LCP", "ITE")
len <- .8

library(RColorBrewer)
cols <- brewer.pal(7, "Dark2")
cols2 <- c(rep(cols[1],5), rep(cols[2],4),rep(cols[3],3),rep(cols[4],5),rep(cols[5],2),
           rep(cols[6],5),rep(cols[7],4))

# windows(7,7)
png(filename = "output/pca_sela.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,5,2,2), las=1,cex.axis=0.8,)
plot(sites,ylab="PC 2 (22.3 %)", xlab="PC 1 (40.4 %)",
     cex=1.5, bg=alpha(cols2, .8), pch=21,xlim=c(-2, 2), ylim=c(-2, 2))
  # text(spp,labels=rownames(scores(sela_rda, display='species')),cex=1,col="grey20")
  text(spp,labels=rownames(spp),cex=1)
  abline(v=0, lty='dashed')
  abline(h=0, lty='dashed')
  arrows(0, 0, len * spp[, 1],  len * spp[, 2], length = 0.05)
  legend("topright", legend= unique(sela$species),pch=21, pt.bg=cols,
         inset=0.01, bty='n', cex=.9)
  # dev.copy2pdf(file= "output/pca_sela.pdf")
  dev.off()

  
#no sel eur------------
# sela2_dca <- decorana(na.omit(sela_noeuro3))
#   summary(sela2_dca, display = 'none') #since axis length is less than 3 so we use PCA
#   speclab <- unique(sela$species)
#   
# #principle compoent analysis with scales variances
# sela2_rda<- rda(na.omit(sela_noeuro3),scale=T)
# sites2 <- scores(sela2_rda, display='sites')
# spp2 <- scores(sela2_rda, display='species')
# 
# cols3 <- c(rep(cols[2],4),rep(cols[3],3),rep(cols[4],5),rep(cols[5],2),
#            rep(cols[6],5),rep(cols[7],4))

# windows(7,7)  
# plot(sites2,ylab="PC 2 (22.3 %)", xlab="PC 1 (40.4 %)",
#        cex=1.5, col=alpha(cols3, .8), pch=16,xlim=c(-2, 2), ylim=c(-2, 2))
#   text(spp2,labels=rownames(scores(sela2_rda, display='species')),cex=1,col="grey20")
#   abline(v=0, lty='dashed')
#   abline(h=0, lty='dashed')
#   arrows(0, 0, len * spp2[, 1],  len * spp2[, 2], length = 0.05, col = "grey20")
#   legend("topright", legend= speclab[2:7],col=cols, pch=16, 
#          inset=0.01,bty='n', cex=.8 )
# dev.copy2pdf(file= "output/pca_sela_noeury.pdf")
# dev.off() 

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
