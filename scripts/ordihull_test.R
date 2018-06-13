#pca with selaginella
sela <- read.csv("raw_data/sela_raw.csv")
sela2 <- sela[complete.cases(sela),]
hab <- read.csv("raw_data/treatments.csv")
sela3 <- merge(sela2, hab)


sela3 <- sela2[,-c(1:2)] ##use this in the rda not here

library(vegan)

#principle compoent analysis with scales variances
sela_rda<- rda(sela3[,3:13],scale=T)

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
pchs <- c(rep(21,5), rep(22,4), rep(21,3), rep(24,5), rep(22,5), 
          rep(24,5), rep(21,4))
habs <- c("Closed Canopy", "Open Canopy", "Swamp")
habpch <- c(21, 24, 22)

ordicols <- c(alpha("forestgreen", .85), alpha("gold", .85), alpha("royalblue", .85))

windows()
# png(filename = "output/pca_sela.png", width = 11, height = 8.5, 
#     units = "in", res= 400)

par(mar=c(5,5,2,2), las=1,cex.axis=0.8)
plot(sites,ylab="PC 2 (22.3 %)", xlab="PC 1 (40.4 %)",
     cex=1.5, bg=alpha(cols2, .8), pch=pchs,xlim=c(-2, 2), ylim=c(-2, 2))
# text(spp,labels=rownames(scores(sela_rda, display='species')),cex=1,col="grey20")
text(spp,labels=rownames(spp),cex=1)
abline(v=0, lty='dashed')
abline(h=0, lty='dashed')
arrows(0, 0, len * spp[, 1],  len * spp[, 2], length = 0.05)
legend("topright", legend= unique(sela$species),pch=21, pt.bg=cols,
       inset=0.01, bty='n', cex=.9,pt.cex=1.25)
legend("topleft", legend= habs,pt.bg="white", pch=habpch,
       inset=0.01, bty='n', cex=.9, pt.cex=1.25)

ordihull(sela_rda, group=sela3$habitat, col=ordicols, draw = "polygon",
         label=T, alpha = 50)  

dev.copy2pdf(file= "output/pca_sela.pdf")
dev.off()