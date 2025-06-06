#' copycat main_func.
#' @import edgeR
#' @import cluster
#' @param rawmat raw data matrix; genes in rows; cell names in columns.
#' @param id.type gene id type: Symbol or Ensemble.
#' @param LOW.DR minimal population fractions of genes for smoothing.
#' @param UP.DR minimal population fractions of genes for segmentation.
#' @param win.size minimal window sizes for segmentation.
#' @param norm.cell.names a vector of normal cell names.
#' @param KS.cut segmentation parameters, input 0 to 1; larger looser criteria.
#' @param sam.name sample name.
#' @param n.cores number of cores for parallel computing.
#' @param ngene.chr minimal number of genes per chromosome for cell filtering.
#' @param distance  distance methods include euclidean, and correlation converted distance include pearson and spearman.
#' @param output.seg TRUE or FALSE, output seg file for IGV visualization
#' @param plot.genes TRUE or FALSE, output heatmap of CNVs with genename labels
#' @param annotation annotation file for all known genes
#' @param remove.cc.genes Whether to remove cell cycle genes from the data
#' @param min.gene.per.cell, default 200
#' @param maxit, default 10000

#' @return 1) aneuploid/diploid prediction results; 2) CNA results in 220KB windows; 3) heatmap; 4) hclustering object.
#'
#' @examples
#' test.ck <- copykat(rawmat=rawdata,id.type="S", ngene.chr=5, win.size=25, KS.cut=0.1,sam.name="test", distance="euclidean", norm.cell.names="", n.cores=4, output.seg="FALSE", min.gene.per.cell=200)

#'
#' test.pred <- test.ck$prediction
#' @export
###


copykat <- function(rawmat=rawdata, id.type="S", ngene.chr=5,min.gene.per.cell=200,
                    LOW.DR=0.05, UP.DR=0.1, win.size=25,
                    norm.cell.names="", KS.cut=0.1,normal_cell_fraction=0.05,
                    sam.name="", distance="euclidean", output.seg="FALSE",
                    plot.genes="FALSE",annotation=NULL,remove.MHC.genes=T, n.cores=1,maxit=10000){

start_time <- Sys.time()
  set.seed(1234)
  sample.name <- paste(sam.name,"_copykat_", sep="")

  print("running copykat v1.1.0")

  print("step1: read and filter data ...")
  print(paste(nrow(rawmat), " genes, ", ncol(rawmat), " cells in raw data", sep=""))

  genes.raw <- apply(rawmat, 2, function(x)(sum(x>0)))

  if(sum(genes.raw> min.gene.per.cell)==0) stop("none cells have more than min.gene.per.cell")
  if(sum(genes.raw<min.gene.per.cell)>1){
    rawmat <- rawmat[, -which(genes.raw< min.gene.per.cell)]
    print(paste("filtered out ", sum(genes.raw<=min.gene.per.cell), " cells with less than min.gene.per.cell; remaining ", ncol(rawmat), " cells", sep=""))
  }
  ##
  der<- apply(rawmat,1,function(x)(sum(x>0)))/ncol(rawmat)

  if(sum(der>LOW.DR)>=1){
    rawmat <- rawmat[which(der > LOW.DR), ]; print(paste(nrow(rawmat)," genes past LOW.DR filtering", sep=""))
  }


  WNS1 <- "data quality is ok"
  if(nrow(rawmat) < 7000){
    WNS1 <- "low data quality"
    UP.DR<- LOW.DR
    print("WARNING: low data quality; assigned LOW.DR to UP.DR...")
  }

  print("step 2: annotations gene coordinates ...")
  if (is.null(annotation)) {
    stop("Please specify the annotation to be used.")
  } else {
    anno.mat <- annotateGenes(mat = rawmat,annotation=annotation, ID.type = id.type) #SYMBOL or ENSEMBLE

    anno.mat <- anno.mat[order(as.numeric(anno.mat$abspos), decreasing = FALSE),]

  }

# print(paste(nrow(anno.mat)," genes annotated", sep=""))

  ### module 3 removing MHC-Class II gene
   if(remove.MHC.genes){
  HLAs <- anno.mat$hgnc_symbol[grepl('^HLA-',anno.mat$hgnc_symbol)]
  toRev <- which(anno.mat$hgnc_symbol %in% c(HLAs))
  if(length(toRev)>0){
    anno.mat <- anno.mat[-toRev, ]
  }
  }
#  print(paste(nrow(anno.mat)," genes after rm cell cycle genes", sep=""))
  ### secondary filtering
  ToRemov2 <- NULL
  for(i in 8:ncol(anno.mat)){
    cell <- cbind(anno.mat$chromosome_name, anno.mat[,i])
    cell <- cell[cell[,2]!=0,]
    if(length(as.numeric(cell))< 5){
      rm <- colnames(anno.mat)[i]
      ToRemov2 <- c(ToRemov2, rm)
    } else if(length(rle(cell[,1])$length)<length(unique((anno.mat$chromosome_name)))|min(rle(cell[,1])$length)< ngene.chr){
      rm <- colnames(anno.mat)[i]
      ToRemov2 <- c(ToRemov2, rm)
    }
    i<- i+1
  }

  if(length(ToRemov2)==(ncol(anno.mat)-7)) stop("all cells are filtered")
  if(length(ToRemov2)>0){
    anno.mat <-anno.mat[, -which(colnames(anno.mat) %in% ToRemov2)]
  }

  rawmat3 <- data.matrix(anno.mat[, 8:ncol(anno.mat)])
  norm.mat<- log(sqrt(rawmat3)+sqrt(rawmat3+1))
  norm.mat<- apply(norm.mat,2,function(x)(x <- x-mean(x)))
  #rawmat3 <- rawmat3+1
  #upper_quart = apply(rawmat3, 2, quantile, probs=0.75)
  #mean_upper_quart = mean(upper_quart)
  #norm.mat = sweep(rawmat3, 2, mean_upper_quart/upper_quart, "*")
  #norm.mat = log(norm.mat+1)


  colnames(norm.mat) <-  colnames(rawmat3)


  ##smooth data
  print("step 3: smoothing data with dlm ...")
  dlm.sm <- function(c){
    model <- dlm::dlmModPoly(order=1, dV=0.16, dW=0.001)
    x <- dlm::dlmSmooth(norm.mat[, c], model)$s
    x<- x[2:length(x)]
    x <- x-mean(x)
  }

  test.mc <-parallel::mclapply(1:ncol(norm.mat), dlm.sm, mc.cores = n.cores)
  norm.mat.smooth <- matrix(unlist(test.mc), ncol = ncol(norm.mat), byrow = FALSE)

  colnames(norm.mat.smooth) <- colnames(norm.mat)

  print("step 4: measuring baselines ...")
  if(length(norm.cell.names)>1){

        #print(paste(length(norm.cell.names), "normal cells provided", sep=""))
         NNN <- length(colnames(norm.mat.smooth)[which(colnames(norm.mat.smooth) %in% norm.cell.names)])
         print(paste(NNN, " known normal cells found in dataset", sep=""))

         if (NNN==0) stop("known normal cells provided; however none existing in testing dataset")
         print("run with known normal...")

         basel <- apply(norm.mat.smooth[, which(colnames(norm.mat.smooth) %in% norm.cell.names)],1,median); print("baseline is from known input")

          d <- parallelDist::parDist(t(norm.mat.smooth),threads =n.cores, method="euclidean") ##use smooth and segmented data to detect intra-normal cells

          km <- 6
          fit <- hclust(d, method="ward.D2")
          selection=data.frame(k=seq(2,50))
          selection$sil=0

          for (i in 1:nrow(selection)) {
            Clustering.results=cutree(fit,k = selection$k[i])
            sil=cluster::silhouette(Clustering.results,dist = d)
            selection$sil[i]=mean(sil[,3])
          }

          selected=selection$k[selection$sil==max(selection$sil)]
          if (length(selected)>1) {
            selected=max(selected)
          }
          km=selected
          CL <- cutree(fit, km)

           while(!all(table(CL)>5)){
          km <- km -1
          CL <- cutree(fit, k=km)
         if(km==2){
         break
         }
         }

        WNS <- "run with known normal"
        preN <- norm.cell.names
         ##relative expression using pred.normal cells
      	norm.mat.relat <- norm.mat.smooth-basel

        }else {
         basa <- baseline.norm.cl(norm.mat.smooth=norm.mat.smooth, min.cells=5, n.cores=n.cores,maxit=maxit)
          basel <- basa$basel
          WNS <- basa$WNS
          preN <- basa$preN
          CL <- basa$cl
          if (WNS =="unclassified.prediction"){

                    basa <- baseline.GMM(CNA.mat=norm.mat.smooth, max.normal=ifelse(normal_cell_fraction>1,
                                                                                    normal_cell_fraction,
                                                                                    normal_cell_fraction*ncol(norm.mat.smooth)), mu.cut=0.05, Nfraq.cut=0.99,RE.before=basa,n.cores=n.cores,maxit=maxit)
                    basel <-basa$basel
                    WNS <- basa$WNS

                    preN <- basa$preN

              }
          ##relative expression using pred.normal cells
             norm.mat.relat <- norm.mat.smooth-basel

             }

  ###use a smaller set of genes to perform segmentation
  DR2 <- apply(rawmat3,1,function(x)(sum(x>0)))/ncol(rawmat3)
  ##relative expression using pred.normal cells
  norm.mat.relat <- norm.mat.relat[which(DR2>=UP.DR),]

  ###filter cells
  anno.mat2 <- anno.mat[which(DR2>=UP.DR), ]

  ToRemov3 <- NULL
  for(i in 8:ncol(anno.mat2)){
    cell <- cbind(anno.mat2$chromosome_name, anno.mat2[,i])
    cell <- cell[cell[,2]!=0,]
    if(length(as.numeric(cell))< 5){
      rm <- colnames(anno.mat2)[i]
      ToRemov3 <- c(ToRemov3, rm)
    } else if(length(rle(cell[,1])$length)<length(unique((anno.mat$chromosome_name)))|min(rle(cell[,1])$length)< ngene.chr){
      rm <- colnames(anno.mat2)[i]
      ToRemov3 <- c(ToRemov3, rm)
    }
    i<- i+1
  }

  if(length(ToRemov3)==ncol(norm.mat.relat)) stop ("all cells are filtered")

  if(length(ToRemov3)>0){
    norm.mat.relat <-norm.mat.relat[, -which(colnames(norm.mat.relat) %in% ToRemov3)]
   #print(paste("filtered out ", length(ToRemov3), " cells with less than ",ngene.chr, " genes per chr", sep=""))
  }

  #print(paste("final segmentation: ", nrow(norm.mat.relat), " genes; ", ncol(norm.mat.relat), " cells", sep=""))

  CL <- CL[which(names(CL) %in% colnames(norm.mat.relat))]
  CL <- CL[order(match(names(CL), colnames(norm.mat.relat)))]

  print("step 5: segmentation...")
  results <- CNA.MCMC(clu=CL, fttmat=norm.mat.relat, bins=win.size, cut.cor = KS.cut, n.cores=n.cores)

  if(length(results$breaks)<25){
    print("too few breakpoints detected; decreased KS.cut to 50%")
    results <- CNA.MCMC(clu=CL, fttmat=norm.mat.relat, bins=win.size, cut.cor = 0.5*KS.cut, n.cores=n.cores)
  }

  if(length(results$breaks)<25){
    print("too few breakpoints detected; decreased KS.cut to 75%")
    results <- CNA.MCMC(clu=CL, fttmat=norm.mat.relat, bins=win.size, cut.cor = 0.5*0.5*KS.cut, n.cores=n.cores)
  }

  if(length(results$breaks)<25) stop ("too few segments; try to decrease KS.cut; or improve data")

  colnames(results$logCNA) <- colnames(norm.mat.relat)
  results.com <- apply(results$logCNA,2, function(x)(x <- x-mean(x)))
  RNA.copycat <- cbind(anno.mat2[, 1:7], results.com)

  saveRDS(RNA.copycat, paste(sample.name, "CNA_raw_results_gene_by_cell.rds", sep=""))


  print("step 6: convert to genomic bins...") ###need multi-core
  Aj <- convert.all.bins(DNA.mat = DNA.hg20, RNA.mat=RNA.copycat,annotation=annotation, n.cores = n.cores)

  uber.mat.adj <- data.matrix(Aj$RNA.adj[, 4:ncol(Aj$RNA.adj)])

  print("step 7: adjust baseline ...")



      #removed baseline adjustment
        if(distance=="euclidean"){
        hcc <- hclust(parallelDist::parDist(t(uber.mat.adj),threads =n.cores, method = distance), method = "ward.D")
        }else {
        hcc <- hclust(as.dist(1-cor(uber.mat.adj, method = distance)), method = "ward.D")
        }
        hc.umap <- cutree(hcc,2)
        names(hc.umap) <- colnames(results.com)

        cl.ID <- NULL
        for(i in 1:max(hc.umap)){
        cli <- names(hc.umap)[which(hc.umap==i)]
        pid <- length(intersect(cli, preN))/length(cli)
        cl.ID <- c(cl.ID, pid)
        i<- i+1
         }

        com.pred <- names(hc.umap)
        com.pred[which(hc.umap == which(cl.ID==max(cl.ID)))] <- "diploid"
        com.pred[which(hc.umap == which(cl.ID==min(cl.ID)))] <- "aneuploid"
        names(com.pred) <- names(hc.umap)

  ################removed baseline adjustment
        results.com.rat <- uber.mat.adj-apply(uber.mat.adj[,which(com.pred=="diploid")], 1, mean)
        results.com.rat <- apply(results.com.rat,2,function(x)(x <- x-mean(x)))
        results.com.rat.norm <- results.com.rat[,which(com.pred=="diploid")]; dim(results.com.rat.norm)

        cf.h <- apply(results.com.rat.norm, 1, sd)
        base <- apply(results.com.rat.norm, 1, mean)

        adjN <- function(j){
        a <- results.com.rat[, j]
        a[abs(a-base) <= 0.25*cf.h] <- mean(a)
        a
        }


        mc.adjN <-  parallel::mclapply(1:ncol(results.com.rat),adjN, mc.cores = n.cores)
        adj.results <- matrix(unlist(mc.adjN), ncol = ncol(results.com.rat), byrow = FALSE)
        colnames(adj.results) <- colnames(results.com.rat)

        #rang <- 0.5*(max(adj.results)-min(adj.results))
        #mat.adj <- adj.results/rang
        mat.adj <- t(t(adj.results)-apply(adj.results,2,mean))

        print("step 8: final prediction ...")

        if(distance=="euclidean"){
         hcc <- hclust(parallelDist::parDist(t(mat.adj),threads =n.cores, method = distance), method = "ward.D")
         }else {
         hcc <- hclust(as.dist(1-cor(mat.adj, method = distance)), method = "ward.D")
         }

         hc.umap <- cutree(hcc,2)
         names(hc.umap) <- colnames(results.com)

        saveRDS(hcc, file = paste(sample.name,"clustering_results.rds",sep=""))

        cl.ID <- NULL
        for(i in 1:max(hc.umap)){
        cli <- names(hc.umap)[which(hc.umap==i)]
        pid <- length(intersect(cli, preN))/length(cli)
        cl.ID <- c(cl.ID, pid)
        i<- i+1
         }

        com.preN <- names(hc.umap)
        com.preN[which(hc.umap == which(cl.ID==max(cl.ID)))] <- "diploid"
        com.preN[which(hc.umap == which(cl.ID==min(cl.ID)))] <- "aneuploid"
        names(com.preN) <- names(hc.umap)

        if(WNS=="unclassified.prediction"){
        com.preN[which(com.preN == "diploid")] <- "c1:diploid:low.conf"
        com.preN[which(com.preN == "aneuploid")] <- "c2:aneuploid:low.conf"
        }

      print("step 9: saving results...")

  ##add back filtered cells as not defined in prediction results
  '%!in%' <- function(x,y)!('%in%'(x,y))

  ndef <- colnames(rawmat)[which(colnames(rawmat) %!in% names(com.preN))]
  if(length(ndef)>0){
    res <- data.frame(cbind(c(names(com.preN),ndef), c(com.preN, rep("not.defined",length(ndef)))))
    colnames(res) <- c("cell.names", "copykat.pred")
  } else {
    res <- data.frame(cbind(names(com.preN), com.preN))
    colnames(res) <- c("cell.names", "copykat.pred")
  }
  ##end
  saveRDS(res, paste(sample.name, "prediction.rds",sep=""))

  ####save copycat CNA
  saveRDS(cbind(Aj$RNA.adj[, 1:3], mat.adj), paste(sample.name, "CNA_results.rds", sep=""))

  ####%%%%%%%%%%%%%%%%%next heatmaps, subpopulations and tSNE overlay
  print("step 10: ploting heatmap ...")
  my_palette <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 3, name = "RdBu")))(n = 999)

  chr <- as.numeric(Aj$DNA.adj$chrom) %% 2+1
  rbPal1 <- colorRampPalette(c('black','grey'))
  CHR <- rbPal1(2)[as.numeric(chr)]
  chr1 <- cbind(CHR,CHR)

  rbPal5 <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, name = "Dark2")[2:1])
  compreN_pred <- rbPal5(2)[as.numeric(factor(com.preN))]

  cells <- rbind(compreN_pred,compreN_pred)

  if (ncol(mat.adj)< 3000){
    h <- 10
  } else {
    h <- 15
  }

  col_breaks = c(seq(-1,-0.4,length=50),seq(-0.4,-0.2,length=150),seq(-0.2,0.2,length=600),seq(0.2,0.4,length=150),seq(0.4, 1,length=50))

  if(distance=="euclidean"){
  jpeg(paste(sample.name,"heatmap.jpeg",sep=""), height=h*250, width=4000, res=100)
   heatmap.3(t(mat.adj),dendrogram="r", distfun = function(x) parallelDist::parDist(x,threads =n.cores, method = distance), hclustfun = function(x) hclust(x, method="ward.D"),
            ColSideColors=chr1,RowSideColors=cells,Colv=NA, Rowv=TRUE,
            notecol="black",col=my_palette,breaks=col_breaks, key=TRUE,
            keysize=1, density.info="none", trace="none",
            cexRow=0.1,cexCol=0.1,cex.main=1,cex.lab=0.1,
            symm=F,symkey=F,symbreaks=T,cex=1, main=paste(WNS1,"; ",WNS, sep=""), cex.main=4, margins=c(10,10))

  legend("topright", paste("pred.",names(table(com.preN)),sep=""), pch=15,col=RColorBrewer::brewer.pal(n = 8, name = "Dark2")[2:1], cex=1)
  dev.off()

  ### add a step to plot out gene by cell matrix
  if(plot.genes=="TRUE"){
    dim(results.com)
    rownames(results.com) <- anno.mat2$hgnc_symbol
    chrg <- as.numeric(anno.mat2$chrom) %% 2+1
    rbPal1g <- colorRampPalette(c('black','grey'))
    CHRg <- rbPal1(2)[as.numeric(chrg)]
    chr1g <- cbind(CHRg,CHRg)

    pdf(paste(sample.name,"with_genes_heatmap.pdf",sep=""), height=h*2.5, width=40)
    heatmap.3(t(results.com),dendrogram="r", distfun = function(x) parallelDist::parDist(x,threads =n.cores, method = distance), hclustfun = function(x) hclust(x, method="ward.D"),
              ColSideColors=chr1g,RowSideColors=cells,Colv=NA, Rowv=TRUE,
              notecol="black",col=my_palette,breaks=col_breaks, key=TRUE,
              keysize=1, density.info="none", trace="none",
              cexRow=0.1,cexCol=0.1,cex.main=1,cex.lab=0.1,
              symm=F,symkey=F,symbreaks=T,cex=1, main=paste(WNS1,"; ",WNS, sep=""), cex.main=4, margins=c(10,10))
    dev.off()
  }
  #end of ploting gene by cell matrix



  } else {
    jpeg(paste(sample.name,"heatmap.jpeg",sep=""), height=h*250, width=4000, res=100)
    heatmap.3(t(mat.adj),dendrogram="r", distfun = function(x) as.dist(1-cor(t(x), method = distance)), hclustfun = function(x) hclust(x, method="ward.D"),
                 ColSideColors=chr1,RowSideColors=cells,Colv=NA, Rowv=TRUE,
              notecol="black",col=my_palette,breaks=col_breaks, key=TRUE,
              keysize=1, density.info="none", trace="none",
              cexRow=0.1,cexCol=0.1,cex.main=1,cex.lab=0.1,
              symm=F,symkey=F,symbreaks=T,cex=1, main=paste(WNS1,"; ",WNS, sep=""), cex.main=4, margins=c(10,10))

    legend("topright", paste("pred.",names(table(com.preN)),sep=""), pch=15,col=RColorBrewer::brewer.pal(n = 8, name = "Dark2")[2:1], cex=1)

    dev.off()
    ### add a step to plot out gene by cell matrix
    if(plot.genes=="TRUE"){
      dim(results.com)
      rownames(results.com) <- anno.mat2$hgnc_symbol
      chrg <- as.numeric(anno.mat2$chrom) %% 2+1
      rbPal1g <- colorRampPalette(c('black','grey'))
      CHRg <- rbPal1(2)[as.numeric(chrg)]
      chr1g <- cbind(CHRg,CHRg)

      pdf(paste(sample.name,"with_genes_heatmap.pdf",sep=""), height=h*2.5, width=40)
      heatmap.3(t(results.com),dendrogram="r", distfun = function(x) as.dist(1-cor(t(x), method = distance)), hclustfun = function(x) hclust(x, method="ward.D"),
                ColSideColors=chr1g,RowSideColors=cells, Colv=NA, Rowv=TRUE,
                notecol="black",col=my_palette,breaks=col_breaks, key=TRUE,
                keysize=1, density.info="none", trace="none",
                cexRow=0.1,cexCol=0.1,cex.main=1,cex.lab=0.1,
                symm=F,symkey=F,symbreaks=T,cex=1, main=paste(WNS1,"; ",WNS, sep=""), cex.main=4, margins=c(10,10))
      dev.off()
    }
    #end of ploting gene by cell matrix
  }

 if(output.seg=="TRUE"){
  print("generating seg files for IGV viewer")

  thisRatio <- cbind(Aj$RNA.adj[, 1:3], mat.adj)
  Short <- NULL
  chr <- rle(thisRatio$chrom)[[2]]

  for(c in 4:ncol(thisRatio))
  {
    for (x in 1:length(chr)){
      thisRatio.sub <- thisRatio[which(thisRatio$chrom==chr[x]), ]
      seg.mean.sub <- rle(thisRatio.sub[,c])[[2]]

      rle.length.sub <- rle(thisRatio.sub[,c])[[1]]

      num.mark.sub <- seq(1,length(rle.length.sub),1)
      loc.start.sub <-seq(1,length(rle.length.sub),1)
      loc.end.sub <- seq(1,length(rle.length.sub),1)

      len <-0
      j <-1

      for (j in 1: length(rle.length.sub)){
        num.mark.sub[j] <- rle.length.sub[j]
        loc.start.sub[j] <- thisRatio.sub$chrompos[len+1]
        len <- num.mark.sub[j]+len
        loc.end.sub[j] <- thisRatio.sub$chrompos[len]
        j <- j+1
      }

      ID <- rep(colnames(thisRatio[c]), times=length(rle.length.sub))
      chrom <- rep(chr[x], times=length(rle.length.sub))
      Short.sub <- cbind(ID,chrom,loc.start.sub,loc.end.sub,num.mark.sub,seg.mean.sub)
      Short <- rbind(Short, Short.sub)
      x <- x+1
    }
    c<- c+1
  }

  colnames(Short) <- c("ID","chrom","loc.start","loc.end","num.mark","seg.mean")
  head(Short)
  saveRDS(Short, paste(sample.name, "CNA_results.seg.rds", sep=""))

}
  end_time<- Sys.time()
  print(end_time -start_time)

  reslts <- list(res, cbind(Aj$RNA.adj[, 1:3], mat.adj), hcc)
  names(reslts) <- c("prediction", "CNAmat","hclustering")
  return(reslts)




}





