#' annotate genes with reference to hg38.
#'
#' @param mat data matrix; genes in rows; cell names in columns.
#' @param ID.type gene id type: Symbol or Ensemble.
#' @param annotation annotation file for all known genes.
#' @return annotations of each genes in rows with chrom and positions.
#' @examples
#' test.anno.mat <- annotateGenes.hg20(mat=matx,annotation=annotation, ID.type="ENSEMBLE_id")
#' @export
annotateGenes <- function(mat,annotation, ID.type="S"){
  print("start annotation ...")

  if(substring(ID.type,1,1) %in% c("E", "e")){
    shar <- intersect(rownames(mat), annotation$ensembl_gene_id)
    mat <- mat[shar,]
    anno <- annotation[which(as.character(annotation$ensembl_gene_id) %in% shar),]
    anno <- anno[!duplicated(anno$hgnc_symbol),]
    anno <- anno[order(match(anno$ensembl_gene_id, rownames(mat))),]
    data <- cbind(anno, mat)

  }else if(substring(ID.type,1,1) %in% c("S", "s")) {

    shar <- intersect(rownames(mat), annotation$hgnc_symbol)
    mat <- mat[shar,]
    anno <- annotation[which(as.character(annotation$hgnc_symbol) %in% shar),]
    anno <- anno[!duplicated(anno$hgnc_symbol),]
    anno <- anno[order(match(anno$hgnc_symbol, rownames(mat))),]
    data <- cbind(anno, mat)
  }
}

