###############KEGG ENRICHMENT#############################

#install.packages(c("DOSE", "clusterProfiler"),repos="http://www.bioconductor.org/packages/devel/bioc")
#source("https://bioconductor.org/biocLite.R")
#biocLite("DOSE")
#biocLite("tidyr")
#biocLite("clusterProfiler")
#biocLite("pathview")
#biocLite("Rgraphviz")
#biocLite("RDAVIDWebService")
#install.packages("rJava")

#setwd('C:/Users/mclos/Desktop')
setwd('C:/Users/mclos/Desktop/Scripts/Python')
library(DOSE)
library(clusterProfiler)
library(RDAVIDWebService)
library(xlsx)

#geneList<- read.table("genelist_Endika.txt") #upload genes
geneList <- read.table('outoupmicros.txt') #Fibromyalgia micros genes

#data(geneList)# Tabla Gene/foldchange
head(geneList)
str(geneList)
geneList <- as.vector(geneList$V1)
is.vector(geneList) #TRUE



eg = bitr(geneList, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db") #convert to Entrez ID if required
head(eg)

#ids <- bitr(x, fromType="SYMBOL", toType=c("UNIPROT", "ENSEMBL"), annoDb="org.Hs.eg.db")
#head(ids)
#geneList2<-read.table("RNA_shCTRL_TAM_vs_shSOX9_GOlist.tsv")
#geneList2<- read.table("C:/R_data/JMM_09/Up_geneList.tsv")
# geneList2<- read.table("UU_BB_All_List.txt")
# geneList2<- read.table("List_RNAseq_GX_Up_filtered.txt")
# geneList2<- read.table("Lista_GX_Down_ordered.txt")
# 
#geneList2<- read.table("Lista_GX_Up_ordered.txt")

#head(geneList2)
#str(geneList2)

#geneList2$entrez <- convertIDs(geneList2$V1, "SYMBOL","ENTREZID",  org.Mm.eg.db)
#########################################################################
##ID conversion####tal como esta no permite obtener tabla de enrichment##
########################################################################
#geneList3<- bitr(geneList2$V1, 
#                     fromType = "SYMBOL", #"ENSEMBL",
#                     toType = "ENTREZID", 
#                     OrgDb = org.Mm.eg.db) 


#head(geneList3)
#str(geneList3)
gene.vector=eg$ENTREZID
names(gene.vector)=eg$ENTREZID
head(gene.vector)
vlist<-gene.vector
head(vlist)
str(vlist)

#listaGenes<- names(geneList2)[abs(geneList2) > 1]
# head(gene)
#listaGenes <- as.vector(geneList3[,2])# or #listaGenes <- as.vector(rownames(datos))
#head(listaGenes)
#genes<-as.character(listaGenes)#Lista de genes sin fold
###################################################################
##############IF required##########################################
###################################################################

gene.vector=geneList2$V2
names(gene.vector)=geneList2$V1
head(gene.vector)

#TRANSFORM DF TO VECTOR##########################
#vlist<-as.vector(t(geneList))
vlist<-gene.vector
head(vlist)
str(vlist)

#listaGenes<- names(geneList2)[abs(geneList2) > 1]
# head(gene)
listaGenes <- as.vector(geneList2[,1])# or #listaGenes <- as.vector(rownames(datos))
head(listaGenes)
genes<-as.character(vlist)#Lista de genes sin fold
###################################################################
###################################################################

#Gene Ontology Classification
ggoBP <- groupGO(gene     = genes,
                 'org.Hs.eg.db',#organism = "human",# organism = "mouse",#'org.Mm.eg.db',
                 ont      = "BP",
                 level    = 3,
                 readable = TRUE)
head(summary(ggoBP))
write.xlsx2(summary(ggoBP), file="Results_Fibro.xlsx", sheetName = 'descriptiveBP', row.names = F)

ggoCC <- groupGO(gene     = genes,
                 'org.Hs.eg.db',#organism = "human",# organism = "mouse", #'org.Mm.eg.db',
                 ont      = "CC",
                 level    = 3,
                 readable = TRUE)
head(summary(ggoCC))
write.xlsx2(summary(ggoCC), file="Results_Fibro.xlsx", sheetName = 'descriptiveCC', row.names = F, append = T)

ggoMF <- groupGO(gene     = genes,
                 'org.Hs.eg.db',#organism = "human",# organism = "mouse",#
                 #'org.Mm.eg.db',
                 ont      = "MF",
                 level    = 3,
                 readable = TRUE)
head(summary(ggoMF))
write.xlsx2(summary(ggoMF), file="Results_Fibro.xlsx", sheetName = 'descriptiveMF', row.names = F, append = T)


###############################################
#CREACION UNIVERSOS CANONICOS
###############################################
library(org.Hs.eg.db)#Human
#library(org.Mm.eg.db)
library(GO.db)
#org.Mm.egGO is an R object that provides mappings between entrez gene identifers and GO identifers
#entrez_object <- org.Mm.egGO
entrez_object <- org.Hs.egGO

# Get the entrez gene identifiers that are mapped to a GO ID
allEntrez <- mappedkeys(entrez_object)
#allEntrez <- as.data.frame(allEntrez)

#GO over-representation test

egoBP <- enrichGO(gene          = genes,
                  universe      = allEntrez,
                  'org.Hs.eg.db',#organism = "human",# organism = "mouse",#
                  #'org.Mm.eg.db',
                  #keytype = 'ENSEMBL',
                  ont           = "BP",
                  pAdjustMethod = "BH",
                  pvalueCutoff  = 0.05,#pvalueCutoff  = 0.01,
                  qvalueCutoff  = 0.05,
                  readable      = TRUE)
head(summary(egoBP))
write.xlsx2(summary(egoBP), file="Results_Fibro.xlsx", sheetName = 'overRep_BP', row.names = F, append = T)

egoCC <- enrichGO(gene          = genes,
                  universe      = allEntrez,
                  'org.Hs.eg.db',#organism = "human",# organism = "mouse",#
                  #'org.Mm.eg.db',
                  ont           = "CC",
                  pAdjustMethod = "BH",
                  pvalueCutoff  = 0.05,#pvalueCutoff  = 0.01,
                  qvalueCutoff  = 0.05,
                  readable      = TRUE)
head(summary(egoCC))
write.xlsx2(summary(egoBP), file="Results_Fibro.xlsx", sheetName = 'overRep_CC', row.names = F, append = T)

egoMF <- enrichGO(gene          = genes,
                  universe      = allEntrez,
                  'org.Hs.eg.db',#organism = "human",# organism = "mouse",#
                  #'org.Mm.eg.db',
                  #keytype = 'SYMBOL',
                  ont           = "MF",
                  pAdjustMethod = "BH",
                  pvalueCutoff  = 0.05,#pvalueCutoff  = 0.01,
                  qvalueCutoff  = 0.05,
                  readable      = TRUE)
head(summary(egoMF))
write.xlsx2(summary(egoBP), file="Results_Fibro.xlsx", sheetName = 'overRep_MF', row.names = F, append = T)

#GO Gene Set Enrichment Analysis
ego2BP <- gseGO(geneList     = vlist,
                OrgDb=org.Hs.eg.db,
                #organism = "mouse",#organism = "human"
                ont          = "BP",
                nPerm        = 1000,
                minGSSize    = 120,
                pvalueCutoff = 0.05,#pvalueCutoff = 0.01,
                verbose      = FALSE)
write.csv( summary(ego2BP), file="gseGOBP_table.csv" )

ego2CC <- gseGO(geneList     = vlist,
                OrgDb=org.Hs.eg.db,
                #organism = "mouse",#organism = "human"
                ont          = "CC",
                nPerm        = 1000,
                minGSSize    = 120,
                pvalueCutoff = 0.05,#pvalueCutoff = 0.01,
                verbose      = TRUE)
write.csv( summary(ego2CC), file="gseGOCC_table.csv" )

ego2MF <- gseGO(geneList     = vlist,
                OrgDb=org.Hs.eg.db,
                #organism = "mouse",#organism = "human"
                ont          = "MF",
                nPerm        = 1000,
                minGSSize    = 120,
                pvalueCutoff = 0.05,#pvalueCutoff = 0.01,
                verbose      = FALSE)
write.csv( summary(ego2MF), file="gseMF_table.csv" )

#KEGG over-representation test
kk <- enrichKEGG(gene         = genes,
                 organism     = "human",
                 pvalueCutoff = 0.05,
                 #readable     = TRUE
                 )
#use_internal_data = TRUE)
head(summary(kk))
write.csv( summary(kk), file="enrichKEGG_table.csv" )

#KEGG Gene Set Enrichment Analysis
kk2 <- gseKEGG(geneList = vlist,
               organism = "human",#organism = "human"
               nPerm        = 1000,
               minGSSize    = 5,
               pvalueCutoff = 0.05,#0.1,
               pAdjustMethod = "BH",
               verbose      = FALSE)
#use_internal_data = TRUE)
head(summary(kk2))
write.csv( summary(kk2), file="gseKEGG_table.csv" )

# #DAVID functional analysis
david <- enrichDAVID(gene = genes,
                      idType = "ENTREZ_GENE_ID",
                      listType = "Gene",
                      annotation = "KEGG_PATHWAY",
                      david.user = "mclos.biodonostia@cicbiogune.es")
write.csv( summary(david), file="David_table.csv" )
#Visualization
#pdf("Enrichment_graphics.pdf")

#Barplot
pdf("Enrichment_barplot.pdf")
#pdf(paste(file.path, dir, "Enrichment_barplot.pdf", sep="/"))
print(barplot(ggoCC, drop=TRUE, showCategory=20,font.size=7))
print(barplot(egoCC, showCategory=20,font.size=7))
print(barplot(ggoBP, drop=TRUE, showCategory=20,font.size=7))
print(barplot(egoBP, showCategory=20,font.size=7))
print(barplot(ggoMF, drop=TRUE, showCategory=20,font.size=7))
print(barplot(egoMF, showCategory=20,font.size=7,horiz = TRUE))
dev.off()

#Dotplot
pdf("Enrichment_dotplot.pdf")
#pdf(paste(file.path, dir, "Enrichment_dotplot.pdf", sep="/"))
print(dotplot(egoBP,font.size=7))
print(dotplot(egoCC,font.size=7))
print(dotplot(egoMF,font.size=7))
dev.off()
#}
#Enrichmap
pdf("Enrichment_Map.pdf")
#pdf(paste(file.path, dir, "Enrichment2_Map.pdf", sep="/"))
enrichMap(ggoBP,vertex.label.cex=1.2,layout=igraph::layout.kamada.kawai)
enrichMap(egoBP,vertex.label.cex=1.2,layout=igraph::layout.kamada.kawai)
enrichMap(ego2BP)
enrichMap(ggoCC,vertex.label.cex=1,vertex.label.font=1,layout=igraph::layout.kamada.kawai)
enrichMap(egoCC,vertex.label.cex=1,vertex.label.font=1,layout=igraph::layout.kamada.kawai)
enrichMap(ego2CC)
enrichMap(ggoMF,vertex.label.cex=1,vertex.label.font=1,layout=igraph::layout.kamada.kawai)
enrichMap(egoMF,vertex.label.cex=1,vertex.label.font=1,layout=igraph::layout.kamada.kawai)
#(ego2MF)
dev.off()

#cnetplot
pdf("Enrichment_Cnet2.pdf")
#pdf(paste(file.path, dir, "Enrichment_Cnet.pdf", sep="/"))

cnetplot(egoBP,vertex.label.cex=1,vertex.label.font=1,categorySize="pvalue", foldChange=vlist)
cnetplot(egoBP,vertex.label.cex=1,vertex.label.font=1,categorySize="geneNum", foldChange=vlist)
cnetplot(egoCC,vertex.label.cex=1,vertex.label.font=1,categorySize="pvalue", foldChange=vlist)
cnetplot(egoCC,vertex.label.cex=1,vertex.label.font=1,categorySize="geneNum", foldChange=vlist)
cnetplot(egoMF,vertex.label.cex=1,vertex.label.font=1,categorySize="pvalue", foldChange=vlist)
cnetplot(egoMF,vertex.label.cex=1,vertex.label.font=1,categorySize="geneNum", foldChange=vlist)
dev.off()

#GSEAplot
#gseaplot(kk2, geneSetID = "hsa04145")

#Networks
pdf("Enrichment_Cnet.pdf")
#pdf(paste(file.path, dir, "Enrichment_Networks.pdf", sep="/"))
plotGOgraph(egoBP)
plotGOgraph(egoCC)
plotGOgraph(egoMF)
dev.off()


#Visualize KEGG pathway
library("pathview")
hsa05200 <- pathview(gene.data  = geneList,
                     pathway.id = "hsa05200",#visualize a particular pathway, that appeared enriched
                     species    = "hsa",
                     limit      = list(gene=max(length(genes)), cpd=1),
                     kegg.native = F, same.layer=T)

dev.off()
