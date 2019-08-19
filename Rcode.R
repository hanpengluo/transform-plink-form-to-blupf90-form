#set path
setwd("D:\\2-test\\20190807SNP\\")
genotype_file_name<-"test_qc.raw"
map_file_name<-"test.map"
out_name<-"genotype"
plink_blupf90<-function(genotype_file_name,map_file_name,out_name){
  if(!require(data.table)) install.packages("data.table")
  ped<-fread(genotype_file_name,header = F)
  map<-fread(map_file_name,header = F)
  #genotype for blupf90
  ped<-ped[-1,-c(1,3:6)]
  ped[is.na(ped)]=5
  ped_blupf90<-matrix(nrow = nrow(ped),ncol = 2)
  names(ped)[1]<-"id"
  n<-max(nchar(ped$id))
  for(i in 1:nrow(ped)){
    id<-as.character(ped[i,1])
    ped_blupf90[i,1]<-sprintf(paste("%",n,"s",sep = ""),id)
    geno<-as.numeric(ped[i,-1])
    ped_blupf90[i,2]<-paste(geno,collapse = "")
  }
  fwrite(as.data.frame(ped_blupf90),paste(out_name,"_blupf90",sep = ""),row.names = F,col.names = F,sep = " ",quote = F)
  #map infomation for blupf90
  map$snp_order<-seq(1:nrow(map))
  fwrite(map[,c(5,1,4)],paste(out_name,"_map_blupf90",sep = ""),row.names = F,col.names = F,sep = " ",quote = F)
}
plink_blupf90(genotype_file_name = genotype_file_name,map_file_name = map_file_name,out_name = out_name)
