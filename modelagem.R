# Necessario definir uma semente em relação a aleatoriedade do modelo utilizaod
set.seed(6)

city_dataset<-read.csv2('city_dataset.csv')
head(city_dataset)

pibpercapita <-city_dataset$pib/city_dataset$populacao
dens_vei     <-city_dataset$veiculos/city_dataset$rodovia
pop1519p     <-city_dataset$pop1519/city_dataset$populacao
pop2024p     <-city_dataset$pop2024/city_dataset$populacao
pop2529p     <-city_dataset$pop2529/city_dataset$populacao


base<-data.frame(cbind(pibpercapita,pop1519p,pop2024p,pop2529p,city_dataset$pjovem,city_dataset$pop60p,
                       city_dataset$pmotos,city_dataset$pmat,dens_vei))

names(base)[c(5:8)]<-c('pop60p','pjovem','pmotos','pmat')
omega<-cor(base,use = 'complete.obs')

base = cbind(base,cidade =city_dataset$cidade,ano = city_dataset$ano)

base<-data.frame(cbind(pibpercapita,pop1519p,pop2024p,pop2529p,city_dataset$pjovem,city_dataset$pop60p,
                       city_dataset$pmotos,city_dataset$pmat,dens_vei))

names(base)[c(5:8)]<-c('pop60p','pjovem','pmotos','pmat')
omega<-cor(base,use = 'complete.obs')

head(base)


kmeans_out<-kmeans(na.omit(base[,c('pjovem','pmotos')]),centers = 4)
kmeans_out$size


membros <- kmeans_out$cluster
base<-base[rowSums(is.na(base[,c('pjovem','pmotos','pmat')]))==0,]
city_dataset_cluster <- cbind(base,membros)



ind_acidente<-rowSums(kmeans_out$centers)

city_dataset_cluster<-cbind(city_dataset_cluster,NA)
names(city_dataset_cluster)[dim(city_dataset_cluster)[2]]<-'ind_acidente'



for(i in 1:10){
  city_dataset_cluster[city_dataset_cluster$membros==i,'ind_acidente']<-ind_acidente[i]
}


plot(city_dataset_cluster$pjovem,city_dataset_cluster$pmotos,xlab="Porporção de jovens", ylab="Proporção de motos")
points(kmeans_out$centers,pch=19,col=2)