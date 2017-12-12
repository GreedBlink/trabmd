#carregando pacotes necessaios 
require(corrplot)
require(normtest)

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

corrplot(omega)

corrplot(omega, method = "color", cl.pos = "b", type = "lower", addgrid.col = "white",
         addCoef.col = "white", tl.col = "black", tl.cex = 0.7, number.cex = 0.7, cl.cex = 0.7)

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

########## Definindo a proxy de forma correta



city_dataset_cluster<-cbind(city_dataset_cluster,NA)
names(city_dataset_cluster)[dim(city_dataset_cluster)[2]]<-'ind_acidente'



for(i in 1:10){
  city_dataset_cluster[city_dataset_cluster$membros==i,'ind_acidente']<-ind_acidente[i]
}


plot(city_dataset_cluster$pjovem,city_dataset_cluster$pmotos,xlab="Porporção de jovens", ylab="Proporção de motos")
points(kmeans_out$centers,pch=19,col=2)


####Regressao linear multipla


# modelo <-  lm(formula = ind_acidente~pibpercapita+pop1519p+pop2024p+pop2529p+pjovem+pmotos+pmat+dens_vei,data = city_dataset_cluster)
# summary(modelo)

## Os unicos parametros estatisticamente significativos sao a densidade de jovens , a densidade de motos e 
###  a quantidade de jovens matriculados no ensino medio

# redefinindo <- lm(formula = ind_acidente~pjovem+pmotos+pmat,data = city_dataset_cluster)
# summary(redefinindo)
# 
# ts.plot(redefinindo$residuals)
# abline(h=0,col=2)
# 
# ts.plot(cbind(city_dataset_cluster$ind_acidente,redefinindo$fitted.values),col=1:2)

# significancia <- vector(mode="character")
# variaveis <-  
# for(i in 1:names(city_dataset)){
#   modelo = lm(formula = ind_acidente ~ )  
# }


# fitted(redefinindo)
# confint(redefinindo)
# vcov(redefinindo)
# influence(redefinindo)
# 
# # diagnostic plots 
# layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
# plot(redefinindo)

## diagnosticos e medidas de ajustes

# boxplot(redefinindo$residuals)
# normtest::jb.norm.test(x = redefinindo$residuals)
# shapiro.test(redefinindo$residuals)
# plot(redefinindo$residuals)
# abline(h=0,col=2)
# 
# hist(redefinindo$residuals,xlim = c(-10,10))
# lines(redefinindo$residuals,col=2)



### Estavamos criando a variavel proxy de forma errada. Utilizando as medidas os centroids como pesos, 
### agora podemos criar a proxy de forma correta, para cada municipio e tb pra cada ano relacionado


pesos<-kmeans_out$centers/ind_acidente;pesos
aux = as.data.frame(pesos)

proxy_ind_acidente = rep(0,times = nrow(city_dataset_cluster))
dataset_final <- cbind(city_dataset_cluster,proxy_ind_acidente = proxy_ind_acidente)

for( i in 1:nrow(city_dataset_cluster)){
  if(city_dataset_cluster$grupo[i]  == 1){
    dataset_final$proxy_ind_acidente[i] = aux$pmotos[1]*dataset_final$pmotos[i] + aux$pjovem[1]*dataset_final$pjovem[i]
  }else if(city_dataset_cluster$grupo[i]  == 2){
    dataset_final$proxy_ind_acidente[i] = aux$pmotos[2]*dataset_final$pmotos[i] + aux$pjovem[2]*dataset_final$pjovem[i]
  }else if(city_dataset_cluster$grupo[i]  == 3){
    dataset_final$proxy_ind_acidente[i] = aux$pmotos[3]*dataset_final$pmotos[i] + aux$pjovem[3]*dataset_final$pjovem[i]
  }else{
    dataset_final$proxy_ind_acidente[i] = aux$pmotos[4]*dataset_final$pmotos[i] + aux$pjovem[4]*dataset_final$pjovem[i]
  }  
}


modelo <-  lm(formula = proxy_ind_acidente~pibpercapita+pmat+dens_vei,data = dataset_final)
summary(modelo)


#verificando normalidade nos resíduos
ts.plot(modelo$residuals)
shapiro.test(modelo$residuals)
normalizando = (modelo$residuals - mean(modelo$residuals))/sd(modelo$residuals)
qqnorm(normalizando)
abline(c(0,1),col=2)
install.packages("xts")
install.packages("atsa")
install.packages("lmtest")
require(lmtest)
lmtest::bptest(modelo)



# pelos testes, os resíduos desse modelo neguem uma distribuicao normal
# podemos fazer uma transformação dos dados

names(dataset_final)
t_dados <- log(dataset_final)
t_dados2 <- na.omit(t_dados)
t_dados <- log(dataset_final[,c("")])
nrow(t_dados2)



modelo_t <-  lm(formula = proxy_ind_acidente~pibpercapita+pmat+dens_vei,data = t_dados2)
summary(modelo_t)
length(modelo_t$residuals)
#verificando normalidade nos resíduos
plot(modelo_t$residuals,pch=20)

abline(h=0,col=2)
shapiro.test(modelo_t$residuals)
normalizando_t = (modelo_t$residuals - mean(modelo_t$residuals))/sd(modelo_t$residuals)
qqnorm(normalizando_t)
abline(c(0,1),col=2)
lmtest::bptest(modelo_t)


residuo <- modelo_t$residuals
t_dados_residuos = cbind(t_dados2,residuo)



###############

require(MASS)
a = MASS::boxcox(modelo)
#pegando o valor 
lambda <- a$x[which.max(a$y)]
#
##########


