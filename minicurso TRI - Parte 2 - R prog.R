######## Minicurso TRI
######## Mar/2024

#################################
#    Pacote e dados
#################################
# Primeiro instalar os pacotes necessario para 
#analise de dados

#install.packages("mirt")
library("mirt") 
#install.packages("ltm") 
library(ltm)
#install.packages('psych')
library(psych) 
#install.packages('nFactors')
library(nFactors)


data <- expand.table(LSAT7)

#Banco de dados do Teste de Admissão da Faculdade de Direito (LSAT7)
#n=1000 pessoas e número de itens=5
###################################
##Análise Descritiva
#################################
descript(data)

#Matriz de correlação tetracorica dos itens - considerando a natureza dicotomica dos dados
tetrachoric(data)$"rho"


######## Unidimensionalidade: Analise Fatorial 
# Autovalores devem ser extraidos com base na regra de Kaiser-Guttman, ou seja,.
#O número de fatores é dado pelo numero de autovalores maiores que um.

(evalues<-eigen(tetrachoric(data)$"rho")$values)
plotnScree(nScree(evalues, model="factors"), main="Scree Plot & Parallel Analysis")



###################################
##M1P - Ajustando o modelo 1 parametro 
#################################
fit1=rasch(data) # Modelo com apenas o parametro b variando.
summary(fit1)
coef(fit1)

#Grau de dificuldade dos itens: 1<5<4<2<3


##########################################
##M2P - Ajustando o modelo de 2 parametros
##########################################
fit2=ltm(data~ z1) # Modelo com c=0 e os parametros a e b variam nos itens.
summary(fit2)
coef(fit2)

#Grau de dificuldade dos itens: 5<1<3<2<4
#Grau de discriminação dos itens: 5<4<1<2<3



##########################################
##M3P - Ajustando o modelo de 3 parametros
##########################################
fit3 <- tpm(data) # Modelo os parametros a, b e c variam nos itens
summary(fit3)
coef(fit3)

#Grau de dificuldade: 5<1<3<4<2
#Grau de discriminação:4<5<1<3<2


#Teste de razao de verossimilhanca entre modelos M1P e M2P para verificar qual tem o melhor ajuste.
anova(fit1, fit2) 


#Teste de razao de verossimilhanca entre modelos M2P e M3P para verificar qual tem o melhor ajuste.
anova(fit2, fit3) 


######## Independencia Local:comparacao entre os valores observados e esperados dado o modelo ajustado, usando residuos qui-quadrados. 
margins(fit2)
# Nao apresenta problema em nenhum par de itens nem entre dois itens.
margins(fit2, "three")
# Apresenta problema em um par de itens nem entre dois itens.


######## Curva Caracteristica do Item - CCI
plot(fit2, lwd = 2, cex = 1.2, legend = TRUE, cx = "bottomright",
     cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


######## Curva de Informacao do Item - CII
plot(fit2, type = "IIC", lwd = 2, cex = 1.2, legend = TRUE, cx = "topright",
     cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

######## Curva de Informacaoo do Teste - CIT
plot(fit2, type = "IIC", items = 0, lwd = 2, cex = 1.2, legend = TRUE, cx = "topright",
     cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)



#########################
##Estimacao da Habilidade (traço latente)
#########################

######## Estimando a Habilidade (traço latente) por padrão de respostas
ltm::factor.scores(fit2)
#Z1 - habilidade estimada

###Calcular a habilidade para um padrao de resposta
#O individuo acertou dois itens (2 e 3) e errou os demais itens

pdrep=rbind(c(0,1,1,0,0))
ltm::factor.scores(fit2, resp.patterns = pdrep)


