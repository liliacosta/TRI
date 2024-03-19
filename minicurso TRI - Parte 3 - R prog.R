######## Minicurso TRI
######## Mar/2024

######## Pacote e dados
  library(ltm)
  library(psych)
  library(nFactors)
  
  data(Environment)

######## Analise descritiva
  descript(Environment)
  # Podemos observar que para todos os seis itens o primeiro nivel de resposta apresenta a maior frequencia, seguido pelo segundo e terceiro niveis.
  # As pessoas se preocupam menos com "Nuclear" e "LeadPetrol", comparando com os demais itens
  
  descript(Environment)$pw.ass
  # Os valores de p para as associacoes pareadas indicam associacoes significativas entre todos os itens.

  cronbach.alpha(Environment, na.rm = TRUE)
  # O escore total eh calculado como a soma dos valores dos itens, sendo considerado os valores 1,2 e 3 para as categorias de resposta muito preocupado, ligeiramente preocupado e pouco preocupado, respectivamente.
  # O alfa de Cronbach mede a consistencia interna.

  rcor.test(Environment, method = "kendall")
  # Um metodo alternativo para explorar o grau de associacao entre pares de itens eh o calculo de um coeficiente de correlacao nao parametrico.

######## Unidimensionalidade: Analise Fatorial 
  # Autovalores devem ser extraidos com base na regra de Kaiser-Guttman, i.e. O numero de fatores dado pelo numero de autovalores maiores que um.
  # O ponto onde a inclinacao da curva esta claramente nivelada (o cotovelo) indica o numero de fatores.
  (evalues<-eigen(rcor.test(Environment, method = "kendall")$cor.mat)$values)
  plotnScree(nScree(evalues, model="factors"), main="Scree Plot & Parallel Analysis")

########  constrained Graded Response Model 
  #  a versao restrita do GRM assume parametros de discriminacao iguais entre itens (modelo RASCH) 
  fit1 <- grm(Environment, constrained = TRUE)
  fit1
  
######## unconstrained Graded Response Model #
  fit2 <- grm(Environment)
  fit2
  # pelo grau de dificuldade: 6 < 4 < 1 < 3 < 5 < 2, portanto as pessoas entrevistadas se preocupam menos com "nuclear" (6), "AirPollution" (4) e "LEadPetrol" (1)
  # pela discriminacao: 1 < 6 < 2 < 5 < 3 < 4, portanto "AirPollution" (4), "RadioWaste" (3) e "Chemicals" (5) discriminam mais as pessoas que se preocupam das que nao se preocupam com o meio ambiente.
  
# teste de razao de verossimilhanca entre modelos restritos e irrestritos para verificar qual tem o melhor ajuste.
  anova(fit1, fit2) # O TRV indica que o GRM irrestrito eh preferivel para esses dados.

######## Independencia Local: comparacao entre os valores observados e esperados dado o modelo ajustado, usando residuos qui-quadrados. 
  margins(fit2)
  margins(fit2, type = "three")
  # Nao apresenta problema em nenhum par de itens nem entre tres itens.
  
  #( exemplo de dados com problema:
    fit3 <- grm(Science)
    margins(fit3)
    margins(fit3, "three")
  #)

######## Curva Caracteristica do Item - CCI
  plot(fit2, lwd = 2, cex = 1.2, legend = TRUE, cx = "left",
         xlab = "Vari??vel Latente", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
  # CCI para cada categoria
  for (ctg in 1:3) {
      plot(fit2, category = ctg, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5,
       cy = 0.85, xlab = "Variavel Latente", cex.main = 1.5, cex.lab = 1.3,
       cex.axis = 1.1)
  }

######## Curva de Informacao do Item - CII
  plot(fit2, type = "IIC", lwd = 2, cex = 1.2, legend = TRUE, cx = "topleft",
       xlab = "Variavel Latente", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

######## Curva de Informacao do Teste - CIT
  plot(fit2, type = "IIC", items = 0, lwd = 2, cex = 1.2, legend = TRUE, cx = "topleft",
       xlab = "Vari??vel Latente", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
  # Informacao por intervalo
  info1 <- information(fit2, c(-4, 0))
  info2 <- information(fit2, c(0, 4))
  text(-1.9, 8, labels = paste("Information in (-4, 0):",
                               paste(round(100 * info1$PropRange, 1), "%", sep = ""),
                               "\n\nInformation in (0, 4):",
                               paste(round(100 * info2$PropRange, 1), "%", sep = "")), cex = 1.2)
  information(fit2, c(-4, 4))
  information(fit2, c(-4, 4), items = c(2, 4))
  
######## Estimando a Variavel Latente
  ltm::factor.scores(fit2)
  # quem eh muito preocupado com tudo (96 pessoas) tem um theta de -0.726 
  # enquanto que quem nao se preocupa com nada (2 pessoas) tem um theta de 2.679
  
  theta = ltm::factor.scores(fit2)$score.dat$z1
  summary(theta)
  hist(theta, main="Histograma da Variavel Latente",xlab="Variavel Latente",ylab="No. de entrevistado")   
  