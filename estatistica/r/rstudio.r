## ANALISE DE DADOS ##

setwd ("C:/Users/mathe/Documentos/Estat�stica")

dados <- read.csv("tabela.csv" ,sep=",")

nacional <- dados[dados$tipo=="N",]

importado <- dados[dados$tipo=="I",]

################################################################################
## MEDIA
################################################################################

medianac <- tapply(nacional$precoatual,nacional$categoria,mean)

## MEDIA DO PRECO ATUAL DOS CARROS NACIONAIS
## Hatch Compacto         Picape        Sedan Grande 
## 45960.21              61492.83       56084.43

mediapotnac <- tapply(nacional$potencia,nacional$categoria,mean)

## MEDIA DA POTENCIA DOS CARROS NACIONAIS
## Hatch Compacto         Picape         Sedan Grande 
## 99.71429               140.16667      130.28571 

mediadesvnac <- tapply(nacional$desvalorizacao,nacional$categoria,mean)

## MEDIA DA DESVALORIZACAO DOS CARROS NACIONAIS
## Hatch Compacto         Picape         Sedan Grande 
## 8268.786               9029.167       9368.857 

mediaimp <- tapply(importado$precoatual,importado$categoria,mean)

## MEDIA DO PRECO ATUAL DOS CARROS IMPORTADOS
## Hatch Compacto         Picape        Sedan Grande 
## 104702.2               93113.5       232719.5 

mediapotimp <- tapply(importado$potencia,importado$categoria,mean)

## MEDIA DA POTENCIA DOS CARROS IMPORTADOS
## Hatch Compacto         Picape         Sedan Grande 
## 149.5000               177.5000       279.0455 

mediadesvimp <- tapply(importado$desvalorizacao,importado$categoria,mean)

## MEDIA DA DESVALORIZACAO DOS CARROS IMPORTADOS
## Hatch Compacto         Picape           Sedan Grande 
## 25298.50               9532             35321.23 

################################################################################
## MEDIANA
################################################################################

mediananac <- tapply(nacional$precoatual,nacional$categoria,median)

## MEDIANA DO PRECO ATUAL DOS CARROS NACIONAIS
## Hatch Compacto         Picape        Sedan Grande 
## 32912.5                61818.5        50562.0 

medianapotnac <- tapply(nacional$potencia,nacional$categoria,median)

## MEDIANA DA POTENCIA DOS CARROS NACIONAIS
## Hatch Compacto         Picape         Sedan Grande 
## 80.5                   141.5          131.0 

medianadesvnac <- tapply(nacional$desvalorizacao,nacional$categoria,median)
## MEDIANA DA DESVALORIZACAO DOS CARROS NACIONAIS
## Hatch Compacto         Picape         Sedan Grande 
## 5284.5                 8552.0         8771.0 

medianaimp <- tapply(importado$precoatual,importado$categoria,median)

## MEDIANA DO PRECO ATUAL DOS CARROS IMPORTADOS
## Hatch Compacto         Picape        Sedan Grande 
## 93320.0                93113.5       163183.0 

medianapotimp <- tapply(importado$potencia,importado$categoria,median)
## MEDIANA DA POTENCIA DOS CARROS IMPORTADOS
## Hatch Compacto         Picape         Sedan Grande 
## 153.0                  177.5          239.5

medianadesvimp <- tapply(importado$desvalorizacao,importado$categoria,median)
## MEDIANA DA DESVALORIZACAO DOS CARROS IMPORTADOS
## Hatch Compacto         Picape        Sedan Grande 
## 28900.5                9532.0        26071.0 

################################################################################
## AMPLITUDE
################################################################################

amplitprecnac <- tapply(nacional$precoatual,nacional$categoria,range)
diff(amplitprecnac$'Hatch Compacto')
diff(amplitprecnac$'Picape')
diff(amplitprecnac$'Sedan Grande')

## AMPLITUDE DOS PRECOS ATUAIS DOS CARROS NACIONAIS
## HATCH COMPACTO: 162980
## PICAPE: 66390
## SEDAN GRANDE: 20008

amplitpotnac <- tapply(nacional$potencia,nacional$categoria,range)
diff(amplitpotnac$'Hatch Compacto')
diff(amplitpotnac$'Picape')
diff(amplitpotnac$'Sedan Grande')

## AMPLITUDE DA POTENCIA DOS CARROS NACIONAIS
## HATCH COMPACTO: 248
## PICAPE: 120
## SEDAN GRANDE: 48

amplitdesvnac <- tapply(nacional$desvalorizacao,nacional$categoria,range)
diff(amplitdesvnac$'Hatch Compacto')
diff(amplitdesvnac$'Picape')
diff(amplitdesvnac$'Sedan Grande')

## AMPLITUDE DA DESVALORIZACAO DOS CARROS NACIONAIS
## HATCH COMPACTO: 19824
## PICAPE: 12913
## SEDAN GRANDE: 7244

amplitprecimp <- tapply(importado$precoatual,importado$categoria,range)
diff(amplitprecimp$'Hatch Compacto')
diff(amplitprecimp$'Picape')
diff(amplitprecimp$'Sedan Grande')

## AMPLITUDE DO PRECO ATUAL DOS CARROS IMPORTADOS
## HATCH COMPACTO: 60169
## PICAPE: 8173
## SEDAN GRANDE: 614536

amplitpotimp <- tapply(importado$potencia,importado$categoria,range)
diff(amplitpotimp$'Hatch Compacto')
diff(amplitpotimp$'Picape')
diff(amplitpotimp$'Sedan Grande')

## AMPLITUDE DA POTENCIA DOS CARROS IMPORTADOS
## HATCH COMPACTO: 48
## PICAPE: 5
## SEDAN GRANDE: 485

amplitdesvimp <- tapply(importado$desvalorizacao,importado$categoria,range)
diff(amplitdesvimp$'Hatch Compacto')
diff(amplitdesvimp$'Picape')
diff(amplitdesvimp$'Sedan Grande')

## AMPLITUDE DA DESVALORIZACAO DOS CARROS IMPORTADOS
## HATCH COMPACTO: 28681
## PICAPE: 116
## SEDAN GRANDE: 139725

################################################################################
## DESVIO PADRAO
################################################################################

sdprecnac <- tapply(nacional$precoatual,nacional$categoria,sd)

## DESVIO PADRAO DO PRECO ATUAL DOS CARROS NACIONAIS
## Hatch Compacto         Picape          Sedan Grande 
## 41492.66               25570.66        8703.43 

sdpotnac <- tapply(nacional$potencia,nacional$categoria,sd)

## DESVIO PADRAO DA POTENCIA DOS CARROS NACIONAIS
## Hatch Compacto         Picape         Sedan Grande 
## 64.35410               46.98262       17.40416 

sddesvnac <- tapply(nacional$desvalorizacao,nacional$categoria,sd)

## DESVIO PADRAO DA DESVALORIZACAO DOS CARROS NACIONAIS
## Hatch Compacto         Picape         Sedan Grande 
## 6557.377               4968.594       2625.431 

sdprecimp <- tapply(importado$precoatual,importado$categoria,sd)

## DESVIO PADRAO DO PRECO ATUAL DOS CARROS IMPORTADOS
## Hatch Compacto         Picape       Sedan Grande 
## 27941.042              5779.184     189192.296 

sdpotimp <- tapply(importado$potencia,importado$categoria,sd)

## DESVIO PADRAO DA POTENCIA DOS CARROS IMPORTADOS
## Hatch Compacto         Picape      Sedan Grande 
## 20.157712              3.535534     146.046137 

sddesvimp <- tapply(importado$desvalorizacao,importado$categoria,sd)

## DESVIO PADRAO DA DESVALORIZACAO DOS CARROS IMPORTADOS
## Hatch Compacto         Picape      Sedan Grande 
## 13723.45531            82.02439    31604.17128

################################################################################
## COEFICIENTE DE VARIACAO
################################################################################

coefvarprecnac <- sdprecnac/medianac

## COEFICIENTE DE VARIACAO DO PRECO ATUAL DOS CARROS NACIONAIS
## Hatch Compacto         Picape         Sedan Grande 
## 0.9027952              0.4158316      0.1551844

coefvarpotnac <- sdpotnac/medianapotnac

## COEFICIENTE DE VARIACAO DA POTENCIA DOS CARROS NACIONAIS
## Hatch Compacto         Picape         Sedan Grande 
## 0.7994298              0.3320327      0.1328562

coefvardesvnac <- sddesvnac/mediadesvnac

## COEFICIENTE DE VARIACAO DA DESVALORIZACAO DOS CARROS NACIONAIS
## Hatch Compacto         Picape         Sedan Grande 
## 0.7930278              0.5502826      0.2802296

coefvarprecimp <- sdprecimp/mediaimp

## COEFICIENTE DE VARIACAO DO PRECO ATUAL DOS CARROS IMPORTADOS
## Hatch Compacto         Picape         Sedan Grande 
## 0.26686191             0.06206601     0.81296264

coefvarpotimp <- sdpotimp/mediapotimp

## COEFICIENTE DE VARIACAO DA POTENCIA DOS CARROS IMPORTADOS
## Hatch Compacto         Picape         Sedan Grande 
## 0.1348342              0.0199185      0.5233776

coefvardesvimp <- sddesvimp/mediadesvimp

## COEFICIENTE DE VARIACAO DA DESVALORIZACAO DOS CARROS IMPORTADOS
## Hatch Compacto         Picape         Sedan Grande 
## 0.54246123             0.00860516     0.89476425

################################################################################
## QUARTIS
################################################################################

tapply(nacional$precoatual,nacional$categoria,quantile,c(0.25,0.50,0.75))

## QUARTIS DO PRECO ATUAL DOS CARROS NACIONAIS
## $`Hatch Compacto`
## 25%      50%      75% 
## 30449.25 32912.50 44768.75 

## $Picape
## 25%      50%      75% 
## 40319.75 61818.50 74934.50 

## $`Sedan Grande`
## 25%     50%     75% 
## 49169.5 50562.0 63827.0 

tapply(nacional$potencia,nacional$categoria,quantile,c(0.25,0.50,0.75))

## QUARTIS DA POTENCIA DOS CARROS NACIONAIS
## $`Hatch Compacto`
## 25%  50%  75% 
## 74.5 80.5 86.0 

## $Picape
## 25%    50%    75% 
## 102.00 141.50 168.25 

## $`Sedan Grande`
## 25%   50%   75% 
## 120.5 131.0 144.0 

tapply(nacional$desvalorizacao,nacional$categoria,quantile,c(0.25,0.50,0.75))

## QUARTIS DA DESVALORIZACAO DOS CARROS NACIONAIS
## $`Hatch Compacto`
## 25%      50%      75% 
## 3966.00  5284.50 10898.75 

## $Picape
## 25%     50%     75% 
## 5329.5  8552.0 11927.5 

## $`Sedan Grande`
## 25%     50%     75% 
## 7559.0  8771.0 10834.5 

tapply(importado$precoatual,importado$categoria,quantile,c(0.25,0.50,0.75))

## QUARTIS DO PRECO ATUAL DOS CARROS IMPORTADOS
## $`Hatch Compacto`
## 25%      50%      75% 
## 89525.0  93320.0 108497.2 

## $Picape
## 25%      50%      75% 
## 91070.25 93113.50 95156.75 

## $`Sedan Grande`
## 25%      50%      75% 
## 78205.0 163183.0 350339.2 

tapply(importado$potencia,importado$categoria,quantile,c(0.25,0.50,0.75))

## QUARTIS DA POTENCIA DOS CARROS IMPORTADOS
## $`Hatch Compacto`
## 25%   50%   75% 
## 143.0 153.0 159.5 

## $Picape
## 25%    50%    75% 
## 176.25 177.50 178.75 

## $`Sedan Grande`
## 25%   50%   75% 
## 176.5 239.5 317.5 

tapply(importado$desvalorizacao,importado$categoria,quantile,c(0.25,0.50,0.75))

## QUATIS DA DESVALORIZACAO DOS CARROS IMPORTADOS
## $`Hatch Compacto`
## 25%     50%     75% 
## 18163.5 28900.5 36035.5 

## $Picape
## 25%  50%  75% 
## 9503 9532 9561 

## $`Sedan Grande`
## 25%      50%      75% 
## 12310.50 26071.00 48212.25

################################################################################
## INTERVALO INTERQUARTIL
################################################################################

tapply(nacional$precoatual,nacional$categoria,IQR)

## INTERVALO INTERQUARTIL DO PRECO ATUAL DOS CARROS NACIONAIS
## Hatch Compacto         Picape         Sedan Grande 
## 14319.50               34614.75       14657.50 

tapply(nacional$potencia,nacional$categoria,IQR)

## INTERVALO INTERQUARTIL DA POTENCIA DOS CARROS NACIONAIS
## Hatch Compacto         Picape         Sedan Grande 
## 11.50                  66.25          23.50 

tapply(nacional$desvalorizacao,nacional$categoria,IQR)

## INTERVALO INTERQUARTIL DA DESVALORIZACAO DOS CARROS NACIONAIS
## Hatch Compacto         Picape         Sedan Grande 
## 6932.75                6598.00        3275.50 

tapply(importado$precoatual,importado$categoria,IQR)

## INTERVALO INTERQUARTIL DO PRECO ATUAL DOS CARROS IMPORTADOS
## Hatch Compacto         Picape      Sedan Grande 
## 18972.25               4086.50      272134.25 

tapply(importado$potencia,importado$categoria,IQR)
## INTERVALO INTERQUARTIL DA POTENCIA DOS CARROS IMPORTADOS
## Hatch Compacto         Picape       Sedan Grande 
## 16.5                   2.5          141.0 

tapply(importado$desvalorizacao,importado$categoria,IQR)

## INTERVALO INTERQUARTIL DA DESVALORIZACAO DOS CARROS IMPORTADOS
## Hatch Compacto         Picape      Sedan Grande 
## 17872.00               58.00       35901.75 

################################################################################
## TABELA DE FREQUENCIA (ABSOLUTA E RELATIVA)
################################################################################

## NAO E NECESSARIO

################################################################################
## BOXPLOT
################################################################################

boxplot(dados$desvalorizacao~dados$categoria+dados$tipo,ylab="Desvalorizacao", xlab="Categoria")
## BOXPLOT DESVALORIZACAO

boxplot(dados$potencia~dados$categoria+dados$tipo,ylab="Potencia", xlab="Categoria")
## BOXPLOT POTENCIA

boxplot(dados$precoatual~dados$categoria+dados$tipo, ylab="Preco Atual", xlab="Categoria")
## BOXPLOT PRECO ATUAL

################################################################################
## PROBABILIDADE
################################################################################

################################################################################
## ANALISE POTENCIA NACIONAL
################################################################################

potnac <- nacional$potencia
mediapotnac <- mean(potnac)
desviopotnac <- sd(potnac)
fteoricoNpotnac <- dnorm(seq(min(potnac),max(potnac),by=1),mean=mediapotnac,sd=desviopotnac)
fteoricoEpotnac <- dexp(seq(min(potnac),max(potnac),by=1),rate=1/mediapotnac)

hist(potnac,freq=F,xlab="Potencia Nacionais",ylab="Frequencia Relativa",main="")
lines(seq(min(potnac),max(potnac),by=1),fteoricoNpotnac,col="red")
lines(seq(min(potnac),max(potnac),by=1),fteoricoEpotnac,col="blue")
legend(x=240,y=0.009,legend=c("Normal","Exponencial"),lty=1,col=c("red","blue"),bty="n")

qqnorm(potnac,xlab="Quantis Teoricos",ylab="Amostra dos Quantis",main="Potencia Nacionais")
qqline(potnac,col="green")

################################################################################
## ANALISE PRECO ATUAL NACIONAL
################################################################################

precnac <- nacional$precoatual
mediaprecnac <- mean(precnac)
desvioprecnac <- sd(precnac)
fteoricoNprecnac <- dnorm(seq(min(precnac),max(precnac),by=10),mean=mediaprecnac,sd=desvioprecnac)
fteoricoEprecnac <- dexp(seq(min(precnac),max(precnac),by=10),rate=1/mediaprecnac) 

hist(precnac,freq=F,xlab="Preco Atual Nacionais",ylab="Frequencia Relativa",main="")
lines(seq(min(precnac),max(precnac),by=1),fteoricoNprecnac,col="red")
lines(seq(min(precnac),max(precnac),by=1),fteoricoEprecnac,col="blue")
legend(x=130000,y=2.0e-05,legend=c("Normal","Exponencial"),lty=1,col=c("red","blue"),bty="n")

qqnorm(precnac,xlab="Quantis Teoricos",ylab="Amostra dos Quantis",main="Preco Atual Nacionais")
qqline(precnac,col="green")

################################################################################
## ANALISE DESVALORIZACAO NACIONAL
################################################################################

desvnac <- nacional$desvalorizacao
mediadesvnac <- mean(desvnac)
desviodesvnac <- sd(desvnac)
fteoricoNdesvnac <- dnorm(seq(min(desvnac),max(desvnac),by=10),mean=mediadesvnac,sd=desviodesvnac)
fteoricoEdesvnac <- dexp(seq(min(desvnac),max(desvnac),by=10),rate=1/mediadesvnac) 

hist(desvnac,freq=F,xlab="Desvalorizacao Nacionais",ylab="Frequencia Relativa",main="")
lines(seq(min(desvnac),max(desvnac),by=10),fteoricoNdesvnac,col="red")
lines(seq(min(desvnac),max(desvnac),by=10),fteoricoEdesvnac,col="blue")
legend(x=15000,y=8e-05,legend=c("Normal","Exponencial"),lty=1,col=c("red","blue"),bty="n")

qqnorm(desvnac,xlab="Quantis Teoricos",ylab="Amostra dos Quantis",main="Desvalorizacao Nacionais")
qqline(desvnac,col="green")

################################################################################
## ANALISE POTENCIA IMPORTADOS
################################################################################

potimp <- importado$potencia
mediapotimp <- mean(potimp)
desviopotimp <- sd(potimp)
fteoricoNpotimp <- dnorm(seq(min(potimp),max(potimp),by=1),mean=mediapotimp,sd=desviopotimp)
fteoricoEpotimp <- dexp(seq(min(potimp),max(potimp),by=1),rate=1/mediapotimp) 

hist(potimp,freq=F,xlab="Potencia Importados",ylab="Frequencia Relativa",main="")
lines(seq(min(potimp),max(potimp),by=1),fteoricoNpotimp,col="red")
lines(seq(min(potimp),max(potimp),by=1),fteoricoEpotimp,col="blue")
legend(x=480,y=0.004,legend=c("Normal","Exponencial"),lty=1,col=c("red","blue"),bty="n")

qqnorm(potimp,xlab="Quantis Teoricos",ylab="Amostra dos Quantis",main="Potencia Importados")
qqline(potimp,col="green")

################################################################################
## ANALISE PRECO ATUAL IMPORTADOS
################################################################################

precimp <- importado$precoatual
mediaprecimp <- mean(precimp)
desvioprecimp <- sd(precimp)
fteoricoNprecimp <- dnorm(seq(min(precimp),max(precimp),by=10),mean=mediaprecimp,sd=desvioprecimp)
fteoricoEprecimp <- dexp(seq(min(precimp),max(precimp),by=10),rate=1/mediaprecimp) 

hist(precimp,freq=F,xlab="Preco Atual Importados",ylab="Frequencia Relativa",main="")
lines(seq(min(precimp),max(precimp),by=10),fteoricoNprecimp,col="red")
lines(seq(min(precimp),max(precimp),by=10),fteoricoEprecimp,col="blue")
legend(x=505000,y=4e-06,legend=c("Normal","Exponencial"),lty=1,col=c("red","blue"),bty="n")

qqnorm(precimp,xlab="Quantis Teoricos",ylab="Amostra dos Quantis",main="Preco Atual Importados")
qqline(precimp,col="green")

################################################################################
## ANALISE DESVALORIZACAO IMPORTADOS
################################################################################

desvimp <- importado$desvalorizacao
mediadesvimp <- mean(desvimp)
desviodesvimp <- sd(desvimp)
fteoricoNdesvimp <- dnorm(seq(min(desvimp),max(desvimp),by=10),mean=mediadesvimp,sd=desviodesvimp)
fteoricoEdesvimp <- dexp(seq(min(desvimp),max(desvimp),by=10),rate=1/mediadesvimp) 

hist(desvimp,freq=F,xlab="Desvalorizacao Importados",ylab="Frequencia Relativa",main="")
lines(seq(min(desvimp),max(desvimp),by=10),fteoricoNdesvimp,col="red")
lines(seq(min(desvimp),max(desvimp),by=10),fteoricoEdesvimp,col="blue")
legend(x=130000,y=1.5e-05,legend=c("Normal","Exponencial"),lty=1,col=c("red","blue"),bty="n")

qqnorm(desvimp,xlab="Quantis Teoricos",ylab="Amostra dos Quantis",main="Desvalorizacao Importados")
qqline(desvimp,col="green")


################################################################################
## CORRELACOES
################################################################################

## DESVALORIZACAO vs PRECO ATUAL - NACIONAL
## H0= NÃO EXISTE CORRELACAO
## H1= EXISTE CORRELACAO

cor.test(nacional$desvalorizacao, nacional$precoatual, method = "pearson")
cor.test(nacional$desvalorizacao, nacional$precoatual, method = "kendall")
cor.test(nacional$desvalorizacao, nacional$precoatual, method = "spearman")

##   Pearson's product-moment correlation

## data:  nacional$desvalorizacao and nacional$precoatual
## t = 5.2665, df = 25, p-value = 1.873e-05
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.4765347 0.8664461
## sample estimates:
##   cor 
## 0.7252189 

## P-VALOR < ALPHA, PORTANTO EXISTE EVIDENCIAS QUE OCORRA CORRELACAO ENTRE A DESVALORIZACAO E O PRECO ATUAL

##GRAFICO DE CORRELACAO
plot(nacional$desvalorizacao, nacional$precoatual, main="CORRELACAO ENTRE DESVALORIZACAO E PRECO ATUAL", xlab="Desvalorizacao", ylab="Preco Atual")

## DESVALORIZACAO E POTENCIA - NACIONAL
## H0= NÃO EXISTE CORRELACAO
## H1= EXISTE CORRELACAO

cor.test(nacional$desvalorizacao, nacional$potencia, method = "pearson")
cor.test(nacional$desvalorizacao, nacional$potencia, method = "kendall") ## VALORES COM EMPATE
cor.test(nacional$desvalorizacao, nacional$potencia, method = "spearman") ## VALORES COM EMPATE

##   Pearson's product-moment correlation

## data:  nacional$desvalorizacao and nacional$potencia
## t = 3.8679, df = 25, p-value = 0.000695
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##   0.3021044 0.8047632
## sample estimates:
##   cor 
## 0.611873 

## P-VALOR < ALPHA, PORTANTO EXISTE EVIDENCIAS QUE OCORRA CORRELACAO ENTRE A DESVALORIZACAO E A POTENCIA

## GRAFICO DE CORRELACAO
plot(nacional$desvalorizacao, nacional$potencia, main="CORRELACAO ENTRE DESVALORIZACAO E POTENCIA", xlab="Desvalorizacao", ylab="Potencia")


## DESVALORIZACAO E (PRECO ATUAL + POTENCIA) - NACIONAL
##H0= NAO EXISTE CORRELACAO
##H1= EXISTE CORRELACAO

cor.test(nacional$desvalorizacao, nacional$precoatual+nacional$potencia, method = "pearson")
cor.test(nacional$desvalorizacao, nacional$precoatual+nacional$potencia, method = "kendall")
cor.test(nacional$desvalorizacao, nacional$precoatual+nacional$potencia, method = "spearman")

##   Pearson's product-moment correlation

## data:  nacional$desvalorizacao and nacional$precoatual + nacional$potencia
## t = 5.2646, df = 25, p-value = 1.882e-05
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##   0.4763266 0.8663790
## sample estimates:
##   cor 
## 0.7250913

## P-VALOR < ALPHA, PORTANDO EXISTE EVIDENCIAS QUE OCORRA CORRELACAO ENTRE DESVALORIZACAO E PRECO ATUAL+POTENCIA

##GRAFICO DE CORRELACAO
plot(nacional$desvalorizacao, nacional$precoatual+nacional$potencia, main="CORRELACAO ENTRE DESVALORIZACAO E PRECO ATUAL vs POTENCIA", xlab="Desvalorizacao", ylab="Preco Atual+Potencia")

## DESVALORIZACAO E PRECO ATUAL - IMPORTADO
## H0= NAO EXISTE CORRELACAO
## H1= EXISTE CORRELACAO

cor.test(importado$desvalorizacao, importado$precoatual, method="pearson")
cor.test(importado$desvalorizacao, importado$precoatual, method="kendall")
cor.test(importado$desvalorizacao, importado$precoatual, method="spearman")

##   Pearson's product-moment correlation

## data:  importado$desvalorizacao and importado$precoatual
## t = 5.8275, df = 27, p-value = 3.337e-06
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##   0.5229211 0.8738438
## sample estimates:
##   cor 
## 0.7463803

## P-VALOR < ALPHA, PORTANTO EXISTE EVIDENCIAS QUE OCORRA CORRELACAO ENTRE DESVALORIZACAO E PRECO ATUAL

##GRAFICO DE CORRELACAO
plot(importado$desvalorizacao, importado$precoatual, main="CORRELACAO ENTRE DESVALORIZACAO E PRECO ATUAL", xlab="Desvalorizacao", ylab="Preco Atual")

## DESVALORIZACAO E POTENCIA - IMPORTADO
##H0= NAO EXISTE CORRELACAO
##H1= EXISTE CORRELACAO

cor.test(importado$desvalorizacao, importado$potencia, method="pearson")
cor.test(importado$desvalorizacao, importado$potencia, method="kendall") ## CONTEM VALORES COM EMPATES
cor.test(importado$desvalorizacao, importado$potencia, method="spearman") ## CONTEM VALORES COM EMPATES

##   Pearson's product-moment correlation

## data:  importado$desvalorizacao and importado$potencia
## t = 5.6665, df = 27, p-value = 5.116e-06
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##   0.5076631 0.8688424
## sample estimates:
##   cor 
## 0.737035 

## P-VALOR < ALPHA, PORTANTO EXISTEM EVIDENCIA QUE OCORRE CORRELACAO ENTRE DESVALORIZACAO E POTENCIA

##GRAFICO DE CORRELACAO
plot(importado$desvalorizacao, importado$potencia, main="CORRELACAO ENTRE DESVALORIZACAO E POTENCIA", xlab="Desvalorizacao", ylab="Potencia")

## DESVALORIZACAO E (PRECO ATUAL + POTENCIA) - IMPORTADO
##H0= NAO EXISTE CORRELACAO
##H1= EXISTE CORRELACAO

cor.test(importado$desvalorizacao, importado$precoatual+importado$potencia, method="pearson")
cor.test(importado$desvalorizacao, importado$precoatual+importado$potencia, method="kendall")
cor.test(importado$desvalorizacao, importado$precoatual+importado$potencia, method="spearman")

## Pearson's product-moment correlation

## data:  importado$desvalorizacao and importado$precoatual + importado$potencia
## t = 5.8277, df = 27, p-value = 3.335e-06
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
## 0.5229443 0.8738513
## sample estimates:
## cor 
## 0.7463945 

## P-VALOR <ALPHA, PORTANTO EXISTEM EVIDENCIAS QUE OCORRE UMA CORRELACAO ENTRE A DESVALORIZACAO vs PRECO ATUAL+POTENCIA

## GRAFICO DE CORRELACAO
plot(importado$desvalorizacao, importado$precoatual+importado$potencia,main ="Correlacao entre Desvalorizacao VS Preco Atual+Potencia", xlab="Desvalorizacao", ylab="Preco Atual + Potencia")




cor.test(dados$precoatual, dados$categoria, method="pearson")

################################################################################
## TESTE DE NORMALIDADE DE SHAPIRO-WILK
################################################################################
##H0= SEGUE DIST. NORMAL
##H1= NAO SEGUE DIST. NORMAL

## TESTE SHAPIRO-WILK PARA "POTENCIA NACIONAIS"
shapiro.test((potnac))

## Shapiro-Wilk normality test

## data:  (potnac)
## W = 0.7533, p-value = 2.354e-05

## NAO SEGUE DISTRIBUICAO NORMAL

## TESTE SHAPIRO-WILK PARA "POTENCIA IMPORTADOS"
shapiro.test(potimp)

##   Shapiro-Wilk normality test

## data:  potimp
## W = 0.8122, p-value = 0.0001395

## NAO SEGUE DISTRIBUICAO NORMAL

## TESTE SHAPIRO-WILK PARA "PRECO ATUAL NACIONAIS"
shapiro.test(precnac)

##  Shapiro-Wilk normality test

## data:  precnac
## W = 0.6746, p-value = 1.75e-06

## NAO SEGUE DISTRIBUICAO NORMAL

## TESTE SHAPIRO-WILK PARA "PRECO ATUAL IMPORTADOS"
shapiro.test(precimp)

##   Shapiro-Wilk normality test

## data:  precimp
## W = 0.8005, p-value = 8.538e-05

## NAO SEGUE DISTRIBUICAO NORMAL

## TESTE SHAPIRO-WILK PARA "DESVALORIZACAO NACIONAIS"
shapiro.test(desvnac)

##   Shapiro-Wilk normality test

## data:  desvnac
## W = 0.9102, p-value = 0.02309

## NAO SEGUE DESTRIBUICAO NORMAL

## TESTE SHAPIRO-WILK PARA "DESVALORIZACAO IMPORTADOS"
shapiro.test(desvimp)

##   Shapiro-Wilk normality test

## data:  desvimp
## W = 0.783, p-value = 4.207e-05

## NAO SEGUE DISTRIBUICAO NORMAL


################################################################################
## INFERENCIA ESTATISTICA
################################################################################

## POSSIVEIS RESULTADOS ESPERADOS ##
## Foi possível concluir que, potencia e preço de compra de um veículo,
## não influênciam em maiores ou menores desvalorizações. Já a categoria,
## modelo, marca e tipo, apresentam resultados importantes na queda de valor
## de um automóvel, desde sua compra zero quilômetro até um período posteriorde três anos. 


################################################################################
## ANOVA
################################################################################

anovai1 <- aov(dados$desvalorizacao~dados$categoria+dados$tipo+dados$categoria*dados$tipo)

summary(anovai1)

## + anovai1 <- aov(dados$desvalorizacao~dados$categoria+dados$tipo+dados$categoria*dados$tipo)
## > summary(anovai1)
## Df    Sum Sq   Mean Sq F value  Pr(>F)   
## dados$categoria             2 3.919e+09 1.959e+09   4.261 0.01956 * 
##   dados$tipo                  1 4.387e+09 4.387e+09   9.541 0.00328 **
##   dados$categoria:dados$tipo  2 3.568e+08 1.784e+08   0.388 0.68047   
## Residuals                  50 2.299e+10 4.598e+08                   
## ---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 

anova <- aov(dados$desvalorizacao~dados$categoria+dados$tipo)

summary(anova)

## > summary(anova)
## Df    Sum Sq   Mean Sq F value Pr(>F)   
## dados$categoria  2 3.919e+09 1.959e+09   4.364 0.0177 * 
##   dados$tipo       1 4.387e+09 4.387e+09   9.771 0.0029 **
##   Residuals       52 2.335e+10 4.490e+08                  
## ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(anova,conf.level = 0.95)

residuos <- anova$residuals


qqnorm(residuos)
qqline(residuos)

shapiro.test(residuos)

##   Shapiro-Wilk normality test

## data:  residuos
## W = 0.7685, p-value = 5.231e-08


## POTENCIA EM RELACAO A CATEGORIA

anovai2 <- aov(dados$potencia~dados$categoria+dados$tipo+dados$categoria*dados$tipo)

summary(anovai2)

## > anovai2 <- aov(dados$potencia~dados$categoria+dados$tipo+dados$categoria*dados$tipo)
## > summary(anovai2)
## Df Sum Sq Mean Sq F value   Pr(>F)    
## dados$categoria             2 204793  102397   9.915 0.000236 ***
##   dados$tipo                  1 103358  103358  10.009 0.002650 ** 
##   dados$categoria:dados$tipo  2  26193   13096   1.268 0.290237    
## Residuals                  50 516348   10327                     
## ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 



anova2 <- aov(dados$potencia~dados$categoria+dados$tipo)

summary(anova2)

## > summary(anova2)
## Df Sum Sq Mean Sq F value   Pr(>F)    
## dados$categoria  2 204793  102397   9.814 0.000242 ***
##   dados$tipo       1 103358  103358   9.906 0.002726 ** 
##   Residuals       52 542540   10433                     
## ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


TukeyHSD(anova2, conf.level = 0.95)

residuos2 <- anova2$residuals

qqnorm(residuos2)
qqline(residuos2)

shapiro.test(residuos2)

## Shapiro-Wilk normality test

## data:  residuos2
## W = 0.8082, p-value = 4.445e-07

## TESTE ANULADO AQUI


## PRECO ATUAL EM RELACAO A CATEGORIA

anovai3 <- aov(dados$precoatual~dados$categoria+dados$tipo+dados$categoria*dados$tipo)
summary(anovai3)

## > anovai3 <- aov(dados$precoatual~dados$categoria+dados$tipo+dados$categoria*dados$tipo)
## > summary(anovai3)
## Df    Sum Sq   Mean Sq F value  Pr(>F)   
## dados$categoria             2 2.203e+11 1.102e+11   7.047 0.00201 **
##   dados$tipo                  1 1.419e+11 1.419e+11   9.074 0.00406 **
##   dados$categoria:dados$tipo  2 3.908e+10 1.954e+10   1.250 0.29532   
## Residuals                  50 7.817e+11 1.563e+10                   
## ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1





anova3 <- aov(dados$precoatual~dados$categoria+dados$tipo)

summary(anova3)

## > summary(anova3)
## Df    Sum Sq   Mean Sq F value  Pr(>F)   
## dados$categoria  2 2.203e+11 1.102e+11   6.980 0.00206 **
##   dados$tipo       1 1.419e+11 1.419e+11   8.988 0.00416 **
##   Residuals       52 8.208e+11 1.578e+10                   
## ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




TukeyHSD(anova3, conf.level = 0.95)

residuos3 <- anova3$residuals

qqnorm(residuos3)
qqline(residuos3)

shapiro.test(residuos3)

##   Shapiro-Wilk normality test

## data:  residuos3
## W = 0.8514, p-value = 6.298e-06
## TESTE ANULADO AQUI


## FAZER CORRELACOES SEPARADOS (quais? estao feitas acima algumas), FAZER NAO PARAMETRICOS (SOMENTE AS CORRELACOES)


## NÃO PARAMETRICOS

## install.packages("agricolae")
library(agricolae)

teste3 <- kruskal(dados$desvalorizacao, dados$categoria)

## PELO TESTE, TEMOS EVIDENCIAS QUE PICAPE E HACTCH COMPACTO SEGUEM A MESMA DISTRIBUICAO EM RELACAO
## A DESVALORIZACAO, JA O SEDAN GRANDE NÃO.

## $groups
## trt    means M
## 1 Sedan Grande   35.31034 a
## 2 Picape         23.11111 b
## 3 Hatch Compacto 20.22222 b

teste4 <- kruskal(nacional$desvalorizacao, nacional$categoria)

## $groups
## trt    means M
## 1 Sedan Grande   17.28571 a
## 2 Picape         14.83333 a
## 3 Hatch Compacto 12.00000 a

teste5 <- kruskal(importado$desvalorizacao, importado$categoria)

## $groups
## trt    means M
## 1 Sedan Grande   15.95455 a
## 2 Hatch Compacto 13.25000 a
## 3 Picape         10.33333 a

teste6 <- wilcox.test(dados$desvalorizacao, tiponum)
## Wilcoxon rank sum test with continuity correction

## data:  dados$desvalorizacao and tiponum
## W = 3136, p-value < 2.2e-16
## alternative hypothesis: true location shift is not equal to 0

teste7 <- kruskal(dados$potencia, dados$categoria)

## $groups
## trt    means M
## 1 Sedan Grande   36.93103 a
## 2 Picape         29.33333 a
## 3 Hatch Compacto 14.50000 b

teste8 <- kruskal(importado$potencia, importado$categoria)

## $groups
## trt    means  M
##1 Sedan Grande   16.97727  a
## 2 Picape         12.83333 ab
## 3 Hatch Compacto  5.75000  b

teste9 <- kruskal(nacional$potencia, nacional$categoria)

## $groups
## trt     means M
## 1 Picape         19.666667 a
## 2 Sedan Grande   19.500000 a
## 3 Hatch Compacto  8.821429 b

teste10 <- wilcox.test(dados$potencia, tiponum)
##   Wilcoxon rank sum test with continuity correction

## data:  dados$potencia and tiponum
## W = 3136, p-value < 2.2e-16
## alternative hypothesis: true location shift is not equal to 0


teste11 <- kruskal(dados$precoatual, dados$categoria)

## $groups
## trt    means  M
## 1 Sedan Grande   36.10345  a
## 2 Picape         27.66667 ab
## 3 Hatch Compacto 16.66667  b


teste12 <- kruskal(importado$precoatual, importado$categoria)

## $groups
## trt    means M
## 1 Sedan Grande   16.00000 a
## 2 Picape         12.33333 a
## 3 Hatch Compacto 11.50000 a

teste13 <- kruskal(nacional$precoatual, nacional$categoria)

## $groups
## trt     means M
## 1 Sedan Grande   19.428571 a
## 2 Picape         18.833333 a
## 3 Hatch Compacto  9.214286 b

tiponum <- as.numeric(dados$tipo)
teste14 <- wilcox.test(dados$precoatual, tiponum)

##   Wilcoxon rank sum test with continuity correction

## data:  dados$precoatual and tiponum
## W = 3136, p-value < 2.2e-16
## alternative hypothesis: true location shift is not equal to 0





teste15 <- wilcox.test(dados$precoatual, tiponum)

## Erro em wilcox.test.default(dados$precoatual, dados$tipo) : 
## 'y' deve ser numérico

teste16 <- 