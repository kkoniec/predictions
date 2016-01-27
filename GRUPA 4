install.packages("FinTS")
install.packages("fGarch")
install.packages("TSA")
install.packages("vars")
install.packages("copula")
library(FinTS)
library(fGarch)
library(TSA)
library(vars)
library(copula)

###KOD TWORZONY PRZEZ PRAWDZIWYCH PROGRAMISTÓW, NIE PRZYWIAZYWALIŚMY SIĘ DO ESTETYKI LUB OPTYMALIZACJI###

readRDS('C:/Users/Bartek/Desktop/Studia/Rok V/Semest Zimowy/Ekonometria Finansowa/Projekt2/wig30components.RDS')->indeksy

colnames(indeksy)->kolumny

matrix(NA,30,4) ->tablicazwr
 
for(i in 1:30)
{
kolumny[i]->tablicazwr[i,1]
}


for (i in 1:30) 
{
indeksy[,c(i)] -> wyniki

wyniki <- wyniki[!is.na(wyniki)]

tail(wyniki,n=15)-> X

tail(wyniki,n=1)->tablicazwr[i,3]

plot(X,type="l",main ="")
title(main = kolumny[i])
cat ("Press [enter] to continue")
line <- readline()

5 -> maxP
5 -> maxQ
matrix(NA, maxP + 1, maxQ + 1) -> AIC
txtProgressBar(min = 0, max = ncol(AIC) * nrow(AIC), style = 3) -> PB
for(p in 0:maxP) {
	for(q in 0:maxQ) {
		tryCatch({
				AIC(arima(X, c(p, 0, q), method="ML")) -> AIC[p + 1, q + 1]
			},
			error = function(e) e,
			warning = function(e) e
		)
		setTxtProgressBar(PB, (maxQ + 1) * p + q)
	}
}
AIC
(c((which.min(AIC) - 1) %% (maxP + 1), (which.min(AIC) - 1) %/% (maxP + 1)) -> params)
close(PB)

arima(X, c(params[1], 0, params[2])) -> armaModel
armaModel

##plot(resid(armaModel), type='p') 
##acf(resid(maModel), lag=25) 
##pacf(resid(maModel), lag=25)
##Box.test(resid(maModel), lag=25, type='Ljung-Box')

length(X)->j
predict(armaModel,n.ahead=1)->predykcja
((predykcja$pred[1]-X[j])/X[j])*100->stopaz

stopaz-> tablicazwr[i,2]

}


tablicazwr[order(tablicazwr[,2],tablicazwr[,1],decreasing=TRUE),]->cowybrac
cowybrac
head(cowybrac,n=7)->ost
sum(as.numeric(ost[,3]))->suma


#TU WPISZ AKUTLANA WARTOSC PORTFELA# 
50000->wartoscportfela
###################################

wartoscportfela/suma->liczbazak
floor(liczbazak)->ilesztukkupic
ilesztukkupic

for(i in 1:7){

cowybrac[i,4]<-as.numeric(ilesztukkupic)

}

for(i in 8:30){

cowybrac[i,4]<-0

}

###TO KUPUJEMY!####
head(cowybrac,7)->ostatecznekupnobyzarabiachajsy
ostatecznekupnobyzarabiachajsy

cowybrac[order(cowybrac[,1],decreasing=FALSE),]->zakupy
zakupy

matrix(NA,7,3) ->rdsik
rdsik

for (i in 1:7)
{
rdsik[i,1]<-ostatecznekupnobyzarabiachajsy[i,1]
rdsik[i,2]<-ostatecznekupnobyzarabiachajsy[i,4]
rdsik[i,3]<-as.numeric(ostatecznekupnobyzarabiachajsy[i,3])
}
rdsik


saveRDS(rdsik, file='C:/Users/Bartek/Desktop/Studia/Rok V/Semest Zimowy/Ekonometria Finansowa/Projekt2/zlecenia.RDS')
readRDS('C:/Users/Bartek/Desktop/Studia/Rok V/Semest Zimowy/Ekonometria Finansowa/Projekt2/zlecenia.RDS')->kontrola
kontrola

#################################################
