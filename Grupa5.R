install.packages("lmtest")
install.packages("TSA")
install.packages("forecast")
install.packages("fGarch")
install.packages("tseries")
library(tseries)
library(lmtest)
library(TSA)
library(forecast)
library(fGarch)

###załadowanie danych, deklaracja potrzebnych wektorów dla zmiennych

readRDS('C:/Users/Michal/Desktop/dane.rds') -> dane

rep(0,60)->predykcje
rep(0,60)->testkpss
rep(0,60)->testadf

nag<-c ('acp','alr','att','bhw','bzw','ccc','cdr','cps','ena','eng','eur','gtc','ing','jsw' ,'ker','kgh','lpp','lts','lwb','mbk','opl','peo','pge','pgn','pkn','pko','pkp','pzu' ,'sns','tpe')


###sprawdzanie które spółki spełniają założenia konieczne modelu ARIMA
###jeśli spółka przejdzie oba testy dostaje odpowiedni znacznik
###i uruchomiony zostaje algorytm modelu arima wyliczajacy prognostyczne
###stopy zwrotu dla danej spółki
###na podstawie tych stóp zwrotu zostanie wybrane kilka najlepszych
###i na ich podstawie będziemy decydować w co zainswestować

for(i in 1:30){
dane[,c(i)]->kolumna
kolumna<-kolumna[!is.na(kolumna)]
###kolumna<-diff(kolumna)
###kolumna<-tail(kolumna,n=10)
length(kolumna) -> T
log(kolumna[-1] / kolumna[-T]) -> kolumna
adf<- adf.test(kolumna)$p.value
kpss<- kpss.test(kolumna)$p.value
testkpss[2*i-1]<-nag[i]
testadf[2*i-1]<-nag[i]
if(adf<0.05 && kpss>0.05){
testkpss[2*i]<-"OK"
testadf[2*i]<-"OK"
###arima(tail(kolumna,n=10), c(4, 0, 2)) -> wynik
auto.arima(kolumna)->wynik
predict(wynik)$pre[1]->pre
pre<-round(pre,10)
predykcje[2*i-1]<-nag[i]
###(pre$pred[1]-tail(kolumna,n=1))/(pre$pred[1])->predykcje[2*i]
pre->predykcje[2*i]
}else{
testkpss[2*i]<-"odrzucone"
testadf[2*i]<-"odrzucone"
predykcje[2*i-1]<-nag[i]
}
}
###Wyświetlenie wyników logarytmicznych stóp zwrotu dla spółek, przy
###spółkach odrzuconych w procesie sprawdzania testami wyświetlona jest
###wartość '0'

predykcje

###Tabele spółek odrzuconych/przyjętych
testkpss
testadf


#######################################################################
