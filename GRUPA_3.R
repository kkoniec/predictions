############# wczytanie danych i bibliotek #############

wejscie =  readRDS('C:/Users/Jarek/Desktop/wig30components.RDS')

#install.packages("lmtest")
#install.packages("TSA")
#install.packages("forecast")
# install.packages("fGarch")

library(tseries)
library(astsa)
library(forecast)
library(fGarch)
library(TSA)

# kilka zmiennych pomocnicznych
licz_kolumn = ncol(wejscie)
licz_wierszy_dane = nrow(wejscie)
nazwy_kolumn = colnames(wejscie)
poz_wier_odciecia = licz_wierszy_dane-30


TablicaSterujaca = data.frame(
  row.names=c(
    'cash',
    'acp', 'alr', 'att', 'bhw', 'bzw', 'ccc', 'cdr', 'cps', 'ena', 'eng',
    'eur', 'gtc', 'ing', 'jsw', 'ker', 'kgh', 'lpp', 'lts', 'lwb', 'mbk',
    'opl', 'peo', 'pge', 'pgn', 'pkn', 'pko', 'pkp', 'pzu', 'sns', 'tpe'
  ),
  ARIMA=rep(0, 31),
  GARCH=rep(0, 31),
  ST_ZWROTU=rep(0, 31),
  quantity=c(1, rep(0, 30)),
  value=c(50000, rep(0, 30))
)


############# koniec wczytanie danych i bibliotek #############




############# funkcje pomocnicze  #############

# pozwoliłem sobie zmodyfikować i wykorzystać Twój kawałek kodu dopasowujący 
# p i q poprzez minimalizację kryterium informacyjnego AIC
ARIMA_model_pred = function(
  notowania, # wektor notowań spółki
  diff_times # stopień zróżnicowania
){  
  
  4 -> maxP
  4 -> maxQ
  matrix(NA, maxP + 1, maxQ + 1) -> AIC
  txtProgressBar(min = 0, max = ncol(AIC) * nrow(AIC), style = 3) -> PB
  for(p in 0:maxP) {
    for(q in 0:maxQ) {
      tryCatch({
        AIC(arima(notowania, c(p, diff_times, q), method="ML")) -> AIC[p + 1, q + 1]
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
  
  arima(notowania, c(params[1], diff_times, params[2])) -> ARIMA_SUGGESTION
  #forecast( ARIMA_SUGGESTION, h=1, fan=TRUE)-> ARIMA_PREDYKCJA
  #ARIMA_PREDYKCJA$mean[1] -> PROGNOZA
  
  predict(ARIMA_SUGGESTION, n.ahead=1)$pred -> PROGNOZA
  PROGNOZA[1] -> PROGNOZA
  
  return(PROGNOZA)
}


#  predykcja GARCH
GARCH_model_pred = function(
  notowania # wektor notowań spółki
){
  GARCH_SUGGESTION = garchFit(~garch(1,1), notowania, cond.dist='sstd', trace=FALSE)
  PROGNOZA = predict(GARCH_SUGGESTION, n.ahead=1)[,1]
  
  return(PROGNOZA)
}


############# koniec funkcje pomocnicze ############# 



for (j in 1:licz_kolumn) {
  plot(wejscie[,j])
}
# Po przejrzeniu zarysów krzywych dla notowań poszczególnych spółek
# stwierdzam, że zadaniem trudnym będzie dopasowanie modelu o zadowalających
# własnościach prognostycznych. Podjęta zostaje zatem decyzja o wybraniu krótkiego
# okresu czasowego na podstawie którego odbędzie się budowa modeli i pradykcja.
# Jako, że w ogólności wpływ na wartość przyszłą maleje wraz ze zwiększeniem
# różnicy czasowej, będę działał na danych z okresu ostatnich 30 dni (około).


dane = wejscie[poz_wier_odciecia:licz_wierszy_dane,]

for (k in 1:30) {
  plot(dane[,k] )
}
# Teraz część szeregów zdaje się "dobrze zachowywać"

# Uwzględnienie w modelu dni w których giełda nie funkcjonowała okazało się
# zbyt trudnym problemem. Ze świadomością pewnej starty dla modelu przychylam się
# do rozwiązania opartego o zwykłe pominięcie w wektorze obserwacji NA. (zapewne 
# odpowiedni model SARIMA jest w stanie uwzględnić co najmniej weekendy)

# Chcąc w sposób uzasadniony przenosić pewne wnioski wydedukowane na podstawie
# jednej części szeregu czasowego na inną jego część należy w pierwszej kolejności
# sprawdzić, czy szereg ten jest stacjonarny.

# Celem wybrania spośród spółek WIG30 tych, których szeregi notowań są stacjonarne
# (bądź ich szeregi zróżnicowane są stacjonarne) użyjemy testu KPSS oraz 
# rozszerzonego testu Dickeya-Fullera

for (kolumna in 1:licz_kolumn) {
  
  # tymczasowy wektor z usunietym NA
  wektor_tmp = na.omit(dane[,kolumna]) 
  
  # sprawdzam stacjonarność szeregu - ADF i KPSS (warunek konieczny dla ARIMA)
  p_v_ADF = adf.test(wektor_tmp)$p.value
  p_v_KPSS = kpss.test(wektor_tmp)$p.value
  
  if(p_v_ADF < 0.05 && p_v_KPSS > 0.05){
    print(paste(nazwy_kolumn[kolumna],"ADF -",p_v_ADF,"  KPSS -",p_v_KPSS, sep=" "))
    plot(wektor_tmp, main=nazwy_kolumn[kolumna], type="l")
    
    #uzupelniam kolumny predykcji w tablicy 
    TablicaSterujaca[kolumna+1,1] = ARIMA_model_pred(wektor_tmp,0)
    TablicaSterujaca[kolumna+1,2] = GARCH_model_pred(wektor_tmp)

  } 
  else {
    d_wektor_tmp = diff(wektor_tmp)
    d_p_v_ADF = adf.test(d_wektor_tmp)$p.value
    d_p_v_KPSS = kpss.test(d_wektor_tmp)$p.value
    
    if(d_p_v_ADF < 0.05 && d_p_v_KPSS > 0.05){
      print(paste(nazwy_kolumn[kolumna],"diff ADF - ",d_p_v_ADF,"  diff KPSS -",d_p_v_KPSS, sep=" "))
      plot(d_wektor_tmp, main=paste(nazwy_kolumn[kolumna],"diff ", sep=" "), type="l")
      
      #uzupelniam kolumny predykcji w tablicy 
      TablicaSterujaca[kolumna+1,1] = ARIMA_model_pred(wektor_tmp,1)
      TablicaSterujaca[kolumna+1,2] = GARCH_model_pred(wektor_tmp)
    }
  }
  
} #koniec for



# Jak się okazuje, mamy kilka spółek, których notowania można uznać za 
# stacjonarne (bądź zróżnicowane notowania) na podstawie wyników ADF i KPSS.

# Z pośród wytypowanych w poprzednim kroku kandydatów wybiorę grupę tych, które
# w ostatnim okresie przejawiają trend wzrostu cen akcji i dla nich będę prowadził
# dalszą analizę. Albo właściwie nie, przeprowadzę analizę dla stacjonarnych
# a o ewentualnym dobrym kierunku zmian poinformuje mnie prognozowana stopa zwrotu.


# wyznaczam stopy zwrotu
for (s in 2:31) {
  if(TablicaSterujaca[s,1] != 0){
    BAZA = na.omit(dane[,s-1])[length(na.omit(dane[,s-1]))]
    PROGNOZA = (TablicaSterujaca[s,1] + TablicaSterujaca[s,2]) / 2
    STOPA_ZWROTU = (PROGNOZA - BAZA)/ BAZA
    TablicaSterujaca[s,3] = STOPA_ZWROTU
  }
}

# podejmowane decyzje o kupnie \ sprzedaży  będą podejmowane na podstawie
# wysokości oszacowanych stóp zwrotu z inwestycji
max(TablicaSterujaca[,3])

