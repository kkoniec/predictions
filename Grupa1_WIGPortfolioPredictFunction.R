install.packages("lmtest")
install.packages("data.table")
install.packages("TSA")
install.packages("forecast")
install.packages("fGarch")
library(data.table)
library(TSA)
library(forecast)
library(fGarch)
library(lmtest)

## Parametr portfolio do którego przypisujemy lokalizacjê i nazwê pliku z portfelem do importu do funkcji
portfolio <- "C:/Users/Grzesiek/Documents/GIT/predictions/portfolio.rds"
## Parametr ExportRdsFileName do którego przypisujemy lokalizacjê i nazwê pliku ze zleceniami do eksportu
ExportRdsFileName <- "C:/Users/Grzesiek/Documents/GIT/predictions/Grupa1_Predykcja.rds"
## Parametr trainDatatrainData do którego przypisujemy lokazlizacjê i nazwê pliku z danymi trenuj¹cymi do importu do funkcji
trainData <- "C:/Users/Grzesiek/Documents/GIT/predictions/wig30components.RDS"

AutoPredict = function(
  trainData,
  portfolio,
  ExportRdsFileName
){  
  
  ## Pobieramy dane trenuj¹ce
  readRDS(file = trainData)-> close
  
  ## Pobieramy dane portfela
  readRDS(file = portfolio)-> InvestingPortfolio
  
  
  ## Tworzymy tabele, do której wprwadzimy predykcje poszczególnych instrumentów
  CompanyReturns <- data.frame(
    row.names=c(
      'acp', 'alr', 'att', 'bhw', 'bzw', 'ccc', 'cdr', 'cps', 'ena', 'eng',
      'eur', 'gtc', 'ing', 'jsw', 'ker', 'kgh', 'lpp', 'lts', 'lwb', 'mbk',
      'opl', 'peo', 'pge', 'pgn', 'pkn', 'pko', 'pkp', 'pzu', 'sns', 'tpe'
    ),
    quantity=c( rep(0, 30))
  )
  
  
  ## Oczyszczamy dane z brakuj¹cych wartoœci ("NA") oraz dziêki temu ograniczamy zakres danych do tego samego zakresu datowego
  ## Dzieki temu, mo¿emy ograniczyæ zjawiska sezonowoœci szeregów oraz pozbywamy siê wartoœci
  ## Nie klasyfikuj¹cych siê do przeprowadzenia analizy - dane takie mog¹ zak³ócaæ szeregi czasowe
  ## Usuneliœmy tak¿e luki czasowe, które nie maj¹ wp³ywu na pozytywne przeprowadzenie analizy.

  ncol(close) -> ColumnCounts
  
  for(i in 1:ColumnCounts) {
    close[!is.na(close[,i]),]->close
  }
  
  ## W tej czêœci dokonujemy predykcji poszczególnych spó³ek
  for(i in 1:ColumnCounts){
    
    ## Pobranie danych oraz stworzenie do nich stóp zwrotu w celu póŸniejszej analizy
    
    close[,i] -> DataSet
    
    
    length(DataSet) -> T
    log(DataSet[-1] / DataSet[-T]) -> ReturnDataSet
    
    
    ## Automatycznie dobierany model trendu do danych oraz predykcja za pomoca funkcji predict
    
    etsModel <- ets( DataSet, model="ZZZ")
    
    predict(etsModel, n.ahead=1)$mean[1] -> EtsPredictValue
    
    ##Logarytmiczna stopa zwrotu po zastosowaniu autodopasowanego modelu trendu
    log(EtsPredictValue/DataSet[T] ) -> estPredictReturnValue
    
    ## ARIMA MODEL - Prediction - u¿ylismy tutaj funkcji auto.arima, która automatycznie dopasowuje
    ## parametry p,d,q w modelu zgodnie z dopasowaniem parametrów do kryteriów informacyjnych AIC i BIC
    ## Co prawda nie u¿ywaliœmy jej na zajêciach. Zosta³a ona tutaj wykorzystana, aby zwiêkszyæ wydajnoœæ skryptu
    ## oraz poprawnoœæ dopasowania parametrów do modelu.
    ## U¿ywamy testu KPSS oraz adf do weryfikacji stacjonarnoœci szeregu (Jednego z g³ównych za³o¿eñ testu arima)
    
    adf.test(ReturnDataSet) -> adfTestResults
    kpss.test(ReturnDataSet) -> kpssTestResults
    
    if ( adfTestResults$p.value <= 0.01 && kpssTestResults$p.value > 0.01){    
      
      auto.arima(ReturnDataSet) -> ArimaModel
      
      
      forecast( ArimaModel, h=1, fan=TRUE)-> ArmiaForecast
      ArmiaForecast$mean[1] -> ArimaForecastValue
      
    }
    
    ##GARCH Model
    
    garchFit(~garch(1,1), ReturnDataSet, cond.dist='sstd', trace=FALSE) -> GarchModel
    predict(GarchModel, n.ahead=1)[,1] -> GarchPredictValue
    
    if(!is.null(ArimaForecastValue)){
      
      (GarchPredictValue + ArimaForecastValue + estPredictReturnValue )/3 -> ReturnValue
    } 
    if  (is.null(ArimaForecastValue)){
      (GarchPredictValue + estPredictReturnValue )/2  -> ReturnValue
    }
    
    CompanyReturns[i,1] <- ReturnValue
    
    NULL -> ReturnValue
    NULL -> ArimaForecastValue
    NULL -> GarchPredictValue
    NULL -> estPredictReturnValue
  }
  
  ## Sprzeda¿ i kupno inwestycji
  
  SummariseTransactions <- data.frame(
    row.names=c(
      'acp', 'alr', 'att', 'bhw', 'bzw', 'ccc', 'cdr', 'cps', 'ena', 'eng',
      'eur', 'gtc', 'ing', 'jsw', 'ker', 'kgh', 'lpp', 'lts', 'lwb', 'mbk',
      'opl', 'peo', 'pge', 'pgn', 'pkn', 'pko', 'pkp', 'pzu', 'sns', 'tpe'
    ),
    quantity=c(rep(0, 30)),
    value=c(rep(0, 30))
  )
  
  nrow(SummariseTransactions) -> SummarisingRows
  nrow(close) -> CloseRows
  
  ## Przypisujemy poszczególnym instrumentom aktualne ceny instrumentów  
  
  for(i in 1:SummarisingRows){
    
    SummariseTransactions[i,2] <- close[CloseRows,i] 
    
  }
  
  ## Sprzeda¿ nierentownych spó³ek - sprzedajemy, je¿eli ich prognozowana stopa zwrotu jest mniejsza lub równa 0

  InvestingCash <- InvestingPortfolio[1,2] 
  CountPlusReturns <- 0 ## liczba dodatnich predykcji - liczba ju¿ zainwestowanych dodatnich predykcji
  
  for (i in 1:SummarisingRows){
    if(CompanyReturns[i,1]<= 0 ){
      
      SummariseTransactions[i,1] <- -InvestingPortfolio[i+1,1]
      InvestingCash <- InvestingCash - SummariseTransactions[i,1]*SummariseTransactions[i,2]
      
    }
    
    if (CompanyReturns[i,1] > 0 && InvestingPortfolio[i+1,1] <= 0 ){
      
      CountPlusReturns <- CountPlusReturns + 1
      
    }
  }
  
  ## Nasz wewnêtrzny wskaŸnik inwestycyjny okreœlaj¹cy pu³ap kwoty inwestycyjnej w dany instrument.
  ## Dzielimy nasz kapita³ inwestycyjny na czeœci, które mo¿emy zainwestowaæ w konkretne spó³ki w stosunku 1:1
  ## NIe potrafiliœmy inaczej tego zrobiæ
  
  InvestingCash / CountPlusReturns -> InvestingAmountIndex
  
  for (i in 1:SummarisingRows){
    
    if (CompanyReturns[i,1] > 0 && InvestingPortfolio[i+1,1] <= 0 ){
      
      SummariseTransactions[i,1] <- floor((InvestingAmountIndex/SummariseTransactions[i,2]))
      
    }
  }
  
  #usuwanie wartoœci 0 z tabeli zaawieraj¹cej transakcje
  SummariseTransactions<-SummariseTransactions[!(SummariseTransactions$quantity==0),]
  saveRDS(SummariseTransactions, file = ExportRdsFileName)
  
  return(SummariseTransactions)
  
}


AutoPredict(trainData,portfolio,ExportRdsFileName)  

  