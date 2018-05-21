# получение наименования файлов
files=list.files(paste0(DIR,"/dat"),full.names=TRUE)
# общая таблица с данными
data.equity=data.frame()
for(file in files){
  # чтение одного файла
  eq=read.csv(file,stringsAsFactors=FALSE)
  # добавление данных в одну таблицу
  data.equity=rbind(data.equity,eq)
}
tickers<-unique(data.equity[,1])

# информация о базе расчета Индекс ММВБ 10
mic.10<-read.csv(paste0(DIR,"/10micexdata.csv"),header = FALSE,stringsAsFactors = FALSE)
# изменения наименования сокращенных названий акций
mic.10.replace=read.csv(paste0(DIR,"/10micex.csv"),header = FALSE,stringsAsFactors = FALSE)
# замена наименования сокращенных названий акций в базе расчета
for(i in 1:nrow(mic.10.replace)){
  mic.10[mic.10==mic.10.replace[i,1]]=mic.10.replace[i,2]
}

mic.ticker=c()
mic.ticker=unique(unlist(sapply(c(18:nrow(mic.10)),function(i){mic.ticker=append(mic.ticker,mic.10[i,3:12])})))
mic.ticker=mic.ticker[mic.ticker!=""]


date.all=c() # массив со всеми датами
for(i in 18:nrow(mic.10)){
  # база расчета Индекса ММВБ 10 за определенный период
  tickers=unlist(mic.10[i,3:length(mic.10[i,])])
  # массив календарных дат для базы расчета
  dates=seq.Date(as.Date(mic.10[i,1],"%d.%m.%Y"), as.Date(mic.10[i,2],"%d.%m.%Y"), by = "day")
  date.t=c() # массив дат для текущей базы расчета
  for(t in tickers){
    # данные по одной акции из базы расчёта
    y=data.equity[which(data.equity[,1]==t),]
    # пересечение календарнх дат и торговых дней
    date.inter=intersect(dates,as.Date(y[,2]))
    # добавление пересечения дат в массив
    date.t=append(date.t,date.inter)
  }
  # добавление уникальных дат в общий массив
  date.all=append(date.all,unique(date.t))
}

# структурированная таблица с данными
data.10=xts(matrix(NA, nrow = length(date.all), ncol = length(mic.ticker),dimnames = list(date.all,mic.ticker)),order.by = as.Date(date.all))

for(t in colnames(data.10)){
  # данные по одной акции из базы расчёта
  y=data.equity[which(data.equity[,1]==t),]
  # цены закрытия по данной акции
  y=xts(Cl(y),order.by=as.Date(y[,2]))
  # заполнение данных в таблице
  data.10[index(y),t]=y[index(data.10)]
}

for(i in 18:nrow(mic.10)){
  # база расчета Индекса ММВБ 10 за определенный период
  tickers=unlist(mic.10[i,3:length(mic.10[i,])])
  for(t in tickers){
    if(t=="") next
    # данные по одной акции из базы расчёта
    y=data.equity[which(data.equity[,1]==t),]
    # цены закрытия по данной акции
    y=xts(Cl(y),order.by=as.Date(y[,2]))
    # пересечение дат в таблице и торговых дат для акции
    #date.inter=intersect(index(data.10),index(y))
    # заполнение данных в таблице
    data.10[index(y),t]=y
  }
}

for(t in colnames(data.10)){
  y=data.equity[which(data.equity[,1]==t),]
  # цены закрытия по данной акции
  y=xts(Cl(y),order.by=as.Date(y[,2]))
  # заполнение данных в таблице
  data.10[index(y),t]=y[index(data.10)]
}

for(i in 1:nrow(data.10)){
  r=data.10[i,]
  if(length(r[,!is.na(r)])<10) print(index(r))
}

# структурированная таблица с данными
data.act=xts(matrix(NA, nrow = length(date.all), ncol = length(mic.ticker),dimnames = list(date.all,mic.ticker)),order.by = as.Date(date.all))
for(i in 18:nrow(mic.10)){
  # база расчёта индекса
  tickers=unlist(mic.10[i,3:length(mic.10[i,])])
  # удаление пустых значений
  tickers=tickers[tickers!=""]
  # период действия базы расчёта
  dates=seq.Date(as.Date(mic.10[i,1],"%d.%m.%Y"), as.Date(mic.10[i,2],"%d.%m.%Y"), by = "day")
  # заполнение индикаторов, о вхождении акций в портфель
  data.act[dates,tickers]=TRUE
}
# сплит акций
data.act["2007-07-18","SBER"]=1000
data.act["2007-07-18","SBERP"]=20
# заполнение индикаторов
# чтобы акции не были исклчены из портфеля
data.act[c("2007-07-19","2007-07-20"),c("SBER","SBERP")]=TRUE

for(i in 1:nrow(data.act)){
  r=data.act[i,]
  if(length(r[,!is.na(r)])<10) print(index(r))
}

# вызов функции с параметрами
benchmark.stock=portfel.equity(data.10, data.act, 1000000, c(0.0001, 0.0001))
# месячная доходность бенчмарка
benchmark.stock.return=monthlyReturn(Cl(to.monthly(benchmark.stock, indexAt = 'lastof')))
