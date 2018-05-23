##### пакеты, настройки #####
require(RSelenium) # парсит сайт
require(wdman) # браузер
require(quantmod)
require(PerformanceAnalytics)
require(gdata) # скачивание файла
require(tseries)

Sys.setlocale("LC_TIME", "English")
DIR<-dirname(rstudioapi::getSourceEditorContext()$path)
source(paste0(DIR,"/functions.R"))

##### получение данных, если нет загруженных #####
pjs <- phantomjs() # запуск сервера - браузера
pjs$output()
rem <- remoteDriver(remoteServerAddr = "localhost"
                    , port = 4567L
                    , browserName = "phantomjs"

) # описание подключения для Selenium
rem$open() # подключение к браузеру
rem$getStatus()
links=names=c() # массив ссылок и названий
for (i in 0:1){ # 2 страницы
  link=paste0("http://pif.investfunds.ru/funds/index.phtml?page=funds&type[]=%D0%BE%D1%82%D0%BA%D1%80%D1%8B%D1%82%D1%8B%D0%B9&c_val[2]=23&value=2&npage=",i) # адрес страницы
  rem$navigate(link) # открытие страницы
  rem$getCurrentUrl() # текущий адрес
  elem <- rem$findElements(using = 'xpath', "//tr/td[1]/a[@class='link']") # выбор элементов
  names <- append(names,unlist(lapply(elem, function(x){x$getElementText()}))) # запись названий ПИФов
  links <- append(links,unlist(lapply(elem, function(x){x$getElementAttribute("href")}))) # запись ссылок ПИФов
}

all=data.frame( # таблица с описанием и общей информацией ПИФов
  Name=as.character(), # название ПИФа
  Link=as.character(), # ссылка на ПИФ
  Number=as.character(), # внутренний номер
  FeeMan=as.numeric(), # вознаграждение менеджера
  FeeDep=as.numeric(), # вознаграждение депозитария
  FeeOth=as.numeric(), # прочие расходы
  stringsAsFactors=FALSE
)

allD=list() # данные по цене и СЧА для ПИФов
dir=getwd()
for(i in 1:length(names)){ # цикл по каждому ПИФу
  all[i,"Name"]<-names[i] # название ПИФа
  all[i,"Link"]<-links[i] # ссылка на ПИФ
  all[i,"Number"]<-regmatches(links[i], regexpr("[0-9]+", links[i], perl=TRUE)) # внутренний номер ПИФа на сайте
  rem$navigate(links[i]) # открытие страницы ПИФа

  elem<-rem$findElement(using = 'xpath', "//a[@class='popup-link arr-open-title']") # поиск конпки на сайте для отображения информации
  elem$clickElement() # нажатие кнопки
  elem <- rem$findElements(using = 'xpath', "//td[@style='border-top:0px;text-align:center']") # список данных по вознаграждению
  z=unlist(lapply(elem, function(x){x$getElementText()})) # текстовое представление данных по вознагржадению
  all[i,c("FeeMan","FeeDep","FeeOth")]=as.numeric(regmatches(z, regexpr("[0-9.]+", z, perl=TRUE)))/100 # сохранения в числовом виде

  download.file(paste0("http://pif.investfunds.ru/funds/export_to_excel.php?f2[0]=",all[i,"Number"],"&export=2&export_type=xls&start_day&finish_day=08&finish_month=02&finish_year=2018&rnd=5030"),destfile="pif.xls",mode="wb",quiet=TRUE) # скачивание файла с данными по цене и СЧА ПИФа
  x<-read.xls(xls=paste0(dir,"/pif.xls"),perl="C:/Perl64/bin/perl.exe") # чтение файла в переменную - таблицу
  x<-x[which(x[,1]!=""&x[,1]!="00.00.0000"),] # очистка пустых строк и неправильных дат
  x<-x[2:length(x[,1]),1:3] # очистка от названий
  x[,2]=as.numeric(gsub(" ","",as.character(x[,2]))) # преобразование цены из тектсового в числовой формат
  x[,3]=as.numeric(gsub(" ","",as.character(x[,3]))) # преобразование СЧА из тектсового в числовой формат
  x1<-as.xts(x[,2],order.by=as.Date(x[,1], "%d.%m.%Y")) # преобразование цены в xts формат
  x2<-as.xts(x[,3],order.by=as.Date(x[,1], "%d.%m.%Y")) # преобразование СЧА в xts формат
  allD[[i]]=list(x1,x2) # сохранение в лист двух xts фреймов
  print(rem$getCurrentUrl()) # печать текущего адреса страницы
}
names(allD)=all[,"Number"] # название в листах соответствуйте внутреннему номеру ПИФов

save(all, file=paste0(DIR,"/pifdata/index/all.RData")) # сохранение в файл
save(allD, file=paste0(DIR,"/pifdata/index/allD.RData")) # сохранение в файл

##### подключение данных, если были загружены #####
load(paste0(DIR,"/pifdata/index/all.RData"))
load(paste0(DIR,"/pifdata/index/allD.RData"))

##### цены и доходность фондов #####
pif.price=xts()
pif.return=xts()
for(i in 1:length(allD)){
  if(i==1){
    # заполненеи первой колонки таблицы
    pif.price=Cl(to.monthly(allD[[i]][[1]],indexAt = 'lastof'))
    pif.return=monthlyReturn(to.monthly(allD[[i]][[1]],indexAt = 'lastof'))
  } else {
    # добавление следующих колонок в таблицу
    pif.price=cbind(pif.price,Cl(to.monthly(allD[[i]][[1]],indexAt = 'lastof')))
    pif.return=cbind(pif.return,monthlyReturn(to.monthly(allD[[i]][[1]],indexAt = 'lastof')))
  }
}
colnames(pif.price)=names(allD)
colnames(pif.return)=names(allD)

for(i in 1:ncol(pif.return)){
  pr=tail(which(is.na(pif.return[,i])),n=1)
  if(length(pr)>0) pif.return[1:pr,i]=NA
  pp=tail(which(is.na(pif.price[,i])),n=1)
  if(length(pp)>0) pif.price[1:pp,i]=NA
  rm(pr,pp)
}
##### безрисковая ставка, Индекс МосБиржи, Индекс ММВБ 10 #####
# чтение данных из файла
ru.1y=read.csv(paste0(DIR,"/RU1Y.csv"),stringsAsFactors = FALSE)
# преобразование в формат данных xts
ru=xts(as.numeric(ru.1y[(1:(nrow(ru.1y)-2)),2]),order.by = as.Date(ru.1y[(1:(nrow(ru.1y)-2)),1], format = "%b %d, %Y"))
# перевод доходности за год к доходности за месяц
ru.1y=xts((1 + Cl(to.monthly(ru,indexAt = 'lastof'))/100)^(1/12)-1)

# чтение данных из файла
moex=read.csv(paste0(DIR,"/MOEX.csv"),stringsAsFactors = FALSE)
# преобразование в формат данных xts
moex=xts(as.numeric(gsub(",", "", moex[,2])),order.by = as.Date(moex[,1], format = "%b %d, %Y"))
# вычисление доходности за месяц
moex=monthlyReturn(to.monthly(moex, indexAt = 'lastof'))

moex10=read.csv(paste0(DIR,"/MOEX10.csv"),stringsAsFactors = FALSE)
moex10=xts(as.numeric(gsub(",", "", moex10[,2])),order.by = as.Date(moex10[,1], format = "%b %d, %Y"))
moex10=monthlyReturn(to.monthly(moex10, indexAt = 'lastof'))

##### стратегия для индексных фондов #####
# период оценки
period = 24
# таблица оценки
pif.act=pif.return
# очистка значений
pif.act[]=NA
# движущийся период оценки в цикле
for(i in 1:(nrow(pif.return)-(period-1))){
  # выделение данных за период в переменную
  dta.m=pif.return[i:(i+(period-1)),]
  # массив с весами оценок фондов
  diff.best=c()
  for(j in 1:ncol(dta.m)){
    # данные по каждому фонду записаны в столбец
    pif=dta.m[,j]
    diff.best[j]=NA
    # проверка на то, что количество данных по фонду
    # соответсвует длине периода
    if(length(na.trim(pif))==period){
      # вычисление пересечения дат с Индексом МосБиржи
      time=intersect(index(pif),index(moex))
      # вычисление разности между фондом и индексом
      diff.index=abs(Return.excess(pif,moex[as.Date(time)]))
      # вычисление показателя для сравнения с другими фондами
      diff.best[j]=mean(diff.index)*sd(diff.index)
    }
  }
  # проверка, если все данные пустые
  if(length(diff.best[is.na(diff.best)])==length(diff.best)) next
  # заполнение названия фондов к их оценке
  names(diff.best)=colnames(dta.m)
  # остаются только те фонды, по которым сформирована оценка
  diff.best=diff.best[!is.na(diff.best)]
  #pif.best.name=names(diff.best)[which(diff.best==min(diff.best,na.rm = TRUE))]
  #pif.act[tail(index(pif),n=1),pif.best.name]=TRUE
  # запись в общую таблицу оценок фондов
  pif.act[tail(index(pif),n=1),names(sort(diff.best))]=c(1:length(diff.best))
}

# создание копии таблицы
pif.act.copy=pif.act
# период для инвестиций
period=12
# стирание данных
pif.act[]=NA
# индикатор для начала
# вычисления фонда в который
# нужно инвестировать
j=11
for(i in 1:(nrow(pif.act.copy)-(period-1))){
  # выделение данных за период в переменную
  dta.m=pif.act.copy[i:(i+(period-1)),]
  # проверка, достаточно ли данных в периоде
  if(sum(dta.m[1,],na.rm = TRUE)>0){
    # увеличение периода на 1 месяц
    j=j+1
    # если количество месяцев кратно периоду
    # то нужно вычислять новый фонд для инвестиций
    # на следующий период
    if(j%%period==0){
      # вычисляется сумма по каждому фонду
      # затем данные записываются в массив
      best=names(sort(colSums(dta.m)))[1]
      # вычисление месяца начала инвестиций
      k=i+period
      # вычисление конечного месяца инвестиции
      k1=ifelse((k+(period-1))>nrow(pif.act.copy),nrow(pif.act.copy),(k+(period-1)))
      # запись в таблицу информации о том,
      # в какой фонд будут сделаны инвестции
      pif.act[index(pif.act.copy[k:k1,]),best[1]]=1
    }
  }
}

##### вычисление бенчмарка на основе индексных фондов #####
# бенчмарк портфель из индексных паевых фондов
benchmark.pif=portfel.equity(pif.price, pif.act, 1000000, c(0.005, 0.015))
# доходность бенчмарка
benchmark.pif.return=pif.graph(benchmark.pif, moex)
