##### получение данных, если нет загруженных #####
links=names=c() # массив ссылок и названий
for (i in 0:1){ # 2 страницы
  link=paste0("http://pif.investfunds.ru/funds/index.phtml?page=funds&type[]=%D0%BE%D1%82%D0%BA%D1%80%D1%8B%D1%82%D1%8B%D0%B9&c_val[1]=8&value=1&npage=",i) # адрес страницы
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

save(all, file=paste0(DIR,"/pifdata/stock/all.RData")) # сохранение в файл
save(allD, file=paste0(DIR,"/pifdata/stock/allD.RData")) # сохранение в файл

##### подключение данных, если были загружены #####
load(paste0(DIR,"/pifdata/stock/all.RData"))
load(paste0(DIR,"/pifdata/stock/allD.RData"))

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


##### вычисление бутстрап модели по трём бенчмаркам #####
# функция вычисления лучших фондов
best.pif=boot.all(return = pif.return, bench = benchmark.pif.return, rf = ru.1y, boot.n = 100, period = 24, l1 = 0.99, l2 = 0.5)
# результат
cat(paste(all[all[,3] %in% names(best.pif),1],round(best.pif,2)),sep = "\n")

# функция вычисления лучших фондов
best.pif=boot.all(return = pif.return, bench = benchmark.pif.return, rf = ru.1y, boot.n = 100, period = 24, l1 = 0.99, l2 = 0.5)
# результат
cat(paste(all[all[,3] %in% names(best.pif),1],round(best.pif,2)),sep = "\n")

# функция вычисления лучших фондов
best.future=boot.all(return = pif.return, bench = benchmark.future.return, rf = ru.1y, boot.n = 100, period = 24, l1 = 0.99, l2 = 0.5)
# результат
cat(paste(all[all[,3] %in% names(best.future),1],round(best.future,2)),sep = "\n")
