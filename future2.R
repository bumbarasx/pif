# чтение данных из файла
future=read.csv(paste0(DIR,"/MX.csv"), stringsAsFactors = F)
# наименование фьючерсов
future.name=unique(future[,2])
# таблица со значениями ГО по фьючерсам
future.data=as.data.frame(matrix(NA,nrow = nrow(future), ncol = length(future.name)))
# название строк
row.names(future.data)=future[,1]
# название столбцов
colnames(future.data)=future.name
# преобразование во временной формат данных
future.data=as.xts(future.data, order.by = as.Date(row.names(future.data), "%d.%m.%Y"))
# создание новых таблиц копированием
future.price=future.price.estim=future.action=future.data

# заполнение данных по каждому фьючерсу
for(name in future.name){
  # временная переменная
  x=future[future[,2]==name,]
  # преобразование формата даты
  date=as.Date(x[,1], "%d.%m.%Y")
  # заполнение значений ГО
  future.data[date,name]=x[,16]
  # заполнение актуальности фьючерса
  future.action[date,name]=1
  # заполнение значений средней взвешенной цены
  future.price[date,name]=x[,3]
  # заполнение значений цены расчета
  future.price.estim[date,name]=x[,4]
  # удаление временных переменных
  rm(x,date)
}

# величина свободных денежных средств
portfel.bank=1000000
# наименование текущего фьючерса в портфеле
portfel.name=""
# история изменения стоимости портфеля
portfel.history=future.data[,1]
# стирание данных
portfel.history[]=NA

for(i in 1:nrow(future.data)){
  # фьючерс в который нужно инвестировать на данной итерации
  buy=colnames(future.action)[which(!is.na(future.action[i,]))]
  # если фьючерс отличается от фьючерса в портфеле
  # и портфель не пустой то происходит освобождение ГО
  if(buy!=portfel.name & portfel.name!=""){
    # ГО возвращается в свободные сердства
    portfel.bank=portfel.bank + portfel.warranty
    # удаление переменных
    rm(portfel.warranty,portfel.count,portfel.price)
  }

  # если новый фьючерс и фьючерс в портфеле не совпадают
  # то нужно инвестировать в данный фьючерс
  # ГО было освобождено на предыдущем шаге
  if(buy!=portfel.name){
    # текущая стоимость фьючерса - это ГО
    price=as.numeric(future.data[i,buy])
    # инвестиции без "плеча"
    # цена количества фьючерсов покрывает весь банк
    n=ceiling(portfel.bank / as.numeric(future.price[i,buy]))
    # свободные денежные средства
    # уменьшаются только на величину ГО
    portfel.bank=portfel.bank - n * price
    # сохранение количества
    portfel.count=n
    # сохранение наименования фьючерса
    portfel.name=buy
    # цена фьючерса для вычисления маржинального требования
    portfel.price=as.numeric(future.price[i,buy])
    # общий размер ГО
    portfel.warranty=n * price
    # удаление переменных
    rm(price,cash,n)
  }
  # если в портфеле есть фьючерс
  if(portfel.count > 0){
    # актуальное значение ГО
    warranty.price = as.numeric(future.data[i,portfel.name])
    if(!is.na(warranty.price)){
      # вычисление актульного ГО покрывающее портфель
      warranty=warranty.price * portfel.count
      # разница между зарезервированным и актуальным ГО
      diff.warranty = portfel.warranty - warranty
      # изменение величины свободных средств
      portfel.bank = portfel.bank + diff.warranty
      # зарезервированное ГО становится актуальным
      portfel.warranty = warranty
    }
    # вычисление маржинального требования
    if(portfel.price==as.numeric(future.price[i,portfel.name])){
      # инвесировани было в данный день
      margin=(as.numeric(future.price.estim[i,portfel.name]) - portfel.price) * portfel.count
    } else {
      # между двумя датами как ращница расчётных цен
      margin=(as.numeric(future.price.estim[i,portfel.name]) - as.numeric(future.price.estim[(i-1),portfel.name])) * portfel.count
    }
    # если свободных средств и ГО не хватает
    # для покрытия маржинального требования
    if((portfel.bank + portfel.warranty) < margin){
      # происходит margin call
      print("Банк опустел")
      break
    }
    # в конце каждого торгового дня свободные средства
    # изменяются на величину маржинального требования
    portfel.bank = portfel.bank + margin
    # удаление переменных
    rm(warranty,margin,diff.warranty,warranty.price)
  }

  portfel.history[i]=portfel.bank + portfel.warranty
}

##### вычисление бенчмарка на основе фьючерса на Индекс МосБиржи #####
# месячная доходность бенчмарка
benchmark.future.return=monthlyReturn(Cl(to.monthly(portfel.history, indexAt = 'lastof')))
