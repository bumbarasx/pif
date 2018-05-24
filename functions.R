portfel.equity=function(prices, action, bank, fee, log = ""){
  # индикатор, нухно ли вести логи
  diag = FALSE
  if(log!="") diag=TRUE
  # значение комиссии на покупку
  fee.buy=fee[1]
  # значение комиссии на продажу
  fee.sell=fee[2]
  # запись текущей даты в логи
  if(diag) write(as.character(Sys.time()),log,,FALSE)
  # портфель, как массив активов
  portfel=c()
  # история изменения стоимости портфеля
  portfel.history=action[,1]
  portfel.history[]=NA


  for(i in 1:nrow(action)){
    # запись даты
    if(diag) write(paste0("------------------------------",index(action)[i],"------------------------------"),log,,TRUE)
    # если был сплит активов
    n.act=action[i,names(portfel)]
    if(length(n.act)>0) n.act=n.act[1,!is.na(n.act)]
    if(length(n.act)>0) n.act=n.act[1,n.act>1]
    if(length(n.act)>0) portfel[colnames(n.act)]=portfel[colnames(n.act)]*n.act
    # если нет цен по всем активам в портфеле
    pr=prices[i,names(portfel)]
    if(length(pr)>0) pr=pr[1,!is.na(pr)]
    if(length(pr)!=length(portfel)){
      portfel.history[i]=equity
      if(diag) write(paste0("По данным активам торги остановлены: ",paste(setdiff(names(portfel),colnames(pr)),collapse = " ")),log,,TRUE)
      next
    }
    # стоимость портфеля
    equity=sum(as.numeric(prices[i,names(portfel)]*portfel))+bank
    # запись диагностической информации
    if(diag) write(paste("Стоимость портфеля:",equity),log,,TRUE)
    if(diag) write(paste("Банк:",bank),log,,TRUE)
    if(diag) write(paste("Портфель:",paste(paste0(names(portfel),":"),portfel,collapse=", ")),log,,TRUE)
    if(diag) write(paste("Количество:",length(portfel)),log,,TRUE)
    # новый список активов для портфеля
    buy=colnames(action)[which(!is.na(action[i,]))]
    # запись стоиости портфеля в историю изменений
    portfel.history[i]=equity
    # если активы в портфеле такие же, в которые нужно инвестировать
    if(length(setdiff(buy,names(portfel)))==0) next

    if(diag) write(paste("Новый портфель:",paste(buy,collapse = " ")),log,,TRUE)
    if(diag) write(paste("Количество:",length(buy)),log,,TRUE)

    # если портфель пустой и покупать ничего не надо
    if(length(portfel)==0 & length(buy)==0) next
    # вес одной части в портфеле
    part=0
    # если покупать ничего не надо
    if(length(buy)==0){
      # будет ребалансировка
      if (length(portfel)!=0) part=1/length(portfel)
    } else {
      # часть каждого актива (равновзвешенный)
      part=1/length(buy)
    }
    # стоимость одной части портфеля
    part.money=equity*part
    if(diag) write(paste("Вес 1 части:",part),log,,TRUE)

    # новые активы, которые нужно купить в портфель: разница между новым списком активов для портфеля и текущим портфелем
    buy.new=setdiff(buy,names(portfel))

    # акивы из портфеля, которые нужно продать: не входят в новой список активов для портфеля
    sell.new=setdiff(names(portfel), buy)

    #### сначала продаём активы из портфеля, чтобы пополнить количество денежных средств ####
    if(length(sell.new)>0) {
      if(diag) write(paste("##### Продажа ",paste(sell.new,collapse = " ")),log,,TRUE)
      for(p in sell.new){
        # текущая цены пифа
        price=as.numeric(prices[i,p])
        # если нет цены - переход к следующему активу
        if(is.na(price)) next
        # вычисление комиссии
        fee=price * fee.sell
        # количество данного актива в портфеле
        n=as.numeric(portfel[p])
        if(diag) write(paste("ПИФ:",p,"Цена:",price,"Кол-во:",n,"Итого:",n*price,"Комиссия:",n*fee),log,,TRUE)
        # изменение цены с учётом комиссии
        price=price - fee
        # продажа и пополнение количества денежных средств
        bank=bank + n*price
      }
      # исключение проданных пифов из портфеля
      portfel=portfel[! names(portfel) %in% sell.new]
    }
    # созранение информации о стоимости портфеля
    portfel.history[i]==sum(as.numeric(prices[i,names(portfel)]*portfel))+bank
    # портфель стал пустым и покупать ничего не нужно
    if(length(portfel)==0 & length(buy.new)==0) next

    #### далее ребалансировка оставшихся пифов в портфеле (не покупка новых) ####

    # цикл по оставшимся пифам в портфеле
    #### ребалансировка на продажу ####
    if(diag) write("##### Допродажа #####",log,,TRUE)
    for(p in names(portfel)){
      # текущая цены актива
      price=as.numeric(prices[i,p])
      # если нет цены - переход к следующему активу
      if(is.na(price)) next
      # количество пифа в портфеле
      n=as.numeric(portfel[p])
      # стоимость доли данного пифа
      part.price=price*n
      # величина части в портфеле
      part.p=part.price/equity
      # если стоимости доли актива больше новой стоимости одной части, то продажа избытка
      if(part.p > part){
        # разница для продажи
        diff.m=part.price - part.money
        # количество, которое будет продано (округление вверх)
        n.new= ceiling(diff.m / price)
        # если ничего не нужно продавать, то выход
        if(n.new==0) next
        # вычисление комиссии
        fee=price*fee.sell
        if(diag) write(paste("ПИФ:",p,"Цена:",price,"Кол-во:",n.new,"Итого:",n.new*price,"Комиссия",n.new*fee),log,,TRUE)
        # изменение цены с учётом комиссии
        price=price-fee
        # вычитание проданного количества пифов
        portfel[p]=portfel[p] - n.new
        # увеличение банка
        bank=bank + n.new*price
        # если был продан весь актив
        portfel=portfel[! portfel == 0]
      }
    }

    #### ребалансировка на покупку ####
    if(diag) write("##### Допокупка #####",log,,TRUE)
    for(p in names(portfel)){
      # текущая цены актива
      price=as.numeric(prices[i,p])
      # если нет цены - переход к следующему активу
      if(is.na(price)) next
      # количество актива в портфеле
      n=as.numeric(portfel[p])
      # стоимость актива в портфеле
      part.price=price*n
      # величина части в портфеле
      part.p=part.price/equity
      # если часть меньше, то нужно докупать
      if(part.p < part){
        # разница для покупки
        diff.m=part.money - part.price
        # вычисление комиссии
        fee=price*fee.buy
        # цена без комиссии
        price.old=price
        # цена с комиссией
        price=price + fee
        # количество, которое можно докупить
        n.new=floor(diff.m / price)
        # если ничего не надо докупать - переход к следующему активу
        if(n.new==0) next
        if(diag) write(paste("ПИФ:",p,"Цена:",price.old,"Кол-во:",n.new,"Итого:",n.new*price.old,"Комиссия:",n.new*fee),log,,TRUE)
        # добавление купленного количества актива в портфель
        portfel[p]=portfel[p] + n.new
        # уменьшение свебодных денежных средств
        bank=bank-n.new*price
      }
    }

    #### покупка новых пифов ####
    if(length(buy.new)>0) {
      if(diag) write(paste("##### Покупка ",paste(buy.new,collapse = " ")),log,,TRUE)
      # если портфель - это оди актив, то стоимость одной части
      # это все доступные денежные средства
      if(part==1) part.money=bank
      for(p in buy.new){
        # текущая цены актива
        price=as.numeric(prices[i,p])
        # если нет цены - переход к следующему активу
        if(is.na(price)) next
        # вычисление комиссии
        fee=price*fee.buy
        # цена без комиссии
        price.old=price
        # цена с комиссией
        price=price+fee
        # количество актива, которое можно купить
        n=floor(part.money / price)
        if(diag) write(paste("Бюджет:",part.money,"ПИФ:",p,"Цена:",price.old,"Кол-во:",n,"Итого:",n*price.old,"Комиссия:",n*fee),log,,TRUE)
        # если ничего не купить, то выход
        if(n==0) next
        # добавление актива в портфель
        portfel=append(portfel,n)
        # уменьшение свободных денежных средств
        bank=bank - n*price
        # добавление названия актива в портфель
        names(portfel)[length(portfel)]=p
      }
    }
    #####
    if(diag) write("##########",log,,TRUE)
    # вычисление стоимости портфеля
    equity=sum(as.numeric(prices[i,names(portfel)]*portfel))+bank
    if(diag) write(paste("Стоимость портфеля:",equity),log,,TRUE)
    if(diag) write(paste("Банк:",bank),log,,TRUE)
    if(diag) write(paste("Портфель:",paste(paste0(names(portfel),":"),portfel,collapse=", ")),log,,TRUE)
    if(diag) write(paste("Количество:",length(portfel)),log,,TRUE)
    # сохранение информации о стоимости портфеля
    portfel.history[i]=equity
  }
  # в качестве результата возвращается
  # таблица данных о временной стоимости портфеля
  return(portfel.history)
}

pif.graph = function(portfel, bench, file=""){
  i=index(portfel)[which(as.numeric(portfel)!=as.numeric(portfel[1]))[1]]
  # пересечение дат с индексом
  dt=intersect(index(portfel),index(bench))
  i=which(as.Date(dt)==i)
  dt=dt[i:length(dt)]
  test=cbind(bench[as.Date(dt)],Return.calculate(portfel[as.Date(dt)]))
  # наименование столбцов
  colnames(test)=c("MOEX","Portfel")
  # построение графика
  if(file!=""){
    png(file,width = 1000,height = 1155, units = "px", pointsize = 20)
    charts.PerformanceSummary(test)
    dev.off()
  } else {
    charts.PerformanceSummary(test)
  }
  # регрессия
  print(summary(lm(test[,2]~test[,1])))
  portfel.return=Return.calculate(portfel[as.Date(dt)])
  portfel.return[1]=0
  return(portfel.return)
}

boot.all = function(return, bench, rf, boot.n = 10, period = 24, l1 = 0.99, l2 = 0.5){
  # библиотека для перемешивания значений массива
  require(tseries)
  # основная таблица t-статистик
  boot=matrix(NA,nrow = ncol(return),ncol = (boot.n + 1))
  # название строк соответствует названия фондов
  row.names(boot)=colnames(return)
  # цикл по каждому фонду
  for(j in 1:ncol(return)){
    # данные по одному фонду
    pif=return[,j]
    # удаление пустых значений
    pif=pif[!is.na(pif)]
    # если достаточное количество данных
    if(length(na.trim(pif))>period){
      # пересечение данных по фонду, бенчмарку и безрисковой ставке по времени
      time=intersect(intersect(index(pif),index(rf)),index(bench))
      # если достаточно общего количества данных
      if(length(time)>period){
        # избыточная доходность фонда
        Er=Return.excess(pif[as.Date(time)],rf[as.Date(time)])
        # избыточная доходность бенчмарка
        Eb=Return.excess(bench[as.Date(time)],rf[as.Date(time)])
        # если количество данных совпадает
        if(length(Er)==length(Eb)){
          # регрессия на реальных данных
          capm=summary(lm(Er ~ Eb))
          # альфа коэффициент
          capm.alfa=capm$coefficients[1,1]
          # t-статистика альфа коэффициента
          capm.t=capm$coefficients[1,3]
          # бета коэффициент
          capm.beta=capm$coefficients[2,1]
          # остатки, ошибки регрессии
          capm.res=capm$residuals
          # сохранение реального значения t-статистики
          boot[j,1]=capm.t
          # восстановление избыточной доходности
          const = capm.beta * Eb
          # бутстрап
          # цикл регрессий по восстановленным данным
          for(k in 1:boot.n){
            # перемешивание остатков
            res=xts(tsbootstrap(ts(capm.res)), order.by = index(capm.res))
            # добавление остатков к восстановленным данных
            Er.b = const + res
            # регрессия на восстановленных данных
            capm.b = summary(lm(Er.b ~ const))
            # сохранение симулированной t-статистики
            boot[j,(1+k)]=capm.b$coefficients[1,3]
          }
        }
      }
    }
  }

  # массив лучших симулированных коэффициентов
  t.best={}
  # цикл по столбцам бутстрапа
  for (l in 2:boot.n){
    # отбор и добавление лучших коэффициентов из столбца
    t.best=c(t.best,boot[which(boot[,l]>=quantile(na.omit(boot[,l]),c(l1)) & boot[,l]>0),l])
  }

  # массив лучших фондов
  pif.best={}
  # цикл по первому столбцу таблицы
  # с коэффициентами на реальных данных
  for(l in 1:nrow(boot)){
    # если есть значение коэффициента у фонда
    if(!is.na(boot[l,1])){
      # если значение коэффицента выше уровня квантили массива лучших коэффициентов
      if(boot[l,1]>quantile(t.best,c(l2))){
        # добавление значения коэффициента фонда в массив
        pif.best=c(pif.best,boot[l,1])
        # добавления названия фонда к коэффциенту
        names(pif.best)[length(pif.best)]=row.names(boot)[l]
      }
    }
  }
  # сортировка массива лучших фондов
  # по убыванию значений коэффициентов
  pif.best=sort(pif.best, decreasing = TRUE)
  # возврат в качестве результата
  # массив лучших фондов
  return(pif.best)
}

boot.history = function(return, bench, rf, boot.n = 10, period = 24, l1 = 0.99, l2 = 0.5){
  # библиотека для перемешивания значений массива
  require(tseries)
  # основная таблица с результатами бутстрапа
  # структура копируется с таблицы доходностей активов
  boot.res=return
  # очистка данных
  boot.res[]=NA
  # цикл с движущимся окном
  for(i in 1:(nrow(return)-(period-1))){
    # данные по доходности активов за период
    dta.m=return[i:(i+(period-1)),]
    # таблица t-статистик
    boot=matrix(NA,nrow = ncol(dta.m),ncol = (boot.n + 1))
    # название строк соответствует названию активов
    row.names(boot)=colnames(dta.m)
    # цикл по каждому активу
    for(j in 1:ncol(dta.m)){
      # данные по активу за период
      pif=dta.m[,j]
      # если достаточное кодичество данных
      if(length(na.trim(pif))==period){
        # пересечение данных по фонду, бенчмарку и безрисковой ставке по времени
        time=intersect(intersect(index(pif),index(rf)),index(bench))
        # если достаточно общего количества данных
        if(length(time)==period){
          # избыточная доходность фонда
          Er=Return.excess(pif[as.Date(time)],rf[as.Date(time)])
          # избыточная доходность бенчмарка
          Eb=Return.excess(bench[as.Date(time)],rf[as.Date(time)])
          # если количество данных совпадает
          if(length(Er)==length(Eb)){
            # регрессия на реальных данных
            capm=summary(lm(Er ~ Eb))
            # альфа коэффициент
            capm.alfa=capm$coefficients[1,1]
            # t-статистика альфа коэффициента
            capm.t=capm$coefficients[1,3]
            # бета коэффициент
            capm.beta=capm$coefficients[2,1]
            # остатки, ошибки регрессии
            capm.res=capm$residuals
            # сохранение реального значения t-статистики
            boot[j,1]=capm.t
            # восстановление избыточной доходности
            const = capm.beta * Eb
            # бутстрап
            # цикл регрессий по восстановленным данным
            for(k in 1:boot.n){
              # перемешивание остатков
              res=xts(tsbootstrap(ts(capm.res)), order.by = index(capm.res))
              # добавление остатков к восстановленным данных
              Er.b = const + res
              # регрессия на восстановленных данных
              capm.b = summary(lm(Er.b ~ const))
              # сохранение симулированной t-статистики
              boot[j,(1+k)]=capm.b$coefficients[1,3]
            }
          }
        }
      }
    }

    # массив лучших симулированных коэффициентов
    t.best={}
    # цикл по столбцам бутстрапа
    for (l in 2:boot.n){
      # отбор и добавление лучших коэффициентов из столбца
      t.best=c(t.best,boot[which(boot[,l]>=quantile(na.omit(boot[,l]),c(l1)) & boot[,l]>0),l])
    }

    # массив лучших фондов
    pif.best={}
    # цикл по первому столбцу таблицы
    # с коэффициентами на реальных данных
    for(l in 1:nrow(boot)){
      # если есть значение коэффициента у фонда
      if(!is.na(boot[l,1])){
        # если значение коэффицента выше уровня квантили массива лучших коэффициентов
        if(boot[l,1]>quantile(t.best,c(l2))){
          # добавление значения коэффициента фонда в массив
          pif.best=c(pif.best,boot[l,1])
          # добавления названия фонда к коэффциенту
          names(pif.best)[length(pif.best)]=row.names(boot)[l]
        }
      }
    }
    # сортировка массива лучших фондов
    # по убыванию значений коэффициентов
    pif.best=sort(pif.best, decreasing = TRUE)
    # лучшие активы за данный период отмечаются в общей таблице
    boot.res[tail(index(dta.m),n=1),names(pif.best)]=c(1:length(pif.best))
  }
  # массив сумм по стоблцам
  sums=colSums(boot.res,na.rm = TRUE)
  # удаляюся те фонды
  # которые ни разу не были отмечены
  sums=sums[sums>0]
  # удаление данных фондов из таблицы
  boot.res=boot.res[,names(sums)]
  # возвращение таблицы в качестве результата функции
  return(boot.res)
}

boot.act=function(boot.res, period = 4){
  # создание копии таблицы
  copy=boot.res
  # стирание данных
  copy[]=NA
  # индикатор для начала
  # вычисления фонда в который
  # нужно инвестировать
  j=period - 1
  for(i in 1:(nrow(boot.res)-(period-1))){
    # выделение данных за период в переменную
    dta.m=boot.res[i:(i+(period-1)),]
    # проверка, достаточно ли данных в периоде
    if(length(sort(colSums(dta.m)))>0){
      # увеличение периода на 1 месяц
      j=j+1
      # если количество месяцев кратно периоду
      # то нужно вычислять новый фонд для инвестиций
      # на следующий период
      if(j%%period==0){
        # вычисляется сумма по каждому фонду
        # затем данные записываются в массив
        best=names(sort(colSums(dta.m)))
        n=as.numeric(floor(quantile(c(0:length(best)),.25)))
        # вычисление месяца начала инвестиций
        #k=i+period
        k=ifelse((i+period)>=nrow(copy),nrow(copy),(i+period))
        # вычисление конечного месяца инвестиции
        k1=ifelse((k+(period-1))>=nrow(copy),nrow(copy),(k+(period-1)))
        if(k==k1) next
        # запись в таблицу информации о том,
        # в какой фонд будут сделаны инвестции
        copy[index(copy[k:k1,]),best[1]]=1
      }
    }
  }
  # массив сумм по стоблцам
  sums=colSums(copy,na.rm = TRUE)
  # удаляюся те фонды
  # которые ни разу не были отмечены
  sums=sums[sums>0]
  # удаление данных фондов из таблицы
  copy=copy[,names(sums)]
  # возвращение таблицы в качестве результата функции
  return(copy)
}
