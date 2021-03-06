portfel.equity=function(prices, action, bank, fee, log = ""){
  # ���������, ����� �� ����� ����
  diag = FALSE
  if(log!="") diag=TRUE
  # �������� �������� �� �������
  fee.buy=fee[1]
  # �������� �������� �� �������
  fee.sell=fee[2]
  # ������ ������� ���� � ����
  if(diag) write(as.character(Sys.time()),log,,FALSE)
  # ��������, ��� ������ �������
  portfel=c()
  # ������� ��������� ��������� ��������
  portfel.history=action[,1]
  portfel.history[]=NA


  for(i in 1:nrow(action)){
    # ������ ����
    if(diag) write(paste0("------------------------------",index(action)[i],"------------------------------"),log,,TRUE)
    # ���� ��� ����� �������
    n.act=action[i,names(portfel)]
    if(length(n.act)>0) n.act=n.act[1,!is.na(n.act)]
    if(length(n.act)>0) n.act=n.act[1,n.act>1]
    if(length(n.act)>0) portfel[colnames(n.act)]=portfel[colnames(n.act)]*n.act
    # ���� ��� ��� �� ���� ������� � ��������
    pr=prices[i,names(portfel)]
    if(length(pr)>0) pr=pr[1,!is.na(pr)]
    if(length(pr)!=length(portfel)){
      portfel.history[i]=equity
      if(diag) write(paste0("�� ������ ������� ����� �����������: ",paste(setdiff(names(portfel),colnames(pr)),collapse = " ")),log,,TRUE)
      next
    }
    # ��������� ��������
    equity=sum(as.numeric(prices[i,names(portfel)]*portfel))+bank
    # ������ ��������������� ����������
    if(diag) write(paste("��������� ��������:",equity),log,,TRUE)
    if(diag) write(paste("����:",bank),log,,TRUE)
    if(diag) write(paste("��������:",paste(paste0(names(portfel),":"),portfel,collapse=", ")),log,,TRUE)
    if(diag) write(paste("����������:",length(portfel)),log,,TRUE)
    # ����� ������ ������� ��� ��������
    buy=colnames(action)[which(!is.na(action[i,]))]
    # ������ �������� �������� � ������� ���������
    portfel.history[i]=equity
    # ���� ������ � �������� ����� ��, � ������� ����� �������������
    if(length(setdiff(buy,names(portfel)))==0) next

    if(diag) write(paste("����� ��������:",paste(buy,collapse = " ")),log,,TRUE)
    if(diag) write(paste("����������:",length(buy)),log,,TRUE)

    # ���� �������� ������ � �������� ������ �� ����
    if(length(portfel)==0 & length(buy)==0) next
    # ��� ����� ����� � ��������
    part=0
    # ���� �������� ������ �� ����
    if(length(buy)==0){
      # ����� ��������������
      if (length(portfel)!=0) part=1/length(portfel)
    } else {
      # ����� ������� ������ (���������������)
      part=1/length(buy)
    }
    # ��������� ����� ����� ��������
    part.money=equity*part
    if(diag) write(paste("��� 1 �����:",part),log,,TRUE)

    # ����� ������, ������� ����� ������ � ��������: ������� ����� ����� ������� ������� ��� �������� � ������� ���������
    buy.new=setdiff(buy,names(portfel))

    # ����� �� ��������, ������� ����� �������: �� ������ � ����� ������ ������� ��� ��������
    sell.new=setdiff(names(portfel), buy)

    #### ������� ������ ������ �� ��������, ����� ��������� ���������� �������� ������� ####
    if(length(sell.new)>0) {
      if(diag) write(paste("##### ������� ",paste(sell.new,collapse = " ")),log,,TRUE)
      for(p in sell.new){
        # ������� ���� ����
        price=as.numeric(prices[i,p])
        # ���� ��� ���� - ������� � ���������� ������
        if(is.na(price)) next
        # ���������� ��������
        fee=price * fee.sell
        # ���������� ������� ������ � ��������
        n=as.numeric(portfel[p])
        if(diag) write(paste("���:",p,"����:",price,"���-��:",n,"�����:",n*price,"��������:",n*fee),log,,TRUE)
        # ��������� ���� � ������ ��������
        price=price - fee
        # ������� � ���������� ���������� �������� �������
        bank=bank + n*price
      }
      # ���������� ��������� ����� �� ��������
      portfel=portfel[! names(portfel) %in% sell.new]
    }
    # ���������� ���������� � ��������� ��������
    portfel.history[i]==sum(as.numeric(prices[i,names(portfel)]*portfel))+bank
    # �������� ���� ������ � �������� ������ �� �����
    if(length(portfel)==0 & length(buy.new)==0) next

    #### ����� �������������� ���������� ����� � �������� (�� ������� �����) ####

    # ���� �� ���������� ����� � ��������
    #### �������������� �� ������� ####
    if(diag) write("##### ��������� #####",log,,TRUE)
    for(p in names(portfel)){
      # ������� ���� ������
      price=as.numeric(prices[i,p])
      # ���� ��� ���� - ������� � ���������� ������
      if(is.na(price)) next
      # ���������� ���� � ��������
      n=as.numeric(portfel[p])
      # ��������� ���� ������� ����
      part.price=price*n
      # �������� ����� � ��������
      part.p=part.price/equity
      # ���� ��������� ���� ������ ������ ����� ��������� ����� �����, �� ������� �������
      if(part.p > part){
        # ������� ��� �������
        diff.m=part.price - part.money
        # ����������, ������� ����� ������� (���������� �����)
        n.new= ceiling(diff.m / price)
        # ���� ������ �� ����� ���������, �� �����
        if(n.new==0) next
        # ���������� ��������
        fee=price*fee.sell
        if(diag) write(paste("���:",p,"����:",price,"���-��:",n.new,"�����:",n.new*price,"��������",n.new*fee),log,,TRUE)
        # ��������� ���� � ������ ��������
        price=price-fee
        # ��������� ���������� ���������� �����
        portfel[p]=portfel[p] - n.new
        # ���������� �����
        bank=bank + n.new*price
        # ���� ��� ������ ���� �����
        portfel=portfel[! portfel == 0]
      }
    }

    #### �������������� �� ������� ####
    if(diag) write("##### ��������� #####",log,,TRUE)
    for(p in names(portfel)){
      # ������� ���� ������
      price=as.numeric(prices[i,p])
      # ���� ��� ���� - ������� � ���������� ������
      if(is.na(price)) next
      # ���������� ������ � ��������
      n=as.numeric(portfel[p])
      # ��������� ������ � ��������
      part.price=price*n
      # �������� ����� � ��������
      part.p=part.price/equity
      # ���� ����� ������, �� ����� ��������
      if(part.p < part){
        # ������� ��� �������
        diff.m=part.money - part.price
        # ���������� ��������
        fee=price*fee.buy
        # ���� ��� ��������
        price.old=price
        # ���� � ���������
        price=price + fee
        # ����������, ������� ����� ��������
        n.new=floor(diff.m / price)
        # ���� ������ �� ���� �������� - ������� � ���������� ������
        if(n.new==0) next
        if(diag) write(paste("���:",p,"����:",price.old,"���-��:",n.new,"�����:",n.new*price.old,"��������:",n.new*fee),log,,TRUE)
        # ���������� ���������� ���������� ������ � ��������
        portfel[p]=portfel[p] + n.new
        # ���������� ��������� �������� �������
        bank=bank-n.new*price
      }
    }

    #### ������� ����� ����� ####
    if(length(buy.new)>0) {
      if(diag) write(paste("##### ������� ",paste(buy.new,collapse = " ")),log,,TRUE)
      # ���� �������� - ��� ��� �����, �� ��������� ����� �����
      # ��� ��� ��������� �������� ��������
      if(part==1) part.money=bank
      for(p in buy.new){
        # ������� ���� ������
        price=as.numeric(prices[i,p])
        # ���� ��� ���� - ������� � ���������� ������
        if(is.na(price)) next
        # ���������� ��������
        fee=price*fee.buy
        # ���� ��� ��������
        price.old=price
        # ���� � ���������
        price=price+fee
        # ���������� ������, ������� ����� ������
        n=floor(part.money / price)
        if(diag) write(paste("������:",part.money,"���:",p,"����:",price.old,"���-��:",n,"�����:",n*price.old,"��������:",n*fee),log,,TRUE)
        # ���� ������ �� ������, �� �����
        if(n==0) next
        # ���������� ������ � ��������
        portfel=append(portfel,n)
        # ���������� ��������� �������� �������
        bank=bank - n*price
        # ���������� �������� ������ � ��������
        names(portfel)[length(portfel)]=p
      }
    }
    #####
    if(diag) write("##########",log,,TRUE)
    # ���������� ��������� ��������
    equity=sum(as.numeric(prices[i,names(portfel)]*portfel))+bank
    if(diag) write(paste("��������� ��������:",equity),log,,TRUE)
    if(diag) write(paste("����:",bank),log,,TRUE)
    if(diag) write(paste("��������:",paste(paste0(names(portfel),":"),portfel,collapse=", ")),log,,TRUE)
    if(diag) write(paste("����������:",length(portfel)),log,,TRUE)
    # ���������� ���������� � ��������� ��������
    portfel.history[i]=equity
  }
  # � �������� ���������� ������������
  # ������� ������ � ��������� ��������� ��������
  return(portfel.history)
}

pif.graph = function(portfel, bench, file=""){
  i=index(portfel)[which(as.numeric(portfel)!=as.numeric(portfel[1]))[1]]
  # ����������� ��� � ��������
  dt=intersect(index(portfel),index(bench))
  i=which(as.Date(dt)==i)
  dt=dt[i:length(dt)]
  test=cbind(bench[as.Date(dt)],Return.calculate(portfel[as.Date(dt)]))
  # ������������ ��������
  colnames(test)=c("MOEX","Portfel")
  # ���������� �������
  if(file!=""){
    png(file,width = 1000,height = 1155, units = "px", pointsize = 20)
    charts.PerformanceSummary(test)
    dev.off()
  } else {
    charts.PerformanceSummary(test)
  }
  # ���������
  print(summary(lm(test[,2]~test[,1])))
  portfel.return=Return.calculate(portfel[as.Date(dt)])
  portfel.return[1]=0
  return(portfel.return)
}

boot.all = function(return, bench, rf, boot.n = 10, period = 24, l1 = 0.99, l2 = 0.5){
  # ���������� ��� ������������� �������� �������
  require(tseries)
  # �������� ������� t-���������
  boot=matrix(NA,nrow = ncol(return),ncol = (boot.n + 1))
  # �������� ����� ������������� �������� ������
  row.names(boot)=colnames(return)
  # ���� �� ������� �����
  for(j in 1:ncol(return)){
    # ������ �� ������ �����
    pif=return[,j]
    # �������� ������ ��������
    pif=pif[!is.na(pif)]
    # ���� ����������� ���������� ������
    if(length(na.trim(pif))>period){
      # ����������� ������ �� �����, ��������� � ����������� ������ �� �������
      time=intersect(intersect(index(pif),index(rf)),index(bench))
      # ���� ���������� ������ ���������� ������
      if(length(time)>period){
        # ���������� ���������� �����
        Er=Return.excess(pif[as.Date(time)],rf[as.Date(time)])
        # ���������� ���������� ���������
        Eb=Return.excess(bench[as.Date(time)],rf[as.Date(time)])
        # ���� ���������� ������ ���������
        if(length(Er)==length(Eb)){
          # ��������� �� �������� ������
          capm=summary(lm(Er ~ Eb))
          # ����� �����������
          capm.alfa=capm$coefficients[1,1]
          # t-���������� ����� ������������
          capm.t=capm$coefficients[1,3]
          # ���� �����������
          capm.beta=capm$coefficients[2,1]
          # �������, ������ ���������
          capm.res=capm$residuals
          # ���������� ��������� �������� t-����������
          boot[j,1]=capm.t
          # �������������� ���������� ����������
          const = capm.beta * Eb
          # ��������
          # ���� ��������� �� ��������������� ������
          for(k in 1:boot.n){
            # ������������� ��������
            res=xts(tsbootstrap(ts(capm.res)), order.by = index(capm.res))
            # ���������� �������� � ��������������� ������
            Er.b = const + res
            # ��������� �� ��������������� ������
            capm.b = summary(lm(Er.b ~ const))
            # ���������� �������������� t-����������
            boot[j,(1+k)]=capm.b$coefficients[1,3]
          }
        }
      }
    }
  }

  # ������ ������ �������������� �������������
  t.best={}
  # ���� �� �������� ���������
  for (l in 2:boot.n){
    # ����� � ���������� ������ ������������� �� �������
    t.best=c(t.best,boot[which(boot[,l]>=quantile(na.omit(boot[,l]),c(l1)) & boot[,l]>0),l])
  }

  # ������ ������ ������
  pif.best={}
  # ���� �� ������� ������� �������
  # � �������������� �� �������� ������
  for(l in 1:nrow(boot)){
    # ���� ���� �������� ������������ � �����
    if(!is.na(boot[l,1])){
      # ���� �������� ����������� ���� ������ �������� ������� ������ �������������
      if(boot[l,1]>quantile(t.best,c(l2))){
        # ���������� �������� ������������ ����� � ������
        pif.best=c(pif.best,boot[l,1])
        # ���������� �������� ����� � �����������
        names(pif.best)[length(pif.best)]=row.names(boot)[l]
      }
    }
  }
  # ���������� ������� ������ ������
  # �� �������� �������� �������������
  pif.best=sort(pif.best, decreasing = TRUE)
  # ������� � �������� ����������
  # ������ ������ ������
  return(pif.best)
}

boot.history = function(return, bench, rf, boot.n = 10, period = 24, l1 = 0.99, l2 = 0.5){
  # ���������� ��� ������������� �������� �������
  require(tseries)
  # �������� ������� � ������������ ���������
  # ��������� ���������� � ������� ����������� �������
  boot.res=return
  # ������� ������
  boot.res[]=NA
  # ���� � ���������� �����
  for(i in 1:(nrow(return)-(period-1))){
    # ������ �� ���������� ������� �� ������
    dta.m=return[i:(i+(period-1)),]
    # ������� t-���������
    boot=matrix(NA,nrow = ncol(dta.m),ncol = (boot.n + 1))
    # �������� ����� ������������� �������� �������
    row.names(boot)=colnames(dta.m)
    # ���� �� ������� ������
    for(j in 1:ncol(dta.m)){
      # ������ �� ������ �� ������
      pif=dta.m[,j]
      # ���� ����������� ���������� ������
      if(length(na.trim(pif))==period){
        # ����������� ������ �� �����, ��������� � ����������� ������ �� �������
        time=intersect(intersect(index(pif),index(rf)),index(bench))
        # ���� ���������� ������ ���������� ������
        if(length(time)==period){
          # ���������� ���������� �����
          Er=Return.excess(pif[as.Date(time)],rf[as.Date(time)])
          # ���������� ���������� ���������
          Eb=Return.excess(bench[as.Date(time)],rf[as.Date(time)])
          # ���� ���������� ������ ���������
          if(length(Er)==length(Eb)){
            # ��������� �� �������� ������
            capm=summary(lm(Er ~ Eb))
            # ����� �����������
            capm.alfa=capm$coefficients[1,1]
            # t-���������� ����� ������������
            capm.t=capm$coefficients[1,3]
            # ���� �����������
            capm.beta=capm$coefficients[2,1]
            # �������, ������ ���������
            capm.res=capm$residuals
            # ���������� ��������� �������� t-����������
            boot[j,1]=capm.t
            # �������������� ���������� ����������
            const = capm.beta * Eb
            # ��������
            # ���� ��������� �� ��������������� ������
            for(k in 1:boot.n){
              # ������������� ��������
              res=xts(tsbootstrap(ts(capm.res)), order.by = index(capm.res))
              # ���������� �������� � ��������������� ������
              Er.b = const + res
              # ��������� �� ��������������� ������
              capm.b = summary(lm(Er.b ~ const))
              # ���������� �������������� t-����������
              boot[j,(1+k)]=capm.b$coefficients[1,3]
            }
          }
        }
      }
    }

    # ������ ������ �������������� �������������
    t.best={}
    # ���� �� �������� ���������
    for (l in 2:boot.n){
      # ����� � ���������� ������ ������������� �� �������
      t.best=c(t.best,boot[which(boot[,l]>=quantile(na.omit(boot[,l]),c(l1)) & boot[,l]>0),l])
    }

    # ������ ������ ������
    pif.best={}
    # ���� �� ������� ������� �������
    # � �������������� �� �������� ������
    for(l in 1:nrow(boot)){
      # ���� ���� �������� ������������ � �����
      if(!is.na(boot[l,1])){
        # ���� �������� ����������� ���� ������ �������� ������� ������ �������������
        if(boot[l,1]>quantile(t.best,c(l2))){
          # ���������� �������� ������������ ����� � ������
          pif.best=c(pif.best,boot[l,1])
          # ���������� �������� ����� � �����������
          names(pif.best)[length(pif.best)]=row.names(boot)[l]
        }
      }
    }
    # ���������� ������� ������ ������
    # �� �������� �������� �������������
    pif.best=sort(pif.best, decreasing = TRUE)
    # ������ ������ �� ������ ������ ���������� � ����� �������
    boot.res[tail(index(dta.m),n=1),names(pif.best)]=c(1:length(pif.best))
  }
  # ������ ���� �� ��������
  sums=colSums(boot.res,na.rm = TRUE)
  # �������� �� �����
  # ������� �� ���� �� ���� ��������
  sums=sums[sums>0]
  # �������� ������ ������ �� �������
  boot.res=boot.res[,names(sums)]
  # ����������� ������� � �������� ���������� �������
  return(boot.res)
}

boot.act=function(boot.res, period = 4){
  # �������� ����� �������
  copy=boot.res
  # �������� ������
  copy[]=NA
  # ��������� ��� ������
  # ���������� ����� � �������
  # ����� �������������
  j=period - 1
  for(i in 1:(nrow(boot.res)-(period-1))){
    # ��������� ������ �� ������ � ����������
    dta.m=boot.res[i:(i+(period-1)),]
    # ��������, ���������� �� ������ � �������
    if(length(sort(colSums(dta.m)))>0){
      # ���������� ������� �� 1 �����
      j=j+1
      # ���� ���������� ������� ������ �������
      # �� ����� ��������� ����� ���� ��� ����������
      # �� ��������� ������
      if(j%%period==0){
        # ����������� ����� �� ������� �����
        # ����� ������ ������������ � ������
        best=names(sort(colSums(dta.m)))
        n=as.numeric(floor(quantile(c(0:length(best)),.25)))
        # ���������� ������ ������ ����������
        #k=i+period
        k=ifelse((i+period)>=nrow(copy),nrow(copy),(i+period))
        # ���������� ��������� ������ ����������
        k1=ifelse((k+(period-1))>=nrow(copy),nrow(copy),(k+(period-1)))
        if(k==k1) next
        # ������ � ������� ���������� � ���,
        # � ����� ���� ����� ������� ���������
        copy[index(copy[k:k1,]),best[1]]=1
      }
    }
  }
  # ������ ���� �� ��������
  sums=colSums(copy,na.rm = TRUE)
  # �������� �� �����
  # ������� �� ���� �� ���� ��������
  sums=sums[sums>0]
  # �������� ������ ������ �� �������
  copy=copy[,names(sums)]
  # ����������� ������� � �������� ���������� �������
  return(copy)
}
