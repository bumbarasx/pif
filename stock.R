# ��������� ������������ ������
files=list.files(paste0(DIR,"/dat"),full.names=TRUE)
# ����� ������� � �������
data.equity=data.frame()
for(file in files){
  # ������ ������ �����
  eq=read.csv(file,stringsAsFactors=FALSE)
  # ���������� ������ � ���� �������
  data.equity=rbind(data.equity,eq)
}

# ���������� � ���� ������� ������ ���� 10
mic.10<-read.csv(paste0(DIR,"/10micexdata.csv"),header = FALSE,stringsAsFactors = FALSE)
# ��������� ������������ ����������� �������� �����
mic.10.replace=read.csv(paste0(DIR,"/10micex.csv"),header = FALSE,stringsAsFactors = FALSE)
# ������ ������������ ����������� �������� ����� � ���� �������
for(i in 1:nrow(mic.10.replace)){
  mic.10[mic.10==mic.10.replace[i,1]]=mic.10.replace[i,2]
}

mic.ticker=c()
mic.ticker=unique(unlist(sapply(c(18:nrow(mic.10)),function(i){mic.ticker=append(mic.ticker,mic.10[i,3:12])})))
mic.ticker=mic.ticker[mic.ticker!=""]


date.all=c() # ������ �� ����� ������
for(i in 18:nrow(mic.10)){
  # ���� ������� ������� ���� 10 �� ������������ ������
  tickers=unlist(mic.10[i,3:length(mic.10[i,])])
  # ������ ����������� ��� ��� ���� �������
  dates=seq.Date(as.Date(mic.10[i,1],"%d.%m.%Y"), as.Date(mic.10[i,2],"%d.%m.%Y"), by = "day")
  date.t=c() # ������ ��� ��� ������� ���� �������
  for(t in tickers){
    # ������ �� ����� ����� �� ���� �������
    y=data.equity[which(data.equity[,1]==t),]
    # ����������� ���������� ��� � �������� ����
    date.inter=intersect(dates,as.Date(y[,2]))
    # ���������� ����������� ��� � ������
    date.t=append(date.t,date.inter)
  }
  # ���������� ���������� ��� � ����� ������
  date.all=append(date.all,unique(date.t))
}

# ����������������� ������� � �������
data.10=xts(matrix(NA, nrow = length(date.all), ncol = length(mic.ticker),dimnames = list(date.all,mic.ticker)),order.by = as.Date(date.all))

for(t in colnames(data.10)){
  # ������ �� ����� ����� �� ���� �������
  y=data.equity[which(data.equity[,1]==t),]
  # ���� �������� �� ������ �����
  y=xts(Cl(y),order.by=as.Date(y[,2]))
  # ���������� ������ � �������
  data.10[index(y),t]=y[index(data.10)]
}

# ��������, ��� ������ ���� ������ �� 10 ������
for(i in 1:nrow(data.10)){
  r=data.10[i,]
  if(length(r[,!is.na(r)])<10) print(index(r))
}

# ����������������� ������� � �������
data.act=xts(matrix(NA, nrow = length(date.all), ncol = length(mic.ticker),dimnames = list(date.all,mic.ticker)),order.by = as.Date(date.all))
for(i in 18:nrow(mic.10)){
  # ���� ������� �������
  tickers=unlist(mic.10[i,3:length(mic.10[i,])])
  # �������� ������ ��������
  tickers=tickers[tickers!=""]
  # ������ �������� ���� �������
  dates=seq.Date(as.Date(mic.10[i,1],"%d.%m.%Y"), as.Date(mic.10[i,2],"%d.%m.%Y"), by = "day")
  # ���������� �����������, � ��������� ����� � ��������
  data.act[dates,tickers]=TRUE
}
# ����� �����
data.act["2007-07-18","SBER"]=1000
data.act["2007-07-18","SBERP"]=20
# ���������� �����������
# ����� ����� �� ���� �������� �� ��������
data.act[c("2007-07-19","2007-07-20"),c("SBER","SBERP")]=TRUE

# ��������, ��� ������ ���� ������ �� 10 ������
for(i in 1:nrow(data.act)){
  r=data.act[i,]
  if(length(r[,!is.na(r)])<10) print(index(r))
}

##### ���������� ��������� �� ������ ����� �� ������� ������� ���� 10 #####
# ����� ������� � �����������
benchmark.stock=portfel.equity(data.10, data.act, 1000000, c(0.0001, 0.0001))
# �������� ���������� ���������
benchmark.stock.return=monthlyReturn(Cl(to.monthly(benchmark.stock, indexAt = 'lastof')))
dt=intersect(index(benchmark.stock.return),index(moex10))
test=cbind(moex10[as.Date(dt)],benchmark.stock.return[as.Date(dt)])
# ������������ ��������
colnames(test)=c("MOEX10","Portfel")
charts.PerformanceSummary(test)
print(summary(lm(test[,2]~test[,1])))
