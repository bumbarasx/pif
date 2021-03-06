##### ������, ��������� #####
require(RSelenium) # ������ ����
require(wdman) # �������
require(quantmod)
require(PerformanceAnalytics)
require(gdata) # ���������� �����
require(tseries)

Sys.setlocale("LC_TIME", "English")
DIR<-dirname(rstudioapi::getSourceEditorContext()$path)
source(paste0(DIR,"/functions.R"))

##### ��������� ������, ���� ��� ����������� #####
pjs <- phantomjs() # ������ ������� - ��������
pjs$output()
rem <- remoteDriver(remoteServerAddr = "localhost"
                    , port = 4567L
                    , browserName = "phantomjs"

) # �������� ����������� ��� Selenium
rem$open() # ����������� � ��������
rem$getStatus()
links=names=c() # ������ ������ � ��������
for (i in 0:1){ # 2 ��������
  link=paste0("http://pif.investfunds.ru/funds/index.phtml?page=funds&type[]=%D0%BE%D1%82%D0%BA%D1%80%D1%8B%D1%82%D1%8B%D0%B9&c_val[2]=23&value=2&npage=",i) # ����� ��������
  rem$navigate(link) # �������� ��������
  rem$getCurrentUrl() # ������� �����
  elem <- rem$findElements(using = 'xpath', "//tr/td[1]/a[@class='link']") # ����� ���������
  names <- append(names,unlist(lapply(elem, function(x){x$getElementText()}))) # ������ �������� �����
  links <- append(links,unlist(lapply(elem, function(x){x$getElementAttribute("href")}))) # ������ ������ �����
}

all=data.frame( # ������� � ��������� � ����� ����������� �����
  Name=as.character(), # �������� ����
  Link=as.character(), # ������ �� ���
  Number=as.character(), # ���������� �����
  FeeMan=as.numeric(), # �������������� ���������
  FeeDep=as.numeric(), # �������������� �����������
  FeeOth=as.numeric(), # ������ �������
  stringsAsFactors=FALSE
)

allD=list() # ������ �� ���� � ��� ��� �����
dir=getwd()
for(i in 1:length(names)){ # ���� �� ������� ����
  all[i,"Name"]<-names[i] # �������� ����
  all[i,"Link"]<-links[i] # ������ �� ���
  all[i,"Number"]<-regmatches(links[i], regexpr("[0-9]+", links[i], perl=TRUE)) # ���������� ����� ���� �� �����
  rem$navigate(links[i]) # �������� �������� ����

  elem<-rem$findElement(using = 'xpath', "//a[@class='popup-link arr-open-title']") # ����� ������ �� ����� ��� ����������� ����������
  elem$clickElement() # ������� ������
  elem <- rem$findElements(using = 'xpath', "//td[@style='border-top:0px;text-align:center']") # ������ ������ �� ��������������
  z=unlist(lapply(elem, function(x){x$getElementText()})) # ��������� ������������� ������ �� ��������������
  all[i,c("FeeMan","FeeDep","FeeOth")]=as.numeric(regmatches(z, regexpr("[0-9.]+", z, perl=TRUE)))/100 # ���������� � �������� ����

  download.file(paste0("http://pif.investfunds.ru/funds/export_to_excel.php?f2[0]=",all[i,"Number"],"&export=2&export_type=xls&start_day&finish_day=08&finish_month=02&finish_year=2018&rnd=5030"),destfile="pif.xls",mode="wb",quiet=TRUE) # ���������� ����� � ������� �� ���� � ��� ����
  x<-read.xls(xls=paste0(dir,"/pif.xls"),perl="C:/Perl64/bin/perl.exe") # ������ ����� � ���������� - �������
  x<-x[which(x[,1]!=""&x[,1]!="00.00.0000"),] # ������� ������ ����� � ������������ ���
  x<-x[2:length(x[,1]),1:3] # ������� �� ��������
  x[,2]=as.numeric(gsub(" ","",as.character(x[,2]))) # �������������� ���� �� ���������� � �������� ������
  x[,3]=as.numeric(gsub(" ","",as.character(x[,3]))) # �������������� ��� �� ���������� � �������� ������
  x1<-as.xts(x[,2],order.by=as.Date(x[,1], "%d.%m.%Y")) # �������������� ���� � xts ������
  x2<-as.xts(x[,3],order.by=as.Date(x[,1], "%d.%m.%Y")) # �������������� ��� � xts ������
  allD[[i]]=list(x1,x2) # ���������� � ���� ���� xts �������
  print(rem$getCurrentUrl()) # ������ �������� ������ ��������
}
names(allD)=all[,"Number"] # �������� � ������ �������������� ����������� ������ �����

save(all, file=paste0(DIR,"/pifdata/index/all.RData")) # ���������� � ����
save(allD, file=paste0(DIR,"/pifdata/index/allD.RData")) # ���������� � ����

##### ����������� ������, ���� ���� ��������� #####
load(paste0(DIR,"/pifdata/index/all.RData"))
load(paste0(DIR,"/pifdata/index/allD.RData"))

##### ���� � ���������� ������ #####
pif.price=xts()
pif.return=xts()
for(i in 1:length(allD)){
  if(i==1){
    # ���������� ������ ������� �������
    pif.price=Cl(to.monthly(allD[[i]][[1]],indexAt = 'lastof'))
    pif.return=monthlyReturn(to.monthly(allD[[i]][[1]],indexAt = 'lastof'))
  } else {
    # ���������� ��������� ������� � �������
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
##### ����������� ������, ������ ��������, ������ ���� 10 #####
# ������ ������ �� �����
ru.1y=read.csv(paste0(DIR,"/RU1Y.csv"),stringsAsFactors = FALSE)
# �������������� � ������ ������ xts
ru=xts(as.numeric(ru.1y[(1:(nrow(ru.1y)-2)),2]),order.by = as.Date(ru.1y[(1:(nrow(ru.1y)-2)),1], format = "%b %d, %Y"))
# ������� ���������� �� ��� � ���������� �� �����
ru.1y=xts((1 + Cl(to.monthly(ru,indexAt = 'lastof'))/100)^(1/12)-1)

# ������ ������ �� �����
moex=read.csv(paste0(DIR,"/MOEX.csv"),stringsAsFactors = FALSE)
# �������������� � ������ ������ xts
moex=xts(as.numeric(gsub(",", "", moex[,2])),order.by = as.Date(moex[,1], format = "%b %d, %Y"))
# ���������� ���������� �� �����
moex=monthlyReturn(to.monthly(moex, indexAt = 'lastof'))

moex10=read.csv(paste0(DIR,"/MOEX10.csv"),stringsAsFactors = FALSE)
moex10=xts(as.numeric(gsub(",", "", moex10[,2])),order.by = as.Date(moex10[,1], format = "%b %d, %Y"))
moex10=monthlyReturn(to.monthly(moex10, indexAt = 'lastof'))

##### ��������� ��� ��������� ������ #####
# ������ ������
period = 24
# ������� ������
pif.act=pif.return
# ������� ��������
pif.act[]=NA
# ���������� ������ ������ � �����
for(i in 1:(nrow(pif.return)-(period-1))){
  # ��������� ������ �� ������ � ����������
  dta.m=pif.return[i:(i+(period-1)),]
  # ������ � ������ ������ ������
  diff.best=c()
  for(j in 1:ncol(dta.m)){
    # ������ �� ������� ����� �������� � �������
    pif=dta.m[,j]
    diff.best[j]=NA
    # �������� �� ��, ��� ���������� ������ �� �����
    # ������������ ����� �������
    if(length(na.trim(pif))==period){
      # ���������� ����������� ��� � �������� ��������
      time=intersect(index(pif),index(moex))
      # ���������� �������� ����� ������ � ��������
      diff.index=abs(Return.excess(pif,moex[as.Date(time)]))
      # ���������� ���������� ��� ��������� � ������� �������
      diff.best[j]=mean(diff.index)*sd(diff.index)
    }
  }
  # ��������, ���� ��� ������ ������
  if(length(diff.best[is.na(diff.best)])==length(diff.best)) next
  # ���������� �������� ������ � �� ������
  names(diff.best)=colnames(dta.m)
  # �������� ������ �� �����, �� ������� ������������ ������
  diff.best=diff.best[!is.na(diff.best)]
  #pif.best.name=names(diff.best)[which(diff.best==min(diff.best,na.rm = TRUE))]
  #pif.act[tail(index(pif),n=1),pif.best.name]=TRUE
  # ������ � ����� ������� ������ ������
  pif.act[tail(index(pif),n=1),names(sort(diff.best))]=c(1:length(diff.best))
}

# �������� ����� �������
pif.act.copy=pif.act
# ������ ��� ����������
period=12
# �������� ������
pif.act[]=NA
# ��������� ��� ������
# ���������� ����� � �������
# ����� �������������
j=11
for(i in 1:(nrow(pif.act.copy)-(period-1))){
  # ��������� ������ �� ������ � ����������
  dta.m=pif.act.copy[i:(i+(period-1)),]
  # ��������, ���������� �� ������ � �������
  if(sum(dta.m[1,],na.rm = TRUE)>0){
    # ���������� ������� �� 1 �����
    j=j+1
    # ���� ���������� ������� ������ �������
    # �� ����� ��������� ����� ���� ��� ����������
    # �� ��������� ������
    if(j%%period==0){
      # ����������� ����� �� ������� �����
      # ����� ������ ������������ � ������
      best=names(sort(colSums(dta.m)))[1]
      # ���������� ������ ������ ����������
      k=i+period
      # ���������� ��������� ������ ����������
      k1=ifelse((k+(period-1))>nrow(pif.act.copy),nrow(pif.act.copy),(k+(period-1)))
      # ������ � ������� ���������� � ���,
      # � ����� ���� ����� ������� ���������
      pif.act[index(pif.act.copy[k:k1,]),best[1]]=1
    }
  }
}

##### ���������� ��������� �� ������ ��������� ������ #####
# �������� �������� �� ��������� ������ ������
benchmark.pif=portfel.equity(pif.price, pif.act, 1000000, c(0.005, 0.015))
# ���������� ���������
benchmark.pif.return=pif.graph(benchmark.pif, moex)
