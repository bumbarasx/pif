##### ��������� ������, ���� ��� ����������� #####
links=names=c() # ������ ������ � ��������
for (i in 0:1){ # 2 ��������
  link=paste0("http://pif.investfunds.ru/funds/index.phtml?page=funds&type[]=%D0%BE%D1%82%D0%BA%D1%80%D1%8B%D1%82%D1%8B%D0%B9&c_val[1]=8&value=1&npage=",i) # ����� ��������
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

save(all, file=paste0(DIR,"/pifdata/stock/all.RData")) # ���������� � ����
save(allD, file=paste0(DIR,"/pifdata/stock/allD.RData")) # ���������� � ����

##### ����������� ������, ���� ���� ��������� #####
load(paste0(DIR,"/pifdata/stock/all.RData"))
load(paste0(DIR,"/pifdata/stock/allD.RData"))

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


##### ���������� �������� ������ �� ��� ���������� #####
# ������� ���������� ������ ������
best.pif=boot.all(return = pif.return, bench = benchmark.pif.return, rf = ru.1y, boot.n = 100, period = 24, l1 = 0.99, l2 = 0.5)
# ���������
cat(paste(all[all[,3] %in% names(best.pif),1],round(best.pif,2)),sep = "\n")

# ������� ���������� ������ ������
best.stock=boot.all(return = pif.return, bench = benchmark.stock.return, rf = ru.1y, boot.n = 100, period = 24, l1 = 0.99, l2 = 0.5)
# ���������
cat(paste(all[all[,3] %in% names(best.stock),1],round(best.stock,2)),sep = "\n")

# ������� ���������� ������ ������
best.future=boot.all(return = pif.return, bench = benchmark.future.return, rf = ru.1y, boot.n = 100, period = 24, l1 = 0.99, l2 = 0.5)
# ���������
cat(paste(all[all[,3] %in% names(best.future),1],round(best.future,2)),sep = "\n")
