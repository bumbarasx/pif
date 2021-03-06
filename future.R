# ������ ������ �� �����
future=read.csv(paste0(DIR,"/MX.csv"), stringsAsFactors = F)
# ������������ ���������
future.name=unique(future[,2])
# ������� �� ���������� �� �� ���������
future.data=as.data.frame(matrix(NA,nrow = nrow(future), ncol = length(future.name)))
# �������� �����
row.names(future.data)=future[,1]
# �������� ��������
colnames(future.data)=future.name
# �������������� �� ��������� ������ ������
future.data=as.xts(future.data, order.by = as.Date(row.names(future.data), "%d.%m.%Y"))
# �������� ����� ������ ������������
future.price=future.price.estim=future.action=future.data

# ���������� ������ �� ������� ��������
for(name in future.name){
  # ��������� ����������
  x=future[future[,2]==name,]
  # �������������� ������� ����
  date=as.Date(x[,1], "%d.%m.%Y")
  # ���������� �������� ��
  future.data[date,name]=x[,16]
  # ���������� ������������ ��������
  future.action[date,name]=1
  # ���������� �������� ������� ���������� ����
  future.price[date,name]=x[,3]
  # ���������� �������� ���� �������
  future.price.estim[date,name]=x[,4]
  # �������� ��������� ����������
  rm(x,date)
}

# �������� ��������� �������� �������
portfel.bank=1000000
# ������������ �������� �������� � ��������
portfel.name=""
# ������� ��������� ��������� ��������
portfel.history=future.data[,1]
# �������� ������
portfel.history[]=NA

for(i in 1:nrow(future.data)){
  # ������� � ������� ����� ������������� �� ������ ��������
  buy=colnames(future.action)[which(!is.na(future.action[i,]))]
  # ���� ������� ���������� �� �������� � ��������
  # � �������� �� ������ �� ���������� ������������ ��
  if(buy!=portfel.name & portfel.name!=""){
    # �� ������������ � ��������� ��������
    portfel.bank=portfel.bank + portfel.warranty
    # �������� ����������
    rm(portfel.warranty,portfel.count,portfel.price)
  }

  # ���� ����� ������� � ������� � �������� �� ���������
  # �� ����� ������������� � ������ �������
  # �� ���� ����������� �� ���������� ����
  if(buy!=portfel.name){
    # ������� ��������� �������� - ��� ��
    price=as.numeric(future.data[i,buy])
    # ���������� ��� "�����"
    # ���� ���������� ��������� ��������� ���� ����
    n=ceiling(portfel.bank / as.numeric(future.price[i,buy]))
    # ��������� �������� ��������
    # ����������� ������ �� �������� ��
    portfel.bank=portfel.bank - n * price
    # ���������� ����������
    portfel.count=n
    # ���������� ������������ ��������
    portfel.name=buy
    # ���� �������� ��� ���������� ������������� ����������
    portfel.price=as.numeric(future.price[i,buy])
    # ����� ������ ��
    portfel.warranty=n * price
    # �������� ����������
    rm(price,cash,n)
  }
  # ���� � �������� ���� �������
  if(portfel.count > 0){
    # ���������� �������� ��
    warranty.price = as.numeric(future.data[i,portfel.name])
    if(!is.na(warranty.price)){
      # ���������� ���������� �� ����������� ��������
      warranty=warranty.price * portfel.count
      # ������� ����� ����������������� � ���������� ��
      diff.warranty = portfel.warranty - warranty
      # ��������� �������� ��������� �������
      portfel.bank = portfel.bank + diff.warranty
      # ����������������� �� ���������� ����������
      portfel.warranty = warranty
    }
    # ���������� ������������� ����������
    if(portfel.price==as.numeric(future.price[i,portfel.name])){
      # ������������ ���� � ������ ����
      margin=(as.numeric(future.price.estim[i,portfel.name]) - portfel.price) * portfel.count
    } else {
      # ����� ����� ������ ��� ������� ��������� ���
      margin=(as.numeric(future.price.estim[i,portfel.name]) - as.numeric(future.price.estim[(i-1),portfel.name])) * portfel.count
    }
    # ���� ��������� ������� � �� �� �������
    # ��� �������� ������������� ����������
    if((portfel.bank + portfel.warranty) < margin){
      # ���������� margin call
      print("���� �������")
      break
    }
    # � ����� ������� ��������� ��� ��������� ��������
    # ���������� �� �������� ������������� ����������
    portfel.bank = portfel.bank + margin
    # �������� ����������
    rm(warranty,margin,diff.warranty,warranty.price)
  }

  portfel.history[i]=portfel.bank + portfel.warranty
}

##### ���������� ��������� �� ������ �������� �� ������ �������� #####
# �������� ���������� ���������
benchmark.future.return=monthlyReturn(Cl(to.monthly(portfel.history, indexAt = 'lastof')))
