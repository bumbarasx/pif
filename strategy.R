# ������� ���������� ������ ������ �� ��������
best.pif.boot=boot.history(return = pif.return, bench = benchmark.pif.return, rf = ru.1y, boot.n = 100, period = 24, l1 = 0.99, l2 = 0.5)
# ������� ��� ��������� �����������
best.pif.act=boot.act(best.pif.boot,4)
# ���������� ��������
portfel=portfel.equity(pif.price, best.pif.act, 1000000, c(0.005, 0.015))
# ���������� ��������
portfel.return=pif.graph(portfel,moex)
