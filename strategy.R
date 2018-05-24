# функция вычисления лучших фондов по периодам
best.pif.boot=boot.history(return = pif.return, bench = benchmark.pif.return, rf = ru.1y, boot.n = 100, period = 24, l1 = 0.99, l2 = 0.5)
# функция для установки индикаторов
best.pif.act=boot.act(best.pif.boot,4)
# построение портфеля
portfel=portfel.equity(pif.price, best.pif.act, 1000000, c(0.005, 0.015))
# доходность портфеля
portfel.return=pif.graph(portfel,moex)
