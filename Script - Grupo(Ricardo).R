flights$Dia_Semana = wday(flights$time_hour)
flights$Mes = month(flights$time_hour)

View(flights)