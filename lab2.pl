fecha(Dia,Mes,Anio,[Dia,Mes,Anio]):- number(Dia),number(Mes),number(Anio), Dia > 0,
    Mes > 0, Anio > 0, Dia =< 31, Mes =< 12.
