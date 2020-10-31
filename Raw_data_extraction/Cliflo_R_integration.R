# install.packages("clifro")
library(clifro)

# Excuse the horrific coding practices throughout...



# Licence key
private.cfuser = cf_user(username = "calroughan", password = "KOHC2SRL")
public.cfuser = cf_user()     # Only reefton data is available with the public account

# Example datatype
# daily.wind.rain.dt = cf_datatype(c(5), c(2), list(4, 1), c(1, NA))

# Public (free) station to query. Test all queries on this one first
# reefton.st = cf_station()

# Locations of stations
# all.auckland.st = cf_find_station("Auckland", search = "region", status = "open")
# cf_save_kml(all.auckland.st, "all_auckland_stations")                             # Open this file in google earth to visualise them
# 
# all.wellington.st = cf_find_station("Wellington", search = "region", status = "open")
# cf_save_kml(all.wellington.st, "all_wellington_st")
# 
# all.christchurch.st = cf_find_station("Christchurch", search = "region", status = "open")
# cf_save_kml(all.christchurch.st, "all_christchurch_st")
# 
# all.queenstown.st = cf_find_station("Queenstown", search = "region", status = "open")
# cf_save_kml(all.queenstown.st, "all_queenstown_st")


# Chosen five stations
Auckland.station <-     cf_station(2006) 
Blenheim.station <-   cf_station(12430)
Wellington.station <-   cf_station(18234)
Christchurch.station <- cf_station(17603)
Queenstown.station <-   cf_station(5535)

#my.dts = cf_datatype() # To step through the menu

Auck.df1 = cf_query(user = private.cfuser,
              datatype = cf_datatype(5, 2, c(2, 3, 4)),
              station = Auckland.station,
              start_date = "2016-03-01 00",
              end_date = "2020-03-01 00")

Auck.df2 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Auckland.station,
                    start_date = "2012-03-01 00",
                    end_date = "2016-03-01 00")

Auck.df3 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Auckland.station,
                    start_date = "2010-03-01 00",
                    end_date = "2012-03-01 00")


Well.df1 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Wellington.station,
                    start_date = "2016-03-01 00",
                    end_date = "2020-03-01 00")

Well.df2 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Wellington.station,
                    start_date = "2012-03-01 00",
                    end_date = "2016-03-01 00")

Well.df3 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Wellington.station,
                    start_date = "2010-03-01 00",
                    end_date = "2012-03-01 00")


Blen.df1 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Blenheim.station,
                    start_date = "2016-03-01 00",
                    end_date = "2020-03-01 00")

Blen.df2 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Blenheim.station,
                    start_date = "2012-03-01 00",
                    end_date = "2016-03-01 00")

Blen.df3 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Blenheim.station,
                    start_date = "2010-03-01 00",
                    end_date = "2012-03-01 00")

Chch.df1 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Christchurch.station,
                    start_date = "2016-03-01 00",
                    end_date = "2020-03-01 00")

Chch.df2 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Christchurch.station,
                    start_date = "2012-03-01 00",
                    end_date = "2016-03-01 00")

Chch.df3 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Christchurch.station,
                    start_date = "2010-03-01 00",
                    end_date = "2012-03-01 00")

Qns.df1 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Queenstown.station,
                    start_date = "2016-03-01 00",
                    end_date = "2020-03-01 00")

Qns.df2 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Queenstown.station,
                    start_date = "2012-03-01 00",
                    end_date = "2016-03-01 00")

Qns.df3 = cf_query(user = private.cfuser,
                    datatype = cf_datatype(5, 2, c(2, 3, 4)),
                    station = Queenstown.station,
                    start_date = "2010-03-01 00",
                    end_date = "2012-03-01 00")





Auck.df1 <- as.data.frame(Auck.df1)
Auck.df2 <- as.data.frame(Auck.df2)
Auck.df3 <- as.data.frame(Auck.df3)

Well.df1 <- as.data.frame(Well.df1)
Well.df2 <- as.data.frame(Well.df2)
Well.df3 <- as.data.frame(Well.df3)

Blen.df1 <- as.data.frame(Blen.df1)
Blen.df2 <- as.data.frame(Blen.df2)
Blen.df3 <- as.data.frame(Blen.df3)

Chch.df1 <- as.data.frame(Chch.df1)
Chch.df2 <- as.data.frame(Chch.df2)
Chch.df3 <- as.data.frame(Chch.df3)

Qns.df1 <- as.data.frame(Qns.df1)
Qns.df2 <- as.data.frame(Qns.df2)
Qns.df3 <- as.data.frame(Qns.df3)

write.csv(rbind(Auck.df1, Auck.df2, Auck.df3), file = "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_Jamie\\Wind\\NIWA_Solar_data_Auckland.csv")
write.csv(rbind(Well.df1, Well.df2, Well.df3), file = "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_Jamie\\Wind\\NIWA_Solar_data_Wellington.csv")
write.csv(rbind(Blen.df1, Blen.df2, Blen.df3), file = "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_Jamie\\Wind\\NIWA_Solar_data_Blenheim.csv")
write.csv(rbind(Chch.df1, Chch.df2, Chch.df3), file = "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_Jamie\\Wind\\NIWA_Solar_data_Christchurch.csv")
write.csv(rbind(Qns.df1, Qns.df2, Qns.df3), file = "C:\\Users\\calro\\Documents\\Academic\\JuDGE project\\LDC_Jamie\\Wind\\NIWA_Solar_data_Queenstown.csv")


plot(df)   





