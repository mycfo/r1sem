library(ggplot2)

data<-read.csv(file="~/R/cdv/data.csv",sep=";",dec=",",encoding = "UTF-8")

#Tworzymy dataframe

names(data)[3:16]<-paste0("Mąka_Pszenna",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(data)[17:30]<-paste0("Mięso_Wołowe_Bez_Kości",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(data)[31:44]<-paste0("Mleko_Krowie_Spożywcze_O_Zawartości_Tłuszczu_Sterylizowane",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(data)[45:58]<-paste0("Śmietana_O_Zawartości_Tłuszczu",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(data)[59:72]<-paste0("Cukier_Biały_Kryształ",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(data)[73:86]<-paste0("Papierosy",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(data)[87:100]<-paste0("Centralne_Ogrzewanie_Lokali_Mieszkalnych",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(data)[101:114]<-paste0("Olej_Napędowy",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(data)[115:128]<-paste0("Bilet_Do_Kina",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(data)[129:142]<-paste0("Gazeta_Regionalna",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))


woj<-rep(c("Polska","dolnoslaskie","kujawsko-pomorskie","lubelskie","lubuskie","lodzkie","malopolskie","mazowieckie","opolskie",
           "podkarpackie","podlaskie","pomorskie","slaskie","swietokrzyskie","warminsko-mazurskie","wielkopolskie","zachodniopomorskie"),12*140)
mies<-rep(c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paz","lis","gru"),each=168)
Mąka_Pszenna<-c(data[,3],data[,4],data[,5],data[,6],data[,7],data[,8],data[,9],data[,10],data[,11],data[,12],data[,13],data[,14],data[,15],data[,16])
Mięso_Wołowe_Bez_Kości<-c(data[,17],data[,18],data[,19],data[,20],data[,21],data[,22],data[,23],data[,24],data[,25],data[,26],data[,27],data[,28],data[,29],data[,30])
Mleko_Krowie_Spożywcze_O_Zawartości_Tłuszczu_Sterylizowane<-c(data[,31],data[,32],data[,33],data[,34],data[,35],data[,36],data[,37],data[,38],data[,39],data[,40],data[,41],data[,42],data[,43],data[,44])
Śmietana_O_Zawartości_Tłuszczu <-c(data[,45],data[,46],data[,47],data[,48],data[,49],data[,50],data[,51],data[,52],data[,53],data[,54],data[,55],data[,56],data[,57],data[,58])
Cukier_Biały_Kryształ<-c(data[,59],data[,60],data[,61],data[,62],data[,63],data[,64],data[,65],data[,66],data[,67],data[,68],data[,69],data[,70],data[,71],data[,72])
Papierosy<-c(data[,73],data[,74],data[,75],data[,76],data[,77],data[,78],data[,79],data[,80],data[,81],data[,82],data[,83],data[,84],data[,85],data[,86])
Centralne_Ogrzewanie_Lokali_Mieszkalnych <-c(data[,87],data[,88],data[,89],data[,90],data[,91],data[,92],data[,93],data[,94],data[,95],data[,96],data[,97],data[,98],data[,99],data[,100])
Olej_Napędowy<-c(data[,101],data[,102],data[,103],data[,104],data[,105],data[,106],data[,107],data[,108],data[,109],data[,110],data[,111],data[,112],data[,113],data[,114])
Bilet_Do_Kina<-c(data[,115],data[,116],data[,117],data[,118],data[,119],data[,120],data[,121],data[,122],data[,123],data[,124],data[,125],data[,126],data[,127],data[,128])
Gazeta_Regionalna<-c(data[,129],data[,130],data[,131],data[,132],data[,133],data[,134],data[,135],data[,136],data[,137],data[,138],data[,139],data[,140],data[,141],data[,142])

data[,2]<-as.character(data[,2])
woj<-rep(data[,2],14)
woj1<-rep(woj,12)
length(woj1)
rok<-rep(c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),each=17)
rok1<-rep(rok,12)
length(rok1)
mies<-rep(c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru"),each=238)
mon<-factor(mies, levels=c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru"), ordered = TRUE)

a=0
Mąka_Pszenna<-c(data[,a+3],data[,a+4],data[,a+5],data[,a+6],data[,a+7],data[,a+8],data[,a+9],data[,a+10],data[,a+11],data[,a+12],data[,a+13],data[,a+14],data[,a+15],data[,a+16])
Mięso_Wołowe_Bez_Kości<-c(data[,a+17],data[,a+18],data[,a+19],data[,a+20],data[,a+21],data[,a+22],data[,a+23],data[,a+24],data[,a+25],data[,a+26],data[,a+27],data[,a+28],data[,a+29],data[,a+30])
Mleko_Krowie_Spożywcze_O_Zawartości_Tłuszczu_Sterylizowane<-c(data[,a+31],data[,a+32],data[,a+33],data[,a+34],data[,a+35],data[,a+36],data[,a+37],data[,a+38],data[,a+39],data[,a+40],data[,a+41],data[,a+42],data[,a+43],data[,a+44])
Śmietana_O_Zawartości_Tłuszczu <-c(data[,a+45],data[,a+46],data[,a+47],data[,a+48],data[,a+49],data[,a+50],data[,a+51],data[,a+52],data[,a+53],data[,a+54],data[,a+55],data[,a+56],data[,a+57],data[,a+58])
Cukier_Biały_Kryształ<-c(data[,a+59],data[,a+60],data[,a+61],data[,a+62],data[,a+63],data[,a+64],data[,a+65],data[,a+66],data[,a+67],data[,a+68],data[,a+69],data[,a+70],data[,a+71],data[,a+72])
Papierosy<-c(data[,a+73],data[,a+74],data[,a+75],data[,a+76],data[,a+77],data[,a+78],data[,a+79],data[,a+80],data[,a+81],data[,a+82],data[,a+83],data[,a+84],data[,a+85],data[,a+86])
Centralne_Ogrzewanie_Lokali_Mieszkalnych <-c(data[,a+87],data[,a+88],data[,a+89],data[,a+90],data[,a+91],data[,a+92],data[,a+93],data[,a+94],data[,a+95],data[,a+96],data[,a+97],data[,a+98],data[,a+99],data[,a+100])
Olej_Napędowy<-c(data[,a+101],data[,a+102],data[,a+103],data[,a+104],data[,a+105],data[,a+106],data[,a+107],data[,a+108],data[,a+109],data[,a+110],data[,a+111],data[,a+112],data[,a+113],data[,a+114])
Bilet_Do_Kina<-c(data[,a+115],data[,a+116],data[,a+117],data[,a+118],data[,a+119],data[,a+120],data[,a+121],data[,a+122],data[,a+123],data[,a+124],data[,a+125],data[,a+126],data[,a+127],data[,a+128])
Gazeta_Regionalna<-c(data[,a+129],data[,a+130],data[,a+131],data[,a+132],data[,a+133],data[,a+134],data[,a+135],data[,a+136],data[,a+137],data[,a+138],data[,a+139],data[,a+140],data[,a+141],data[,a+142])

a=140
Mąka_Pszenna<-c(Mąka_Pszenna,data[,a+3],data[,a+4],data[,a+5],data[,a+6],data[,a+7],data[,a+8],data[,a+9],data[,a+10],data[,a+11],data[,a+12],data[,a+13],data[,a+14],data[,a+15],data[,a+16])
Mięso_Wołowe_Bez_Kości<-c(Mięso_Wołowe_Bez_Kości,data[,a+17],data[,a+18],data[,a+19],data[,a+20],data[,a+21],data[,a+22],data[,a+23],data[,a+24],data[,a+25],data[,a+26],data[,a+27],data[,a+28],data[,a+29],data[,a+30])
Mleko_Krowie_Spożywcze_O_Zawartości_Tłuszczu_Sterylizowane<-c(Mleko_Krowie_Spożywcze_O_Zawartości_Tłuszczu_Sterylizowane,data[,a+31],data[,a+32],data[,a+33],data[,a+34],data[,a+35],data[,a+36],data[,a+37],data[,a+38],data[,a+39],data[,a+40],data[,a+41],data[,a+42],data[,a+43],data[,a+44])
Śmietana_O_Zawartości_Tłuszczu <-c(Śmietana_O_Zawartości_Tłuszczu ,data[,a+45],data[,a+46],data[,a+47],data[,a+48],data[,a+49],data[,a+50],data[,a+51],data[,a+52],data[,a+53],data[,a+54],data[,a+55],data[,a+56],data[,a+57],data[,a+58])
Cukier_Biały_Kryształ<-c(Cukier_Biały_Kryształ,data[,a+59],data[,a+60],data[,a+61],data[,a+62],data[,a+63],data[,a+64],data[,a+65],data[,a+66],data[,a+67],data[,a+68],data[,a+69],data[,a+70],data[,a+71],data[,a+72])
Papierosy<-c(Papierosy,data[,a+73],data[,a+74],data[,a+75],data[,a+76],data[,a+77],data[,a+78],data[,a+79],data[,a+80],data[,a+81],data[,a+82],data[,a+83],data[,a+84],data[,a+85],data[,a+86])
Centralne_Ogrzewanie_Lokali_Mieszkalnych <-c(Centralne_Ogrzewanie_Lokali_Mieszkalnych ,data[,a+87],data[,a+88],data[,a+89],data[,a+90],data[,a+91],data[,a+92],data[,a+93],data[,a+94],data[,a+95],data[,a+96],data[,a+97],data[,a+98],data[,a+99],data[,a+100])
Olej_Napędowy<-c(Olej_Napędowy,data[,a+101],data[,a+102],data[,a+103],data[,a+104],data[,a+105],data[,a+106],data[,a+107],data[,a+108],data[,a+109],data[,a+110],data[,a+111],data[,a+112],data[,a+113],data[,a+114])
Bilet_Do_Kina<-c(Bilet_Do_Kina,data[,a+115],data[,a+116],data[,a+117],data[,a+118],data[,a+119],data[,a+120],data[,a+121],data[,a+122],data[,a+123],data[,a+124],data[,a+125],data[,a+126],data[,a+127],data[,a+128])
Gazeta_Regionalna<-c(Gazeta_Regionalna,data[,a+129],data[,a+130],data[,a+131],data[,a+132],data[,a+133],data[,a+134],data[,a+135],data[,a+136],data[,a+137],data[,a+138],data[,a+139],data[,a+140],data[,a+141],data[,a+142])


a=11*140
Mąka_Pszenna<-c(Mąka_Pszenna,data[,a+3],data[,a+4],data[,a+5],data[,a+6],data[,a+7],data[,a+8],data[,a+9],data[,a+10],data[,a+11],data[,a+12],data[,a+13],data[,a+14],data[,a+15],data[,a+16])
Mięso_Wołowe_Bez_Kości<-c(Mięso_Wołowe_Bez_Kości,data[,a+17],data[,a+18],data[,a+19],data[,a+20],data[,a+21],data[,a+22],data[,a+23],data[,a+24],data[,a+25],data[,a+26],data[,a+27],data[,a+28],data[,a+29],data[,a+30])
Mleko_Krowie_Spożywcze_O_Zawartości_Tłuszczu_Sterylizowane<-c(Mleko_Krowie_Spożywcze_O_Zawartości_Tłuszczu_Sterylizowane,data[,a+31],data[,a+32],data[,a+33],data[,a+34],data[,a+35],data[,a+36],data[,a+37],data[,a+38],data[,a+39],data[,a+40],data[,a+41],data[,a+42],data[,a+43],data[,a+44])
Śmietana_O_Zawartości_Tłuszczu <-c(Śmietana_O_Zawartości_Tłuszczu ,data[,a+45],data[,a+46],data[,a+47],data[,a+48],data[,a+49],data[,a+50],data[,a+51],data[,a+52],data[,a+53],data[,a+54],data[,a+55],data[,a+56],data[,a+57],data[,a+58])
Cukier_Biały_Kryształ<-c(Cukier_Biały_Kryształ,data[,a+59],data[,a+60],data[,a+61],data[,a+62],data[,a+63],data[,a+64],data[,a+65],data[,a+66],data[,a+67],data[,a+68],data[,a+69],data[,a+70],data[,a+71],data[,a+72])
Papierosy<-c(Papierosy,data[,a+73],data[,a+74],data[,a+75],data[,a+76],data[,a+77],data[,a+78],data[,a+79],data[,a+80],data[,a+81],data[,a+82],data[,a+83],data[,a+84],data[,a+85],data[,a+86])
Centralne_Ogrzewanie_Lokali_Mieszkalnych <-c(Centralne_Ogrzewanie_Lokali_Mieszkalnych ,data[,a+87],data[,a+88],data[,a+89],data[,a+90],data[,a+91],data[,a+92],data[,a+93],data[,a+94],data[,a+95],data[,a+96],data[,a+97],data[,a+98],data[,a+99],data[,a+100])
Olej_Napędowy<-c(Olej_Napędowy,data[,a+101],data[,a+102],data[,a+103],data[,a+104],data[,a+105],data[,a+106],data[,a+107],data[,a+108],data[,a+109],data[,a+110],data[,a+111],data[,a+112],data[,a+113],data[,a+114])
Bilet_Do_Kina<-c(Bilet_Do_Kina,data[,a+115],data[,a+116],data[,a+117],data[,a+118],data[,a+119],data[,a+120],data[,a+121],data[,a+122],data[,a+123],data[,a+124],data[,a+125],data[,a+126],data[,a+127],data[,a+128])
Gazeta_Regionalna<-c(Gazeta_Regionalna,data[,a+129],data[,a+130],data[,a+131],data[,a+132],data[,a+133],data[,a+134],data[,a+135],data[,a+136],data[,a+137],data[,a+138],data[,a+139],data[,a+140],data[,a+141],data[,a+142])


df<-data.frame(woj=woj1, rok=rok1, mies=mies, mon=mon, Mąka_Pszenna=Mąka_Pszenna, Mięso_Wołowe_Bez_Kości=Mięso_Wołowe_Bez_Kości,
                 Mleko_Krowie_Spożywcze_O_Zawartości_Tłuszczu_Sterylizowane=Mleko_Krowie_Spożywcze_O_Zawartości_Tłuszczu_Sterylizowane, Śmietana_O_Zawartości_Tłuszczu =Śmietana_O_Zawartości_Tłuszczu , Cukier_Biały_Kryształ=Cukier_Biały_Kryształ, Papierosy=Papierosy,
                 Centralne_Ogrzewanie_Lokali_Mieszkalnych =Centralne_Ogrzewanie_Lokali_Mieszkalnych , Olej_Napędowy=Olej_Napędowy, Bilet_Do_Kina=Bilet_Do_Kina, Gazeta_Regionalna=Gazeta_Regionalna)

#Zmieniamy NA na zero

df[is.na(df)] <- 0

#Robimy wykresy z ggplot

#Mąką_Pszenna

ggplot(data = df, 
       aes(x = rok, 
           y = Mąka_Pszenna)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Mąką pszenna: rok v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

ggplot(data = df, 
       aes(x = woj, 
           y = Mąka_Pszenna)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Mąka pszenna: województwa v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))


#Mięso wołowe bez kości

ggplot(data = df, 
       aes(x = rok, 
           y = Mięso_Wołowe_Bez_Kości)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Mięso wołowe bez kości: rok v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

ggplot(data = df, 
       aes(x = woj, 
           y = Mięso_Wołowe_Bez_Kości)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Mięso wołowe bez kości: województwa v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))


#Mleko krowie 3-3,5%

ggplot(data = df, 
       aes(x = rok, 
           y = Mleko_Krowie_Spożywcze_O_Zawartości_Tłuszczu_Sterylizowane)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Mleko krowie 3-3,5%: rok v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

ggplot(data = df, 
       aes(x = woj, 
           y = Mleko_Krowie_Spożywcze_O_Zawartości_Tłuszczu_Sterylizowane)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Mleko krowie 3-3,5%: województwa v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

#Śmietana 18%

ggplot(data = df, 
       aes(x = rok, 
           y = Śmietana_O_Zawartości_Tłuszczu)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Śmietana 18%: rok v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

ggplot(data = df, 
       aes(x = woj, 
           y = Śmietana_O_Zawartości_Tłuszczu)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Śmietana 18%: województwa v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))



#Cukier biały kryształ
ggplot(data = df, 
       aes(x = rok, 
           y = Cukier_Biały_Kryształ)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Cukier biały kryształ: rok v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

ggplot(data = df, 
       aes(x = woj, 
           y = Cukier_Biały_Kryształ)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Cukier biały kryształ: województwa v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))


#Papierosy 20 szt.

ggplot(data = df, 
       aes(x = rok, 
           y = Papierosy)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Papierosy 20 szt.: rok v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

ggplot(data = df, 
       aes(x = woj, 
           y = Papierosy)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Papierosy 20 szt.: województwa v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))



#Centralne ogrzewanie lokali mieszkalnych za 1 m2
ggplot(data = df, 
       aes(x = rok, 
           y = Centralne_Ogrzewanie_Lokali_Mieszkalnych)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Centralne ogrzewanie lokali mieszkalnych za 1 m2: rok v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

ggplot(data = df, 
       aes(x = woj, 
           y = Centralne_Ogrzewanie_Lokali_Mieszkalnych)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Centralne ogrzewanie lokali mieszkalnych za 1 m2: województwa v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))


#Olej napędowy
ggplot(data = df, 
       aes(x = rok, 
           y = Olej_Napędowy)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Olej napędowy: rok v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

ggplot(data = df, 
       aes(x = woj, 
           y = Olej_Napędowy)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Olej napędowy: województwa v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

#Bilet do kina
ggplot(data = df, 
       aes(x = rok, 
           y = Bilet_Do_Kina)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Bilet do kina: rok v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

ggplot(data = df, 
       aes(x = woj, 
           y = Bilet_Do_Kina)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Bilet do kina: województwa v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))


#Gazeta regionalna
ggplot(data = df, 
       aes(x = rok, 
           y = Gazeta_Regionalna)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Gazeta regionalna: rok v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

ggplot(data = df, 
       aes(x = woj, 
           y = Gazeta_Regionalna)) + 
  geom_point(color = 'blue', 
             size = 1, 
             shape = 15) +
  ggtitle("Gazeta regionalna: województwa v. ceny") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill = "white", colour = "grey50"))

View(df)
