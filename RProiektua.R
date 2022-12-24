# ======================================================
#
#                           Lehenengo 
#                             Zatia
#
# ======================================================


# A azpiatala 
#---------------------------------------------------------------------------------------------------------------
# Testu ingurua: 

# Hiru dado botako ditugu eta X ≡‘hiru dadoen batura’ hartuko dugu.
# Datuak aztertu eta hurrengo konklusiora ailegatu gara:

# 3 dado bota eta gero haien harteko baturen konbinazio posibleak guztiak haue dira: 6*6*6 = 6^3 = 216

# Jakinda konbinazio posible guztiak goazen haien distribuzioa lortzera: 

num = 0
dadoKasuenBektorea = c(replicate(18,0))
for(dado1 in 1:6)
{
  for(dado2 in 1:6)
  {
    for(dado3 in 1:6)
    {
      num = dado1 + dado2 + dado3
      dadoKasuenBektorea[num] = dadoKasuenBektorea[num] + 1 
    }
  }
}

probabilitateenBektorea = c(replicate(18,0))
probabilitateenBektorea = dadoKasuenBektorea/216
laginekoElementuKopurua = length(probabilitateenBektorea)
# Azalpena: bektorean zenbaki bakoitza ateratzeko probabilitatea gordeko da baina taula ulergarria izan dadin bakarrik konbinazio posibleak adierazten dira
# Lortutako konbinazio posibleak: ---------------------------------------------
  #  ----------
  #  | 1 |  0 |
  #  | --------
  #  | 2 |  0 |
  #  | --------
  #  | 3 |  1 |
  #  | --------
  #  | 4 |  3 |
  #  | --------
  #  | 5 |  6 |
  #  | --------
  #  | 6 |  10 |
  #  | --------
  #  | 7 |  15 |
  #  | --------
  #  | 8 |  21 |
  #  | --------
  #  | 9 |  25 |
  #  | --------
  #  | 10 |  27 |
  #  | --------
  #  | 11 |  27 |
  #  | --------
  #  | 12 |  25 |
  #  | --------
  #  | 13 |  21 |
  #  | --------
  #  | 14 |  15 |
  #  | --------
  #  | 15 |  10 |
  #  | --------
  #  | 16 |  6 |
  #  | --------
  #  | 17 |  3 |
  #  | --------
  #  | 18 |  1 |
  #  | --------
#-----------------------------------------------------------------------------

# Ikus dezakegu gure datuak banaketa normala jarraitzen dutela. 
# Zentzuzkoa baita, izan ere, zorizko aldagaien arteko batura azko egiten hari garelako.

# Irudikatu barra diagrama 
barplot(probabilitateenBektorea,
        main="1.ariketa",
        xlab="Gehiketa",
        ylab="Probabilitatea",
        col="#DF460C",
        border="#DF460C")


# sum() funtzioarekin errore bat dela eta aukeratuko dugu for baten bidez egitea.
itxaropena = 0 
for(i in 1:18)
{
  itxaropena = itxaropena + (i*probabilitateenBektorea[i])
}

# Bariantza kalkulatuko dugu:
# R-k kuasibariantza kalkulatzen duenez eskuz edo kodearen bidez kalkulatuko dugu.

bariantza = 0
batazBestekoa = mean(probabilitateenBektorea)
# kasu honetan sum funtzioak ez du errorerik ematen hori dela eta erbiliko dugu
bariantza = (1/laginekoElementuKopurua * (sum(probabilitateenBektorea^2))) - batazBestekoa^2




# B azpiatala 
#---------------------------------------------------------------------------------------------------------------
# Testu ingurua: 

# X zorizko aldagaiaren balioak simulatuko dituzu eta bertatik ateratako zorizko
# laginean eraikitako X¯ estatistikoaren lagin-banaketa nolakoa den aztertu.

# Ondoren, 3 dado n aldiz jakin batzuen jaurtiketa egingo dugu eta esperimentu bakoitzean lortutako emaitzen batez bestekoa kalkulatuko dugu. 
# Esperimentu hau 5 aldiz errepikatuko da, n: 10, 100 eta 1000rako balio desberdinak erabiliz.
# Esperimentu bakoitzaren datuak gordetzeko, 5 posizioko bektore bat erabiliko dugu, bat esperimentuaren errepikapen bakoitzerako. 
# Ondoren, datuak eta horiek ordezkatzen dituen grafiko bat erakutsiko dira.


# Simulaziekin hasi baino lehen argituko dugu runif() funtzioak 1 ,6 balioek sortuko dituela baina ez dira zehatzak izango, hau da,
# komarekin adieraziko dituela, hori dela eta, round() funtzioaren bidez borodilduko ditugu.





# --------------------------------------------- N = 10 ---------------------------------------------


n = 10
dadoSimulazio10 = c()


for(i in 1:5)
{
  laginenBatura = 0
  # 3 dadoen banaketa uniformeen batura
  for(j in 1:3)
  {
    lagina = runif(n, min = 1, max = 6)
    laginaBorobilduta = round(lagina)
    laginenBatura = laginenBatura + laginaBorobilduta
  }
  laginenBatazBestekoa = mean(laginenBatura)
  dadoSimulazio10[i] = laginenBatazBestekoa
}

# --------------------------------------------- N = 100 ---------------------------------------------


n = 100
dadoSimulazio100 = c()


for(i in 1:5)
{
  laginenBatura = 0
  # 3 dadoen banaketa uniformeen batura
  for(j in 1:3)
  {
    lagina = runif(n, min = 1, max = 6)
    laginaBorobilduta = round(lagina)
    laginenBatura = laginenBatura + laginaBorobilduta
  }
  laginenBatazBestekoa = mean(laginenBatura)
  dadoSimulazio100[i] = laginenBatazBestekoa
}


# --------------------------------------------- N = 1000 ---------------------------------------------


n = 1000
dadoSimulazio1000 = c()


for(i in 1:5)
{
  laginenBatura = 0
  # 3 dadoen banaketa uniformeen batura
  for(j in 1:3)
  {
    lagina = runif(n, min = 1, max = 6)
    laginaBorobilduta = round(lagina)
    laginenBatura = laginenBatura + laginaBorobilduta
  }
  laginenBatazBestekoa = mean(laginenBatura)
  dadoSimulazio1000[i] = laginenBatazBestekoa
}


# Hiru bektoreen emaitzak erakutziko ditugu barra diagrama batean haien portaera aztertzeko

par(mfrow=c(1,3))

barplot(dadoSimulazio10,
        main="1. Zatia B Atala n=10",
        xlab="Laginak",
        ylab="Bataz-bestekoa",
        col="#0C9ED8",
        border="#0C9ED8")


barplot(dadoSimulazio100,
        main="1. Zatia B Atala n=100",
        xlab="Laginak",
        ylab="Bataz-bestekoa",
        col="#DB188E",
        border="#DB188E")


barplot(dadoSimulazio1000,
        main="1. Zatia B Atala n=1000",
        xlab="Laginak",
        ylab="Bataz-bestekoa",
        col="#24D310",
        border="#24D310")

par(mfrow=c(1,1))

dadoSimulazio10
dadoSimulazio100
dadoSimulazio1000


# Grafikoei eta datuen portaerari erreparatuz gero, datu gehiago ditugun heinean, aurreko atalean kalkulatutako itxaropenaren antzeko balioak lortzen ditugu (10.5). 
# Horrek adierazten du laginetatik lortutako datuak fidagarriak direla.


# X-ren banaketa teorikoa kalkulatzeko, hiru dado 100 aldiz jaurtitzeko eta haren batez bestekoa 1000 aldiz kalkulatzeko esperimentua errepikatuko dugu. 
# Horretarako, batezbesteko bakoitza 1000 elementuko bektore batean gordeko dugu, bat esperimentu bakoitzeko. 
# Ondoren, banaketa histograma baten eta haren "itxaropena" markatuko duen lerro baten bidez erakutsiko dugu.

K = 1000
n = 100
dadoLagina1000 = c()
for(i in 1:K)
{
  laginenBatura = 0 
  for(j in 1:3)
  {
    lagina = runif(n, min = 1, max = 6)
    laginaBorobilduta = round(lagina)
    laginenBatura = laginenBatura + laginaBorobilduta
  }
  laginarenBatazBestekoa = mean(laginenBatura)
  dadoLagina1000[i] = laginarenBatazBestekoa
}

# histograma irudikatu 

hist(dadoLagina1000, 
        main=" 1. Zatia B atala K = 1000",
        xlab= "Laginak",
        ylab= "Batazbestekoak",
        col= "#11BBC3",
        border = "#ffffff")


# itxaropen teorikoa irudikatu

abline(v = mean(dadoLagina1000), col = "red" )


