#Dies ist eine Testdatei des Paketes OSE sie soll zeigen, dass alle Funktionen wie gewünscht funktionieren. 
#Da die diversen Plotfunktionen nicht mit dem Paket "testthat" geprüft werden können, soll dieses file vor allem zu diesem Zweck dienen.

#########################################################################################################
library(OSE)
#Definieren zunächst ein paar Wahrscheinlichkeitsdichten auf dem Intervall [0,1] 
set.seed(100)
dens1 <- function(x) 1 + 0.5 * sin(2 * pi * x)
dens2 <- function(x) dunif(x)
dens3 <- function(x) dnorm(x, 0.5, 0.088)
#Check um zu prüfen, ob diese tatsächlich Dichten auf [0, 1] sind
integrate(dens1, 0, 1)$value
integrate(dens2, 0, 1)$value
integrate(dens3, 0, 1)$value



############Tests mit der trigonometrischen Basis###################

#Erstellen einer trigonometrischen Basis mit genügend hoher Dimension
trig_bas <- Trig_Basis$new(10000)
#Jetzt kann man sich eine beliebige Basisfunktion anzeigen lassen
trig_bas$get_function(4)

#Die plot_basis()-Funktion.
#Mit dieser kann man sich die Basis-Funktionen zeichnen lassen. 
plot_basis(trig_bas, 2)
plot_basis(trig_bas, seq(3))
plot_basis(trig_bas, c(10, 12, 14))

#Die rdens()-Funktion
#Mit dieser kann eine Stichprobe von Daten einer gegebenen Dichte generiert werden
data1 <- rdens(dens1, 100)
data2 <- rdens(dens2, 100)
data3 <- rdens(dens3, 150)
data1 
data2
data3

#Die est_dens()-Funktion
#Diese Funktion implementiert die eindimensionale OSE Schätzung
oneest1 <- est_dens(trig_bas, data1, 10)
oneest2 <- est_dens(trig_bas, data2, 10) 
oneest3 <- est_dens(trig_bas, data3, 10)
#10 wurde hierbei eher random gewählt

#Die plot_dens()-Funktion
#Diese Funktion plottet für gegebene Daten die geschätzte Dichte unter Verwendung von est_dens()
p1 <- plot_dens(trig_bas, data1, 10) #Schätzung für dens1 und Dimension 10
p2 <- plot_dens(trig_bas, data2, 10) #Schätzung für dens2 und Dimension 10 
p3 <- plot_dens(trig_bas, data3, 10) #Schätzung für dens3 und Dimension 10
#Diese Funktion erlaubt es uns auch die geschätzte Dichte mit geschätzen Dichte der model selction und der wahren Dichte (sofern bekannt)
#zu vergleichen. Zum genaueren Verständnis der Funktion, kannn die Dokumentation zurate gezogen werden.
#ACHTUNG dies kann, je nach Computerleistung,  ein bisschen dauern.
p4 <- plot_dens(trig_bas, data1, 5, dens = dens1, compare = TRUE)
p5 <- plot_dens(trig_bas, data2, 5, dens = dens2, compare = TRUE) #Hier kann die model selection teilweise sogar die Dichte genau schätzen
p6 <- plot_dens(trig_bas, data3, 3, dens = dens3, compare = TRUE) 
p1
p2
p3
p4
p5 
p6

#Die perfect_D()-Funktion
#Diese Funktion gibt den adaptive estimator zurück, also einen relevanten Wert für die Dimension $D$ der nur auf den Daten basiert
perfect1 <- perfect_D(trig_bas, data1) 
perfect2 <- perfect_D(trig_bas, data2) 
perfect3 <- perfect_D(trig_bas, data3)
perfect1
perfect2
perfect3

#Die est_dens2()-Funktion
#Diese Funktion gibt die OSE Schätzung für die bivariate Dichte zurück
#Als Dimensionsargumente verwenden wir hier beispielweise gerade diese die von perfect_D() zurückgegeben werden
twodest1 <- est_dens2(trig_bas, data = data.frame(data1, data1), dim = c(perfect1, perfect1))  
twodest2 <- est_dens2(trig_bas, data = data.frame(data2, data2), dim = c(perfect2, perfect2))
twodest3 <- est_dens2(trig_bas, data = data.frame(data3, data3), dim = c(perfect3, perfect3))

#Die plot_dens2()-Funktion
#Diese Funktion plottet für gegebene Daten die geschätzte bivariate Dichte unter verwendung von est_dens2()
#ACHTUNG: Die Ausführung dieses Codes braucht ein wenig
p7 <- plot_dens2(trig_bas, data = data.frame(data1, data1), dim = c(perfect1, perfect1))
p8 <- plot_dens2(trig_bas, data = data.frame(data2, data2), dim = c(perfect2, perfect2))
p9 <- plot_dens2(trig_bas, data = data.frame(data3, data3), dim = c(perfect3, perfect3))
p7
p8
p9

#Die datsim()-Funktion
#Diese Funktion gibt einen Dataframe von Paaren von (x, y). Die x werden mit einer gegebenen Dichte jeweils von rdens() erzeugt.
#die y genügen der Gleichung y = m(x) + e, wobei m eine Regressionfunktion ist, und e ein nomalv. Fehlerterm
datasim1 <- datsim(dens1, log, 150)
datasim2 <- datsim(dens2, log, 150)
datasim3 <- datsim(dens3, log, 150)
datasim1
datasim2
datasim3

#Die reg_est()-Funktion
#Diese Funktion gibt einen Schätzer für die Regressionsfunktion m im Problem Y_i = m(X_i) + \varepsilon_i zurück
#ACHTUNG: Die Ausführung dieses Codes braucht ein wenig
regm1 <- reg_est(trig_bas, trig_bas, datasim1)
regm2 <- reg_est(trig_bas, trig_bas, datasim2) 
regm3 <- reg_est(trig_bas, trig_bas, datasim3)

#Die plot_reg()-Funktion
#Diese Funktion plottet den Schätzer für die Regressionsfunktion unter Verwendung von reg_est()
#Es werden automatisch die Datenpunkte dir zur Schätzung benutzt werden, als Scatterplot mit eingefügt
#ACHTUNG: Die Ausführung des folgenden Codes braucht ein wenig
p10 <- plot_reg(trig_bas, trig_bas, datasim1)
p11 <- plot_reg(trig_bas, trig_bas, datasim2)
p12 <- plot_reg(trig_bas, trig_bas, datasim3)
#Über den optionalen Parameter f, kann man eine Vergleichsfunktion übergeben.
p13 <- plot_reg(trig_bas, trig_bas, datasim1, f =  log)
p14 <- plot_reg(trig_bas, trig_bas, datasim2, f =  log)
p15 <- plot_reg(trig_bas, trig_bas, datasim3, f =  log)
p10
p11
p12
p13
p14
p15


########################## Die Histogrammbasis #################################################
# Basis erstellen
hist_bas_trial <- Hist_Basis$new(5)

#Jetzt kann man sich eine beliebige Basisfunktion anzeigen lassen
hist_bas_trial$get_function(4)

#Die plot_basis()-Funktion.
#Mit dieser kann man sich die Basis-Funktionen zeichnen lassen. 
plot_basis(hist_bas_trial, 2)
plot_basis(hist_bas_trial, seq(3))

#Erstelle Basis von genügend hoher Dimension
hist_bas <- Hist_Basis$new(15)

#Datengenerierung
data4 <- rdens(dens1, 15)
data5 <- rdens(dens2, 15)
data6 <- rdens(dens3, 15)
data4 
data5
data6

#Die est_dens()-Funktion
#Diese Funktion implementiert die eindimensionale OSE Schätzung
oneest4 <- est_dens(hist_bas, data4, 13)
oneest5 <- est_dens(hist_bas, data5, 7) 
oneest6 <- est_dens(hist_bas, data6, 13)
#10 wurde hierbei eher random gewählt

#Die plot_dens()-Funktion
#für die Histogrammbasis ist es selbsverständlich schwierig dens1 und dens2 zu schätzen
#dens3 (Normalverteilung) funktioniert da schon besser
p16 <- plot_dens(hist_bas, data4, 13) #Schätzung für dens1 und Dimension 13
p17 <- plot_dens(hist_bas, data5, 7) #Schätzung für dens2 und Dimension 7
p18 <- plot_dens(hist_bas, data6, 13) #Schätzung für dens3 und Dimension 13
#Diese Funktion erlaubt es uns auch die geschätzte Dichte mit geschätzen Dichte der model selction und der wahren Dichte (sofern bekannt)
#zu vergleichen. ACHTUNG dies kann, je nach computerleistung,  ein bisschen dauern.
p19 <- plot_dens(hist_bas, data4, 3, dens = dens1, compare = TRUE)
p20 <- plot_dens(hist_bas, data5, 3, dens = dens2, compare = TRUE)
p21 <- plot_dens(hist_bas, data6, 3, dens = dens3, compare = TRUE) 
p16
p17
p18
p19
p20
p21

#Die perfect_D()-Funktion
#Diese Funktion gibt den adaptive estimator zurück, also einen relevanten Wert für die Dimension $D$ der nur auf den Daten basiert
perfect4 <- perfect_D(hist_bas, data4) 
perfect5 <- perfect_D(hist_bas, data5) 
perfect6 <- perfect_D(hist_bas, data6)
perfect4
perfect5
perfect6

#Die est_dens2()-Funktion
twodest4 <- est_dens2(hist_bas, data = data.frame(data4, data4), dim = c(perfect4, perfect4))  
twodest5 <- est_dens2(hist_bas, data = data.frame(data5, data5), dim = c(perfect5, perfect5))
twodest6 <- est_dens2(hist_bas, data = data.frame(data6, data6), dim = c(perfect6, perfect6))

#Die plot_dens2()-Funktion
#ACHTUNG die Ausführung dieses Codes braucht recht lange!
#Die bivariaten Dichten von dens1 und dens2 sind für die Histogrammbasis sehr schwer zu realisieren. 
#Wir stellen deshalb nur die Schätzung der bivariaten Normalverteilung vor
p22 <- plot_dens2(hist_bas, data = data.frame(data6, data6), dim = c(perfect6, perfect6))
p22

#simulating data
datasim4 <- datsim(dens1, log, 15)
datasim5 <- datsim(dens2, log, 15)
datasim6 <- datsim(dens3, log, 15)
datasim4
datasim5
datasim6

#Die reg_est()-Funktion
regm1 <- reg_est(hist_bas, hist_bas, datasim4)
regm2 <- reg_est(hist_bas, hist_bas, datasim5) 
regm3 <- reg_est(hist_bas, hist_bas, datasim6)

#Die plot_reg()-Funktion
p23 <- plot_reg(hist_bas, hist_bas, datasim4)
p24 <- plot_reg(hist_bas, hist_bas, datasim5)
p25 <- plot_reg(hist_bas, hist_bas, datasim6)
#Der optionaler Parameter f, ermöglicht es uns eine Vergleichsfunktion zu übergeben.
p26 <- plot_reg(hist_bas, hist_bas, datasim4, f =  log)
p27 <- plot_reg(hist_bas, hist_bas, datasim5, f =  log)
p28 <- plot_reg(hist_bas, hist_bas, datasim6, f =  log)
p23
p24
p25
p26
p27
p28