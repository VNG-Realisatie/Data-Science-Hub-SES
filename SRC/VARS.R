
#-------------------------------------------------------------------------------
#DATA PREPARATION

#HIGH-ORDER OUTCOME VARIABLES (as proxy for vulnerability) : inclusion criteria
#
#GGEES203 : (zeer) ernstig eenzaam (dichotoom)
#GGRLS202 : onvoldoende regie over eigen leven (dichotoom)
#GGADS201 : (mid-)Matig of hoog risico op angststoornis of depressie (dichotoom)
#KLGGA207 ; (zeer) slechte gezondheid (dichotoom)
#score_zw : hoge samenloop (op onderstaande items, 4<) (will be created on-the-fly) 


#FEATURES 

#zinvol bestaan / betekenisvol sociaal netwerk 
#MMWSA205, MMWSA211 : werk / opleiding 
#MMVWB201 : vrijwilligerswerk 
#MCMZGS203 : mantelzorger 

#eenzaamheid 
#GGEEB201 : !Kan praten over dagelijkse problemen 
#GGEEB203 : Ervaar leegte 
#GGEEB204 : !mensen om op terug te vallen bij narigheid 
#GGEEB207 : !Veel mensen om op te vertrouwen 
#GGEEB208 : !Voldoende mensen waarmee verbondenheid is 

#GGEEB210 : Voel me vaak in de steekgelaten REMOVED
#GGEEB211 : !Kan bij vrienden terecht wanneer behoefte is REMOVED

#gezondheid en beperkingen
#CALGA260 : Heeft langdurige ziekte(n) of aandoening(en) 
#CALGA261 : Is (ernstig) beperkt in activiteiten vanwege gezondheid 
#MMIKB201 : moeite met rondkomen 

#LGBPS209 : Heeft minimaal een beperking met horen, zien of mobiliteit (ofwel minimaal grote moeite met 1 vd 7 OECD items) REMOVE
#LGBPS203 : beperking horen
#LGBPS204 : beperking zien
#LGBPS205 : beperking mobiliteit

#lifestyle/gedrag
#AGGWS205 : Obesitas, ofwel een BMI van 30 of hoger
#LFALA213 : zware drinker
#LFRKA205 : roker
# beweegnorm / fitnorm ADD? 

#angst en depressie
#GGADB201 : Vaak vermoeid zonder duidelijke reden 
#GGADB202 : Vaak zenuwachtig 
#GGADB204 : Vaak hopeloos 
#GGADB207 : Vaak somber of depressief
#GGADB210 : Vaak afkeurenswaardig, minderwaardig of waardeloos

#regie op het leven
#GGRLB201 : Weinig controle over dingen die mij overkomen
#GGRLB202 : Sommige van mijn problemen kan ik met geen mogelijkheid oplossen
#GGRLB204 : Ik voel me vaak hulpeloos bij omgaan problemen van het leven
#GGRLB206 : Wat er in de toekomst met me gebeurt hangt voor grootste deel van mezelf af
#MCMZOS304 : mantelzorg ontvangen

#vitaliteit en veerkracht ADD?
#levenstevredenheid en geluk ADD?


#create new recoded variables
#ervarengezondheid_dich 1 '(zeer) slecht' 0 'matig tot (zeer) goed)'
SOURCE_RAW$ervarengezondheid_dich = recode(SOURCE_RAW$KLGGA207, "3=1; 1=0; 2=0; 9=NA")

#regie_dich 1 'onvoldoende regie' 0 'matig of veel regie'
SOURCE_RAW$regie_dich = recode(SOURCE_RAW$GGRLS203, "0=1; 1=0; 9=NA")

#angstdepressie_dich 1 '(zeer) hoog' 0 'niet of nauwelijks'
SOURCE_RAW$angstdepressie_dich <-0
SOURCE_RAW$angstdepressie_dich[SOURCE_RAW$GGADS201>22] <-1
SOURCE_RAW$angstdepressie_dich[SOURCE_RAW$GGADS201==9] <- NA

#eenzaamheid_dich 1 '(zeer) ernstig eenzaam' 0 'niet of matig eenzaam'
SOURCE_RAW$eenzaamheid_dich = recode(SOURCE_RAW$GGEES209, "1=1; 0=0; 8=0; 9=NA")

#werkopleiding 'betaald werk, student, vrijwilliger' 
SOURCE_RAW$werkopleiding <- 0
SOURCE_RAW$werkopleiding[SOURCE_RAW$MMWSA205==1 | SOURCE_RAW$MMWSA211==1] <- 1
SOURCE_RAW$werkopleiding[SOURCE_RAW$MMWSA205==9 & SOURCE_RAW$MMWSA211==9] <- NA

#vrijwilligerswerk NEW
SOURCE_RAW$vrijwilligerswerk <-0
SOURCE_RAW$vrijwilligerswerk[SOURCE_RAW$MMVWB201==1] <-1
SOURCE_RAW$vrijwilligerswerk[SOURCE_RAW$MMVWB201==9] <-NA

#mantelzorger NEW
SOURCE_RAW$mantelzorg[SOURCE_RAW$MCMZGS203==0] <-0
SOURCE_RAW$mantelzorg[SOURCE_RAW$MCMZGS203==1] <-1
SOURCE_RAW$mantelzorg[SOURCE_RAW$MCMZGS203==9] <-NA

#vriendenkring NEW
SOURCE_RAW$vriendenkring <-0
SOURCE_RAW$vriendenkring[SOURCE_RAW$GGEEB206==2 | SOURCE_RAW$GGEEB206==3] <-1
SOURCE_RAW$vriendenkring[SOURCE_RAW$GGEEB206==9] <-NA

#goedevriend NEW
SOURCE_RAW$goedevriend <-0
SOURCE_RAW$goedevriend[SOURCE_RAW$GGEEB202==2 | SOURCE_RAW$GGEEB202==3] <-1
SOURCE_RAW$goedevriend[SOURCE_RAW$GGEEB202==9] <-NA

#mensen om me heen NEW
SOURCE_RAW$mensenomheen <-0
SOURCE_RAW$mensenomheen[SOURCE_RAW$GGEEB209==2 | SOURCE_RAW$GGEEB209==3] <-1
SOURCE_RAW$mensenomheen[SOURCE_RAW$GGEEB209==9] <-NA

#inkkwin_2016 'Gestandaardiseerd huishoudinkomen in kwintielen (numerieke variabele)'
#inkkwin_2016 1 '0 tot 20% (max 16.100 euro)' 2 '20 tot 40% (max 21.300 euro)' 3 '40 tot 60% (max 27.200 euro)'
#4 '60 tot 80% (max 35.100 euro)' 5 '80 tot 100% (> 35.100 euro)' 9 'onbekend'.

#inkomenzeerlaag_dich 1 'zeer laag, max 16.100 euro' 0 'hoger'
#SOURCE_RAW$inkomenzeerlaag_dich = recode(SOURCE_RAW$inkkwin_2016, "1=1; 2=0; 3=0; 4=0; 5=0; 9=NA")
#SOURCE_RAW$inkomenzeerlaag_dich = recode(SOURCE_RAW$KwintielInk, "1=1; 2=0; 3=0; 4=0; 5=0; 9=NA")

#inkomenlaag_dich 1 'laag, max 21.300 euro' 0 'hoger'
#SOURCE_RAW$inkomenlaag_dich = recode(SOURCE_RAW$inkkwin_2016, "1=1; 2=1; 3=0; 4=0; 5=0; 9=NA")
#SOURCE_RAW$inkomenlaag_dich = recode(SOURCE_RAW$KwintielInk, "1=1; 2=1; 3=0; 4=0; 5=0; 9=NA")

#leeftijdcat6  'leeftijd in 6 categorieen obv geboortedatum' 
SOURCE_RAW$leeftijdcat6 = recode(SOURCE_RAW$Lftklassen, "1=1; 2:4=2; 5:7=3; 8:9=4; 10:11=5; 12:14=6; 99=NA")

#leeftijd70eo 'leeftijd 70 jaar of ouder'
SOURCE_RAW$leeftijd70eo = recode(SOURCE_RAW$Lftklassen, "11=1; 12=1; 13=1; 14=1; 99=NA; else=0")

#opl_lm opleiding laag midden 1
#SOURCE_RAW$opl_lm = recode(SOURCE_RAW$opl_dichVM, "0=0; 1=1; 9=NA")
SOURCE_RAW$opl_lm = recode(SOURCE_RAW$Opleiding_samind, "1=1; 2=1; 3=0; 4=0; 9=NA")

#ziek_lt langdurige ziekten
SOURCE_RAW$ziek_lt = recode(SOURCE_RAW$CALGB260, "1=1; 2=0; 9=NA")

#depri_hg matig of hoog risico op angst en depressie
SOURCE_RAW$depri_hg = recode(SOURCE_RAW$GGADA202, "0=0; 1=1; 9=NA")


#NORMALIZE DATA
#Recoding -where needed- into new dichotomous variable with the same direction of the loadings (0=NO ISSUE)
SOURCE_RAW$werkopleiding_dich = recode(SOURCE_RAW$werkopleiding, "1=0; 0=1;")

SOURCE_RAW$vrijwilligerswerk_dich = recode(SOURCE_RAW$vrijwilligerswerk, "1=0; 0=1;")

#okay, mz is not a social activity (but is a meaningful social relation)
SOURCE_RAW$mantelzorg_dich = recode(SOURCE_RAW$mantelzorg, "1=0; 0=1;")

SOURCE_RAW$vriendenkring_dich = recode(SOURCE_RAW$vriendenkring, "1=0; 0=1;")

SOURCE_RAW$goedevriend_dich = recode(SOURCE_RAW$goedevriend, "1=0; 0=1;")

SOURCE_RAW$mensenomheen_dich = recode(SOURCE_RAW$mensenomheen, "1=0; 0=1;")

SOURCE_RAW$MMIKB201_dich = recode(SOURCE_RAW$MMIKB201, "4=1; 3=0; 2=0; 1=0")

SOURCE_RAW$GGEEB201_dich = recode(SOURCE_RAW$GGEEB201, "3=1; 2=0; 1=0")

SOURCE_RAW$GGEEB203_dich = recode(SOURCE_RAW$GGEEB203, "1=1; 2=0; 3=0")

SOURCE_RAW$GGEEB204_dich = recode(SOURCE_RAW$GGEEB204, "3=1; 2=0; 1=0")

SOURCE_RAW$GGEEB207_dich = recode(SOURCE_RAW$GGEEB207, "3=1; 2=0; 1=0")

SOURCE_RAW$GGEEB208_dich = recode(SOURCE_RAW$GGEEB208, "3=1; 2=0; 1=0")

SOURCE_RAW$GGEEB210_dich = recode(SOURCE_RAW$GGEEB210, "1=1; 2=0; 3=0")

SOURCE_RAW$GGEEB211_dich = recode(SOURCE_RAW$GGEEB211, "3=1; 2=0; 1=0")

SOURCE_RAW$GGRLB201_dich = recode(SOURCE_RAW$GGRLB201, "1=1; 2=0; 3=0; 4=0; 5=0")

SOURCE_RAW$GGRLB202_dich = recode(SOURCE_RAW$GGRLB202, "1=1; 2=0; 3=0; 4=0; 5=0")

SOURCE_RAW$GGRLB204_dich = recode(SOURCE_RAW$GGRLB204, "1=1; 2=0; 3=0; 4=0; 5=0")

SOURCE_RAW$GGRLB206_dich = recode(SOURCE_RAW$GGRLB206, "1=1; 2=0; 3=0; 4=0; 5=0")

SOURCE_RAW$GGADB201_dich = recode(SOURCE_RAW$GGADB201, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_RAW$GGADB202_dich = recode(SOURCE_RAW$GGADB202, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_RAW$GGADB204_dich = recode(SOURCE_RAW$GGADB204, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_RAW$GGADB207_dich = recode(SOURCE_RAW$GGADB207, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_RAW$GGADB210_dich = recode(SOURCE_RAW$GGADB210, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_RAW$MCMZOS304_dich = recode(SOURCE_RAW$MCMZOS304, "0=0; 1=1; 9=NA")
