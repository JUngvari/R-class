RclassIndependent
========================================================
author: Judit Ungvari-Martin  
date: April 24, 2015

Descriptive Statistics of Sampling Birds in Two Forest Types
========================================================

Introduction of disseration is going to include basic information from my fieldwork.

- Western Amazonia

![map](PeruGISArcdesktop.jpg)

Localities & Background
========================================================
- 22 months in the field
- mark/recapture of birds 
- 2 different habitat types

*******

![closer](GisArcdesktop.jpg)



Packages Used
========================================================


```r
library("knitr")
library("gdata")
library("vegan")
library("bipartite")
library("betapart")
library("reshape")
library("labdsv")
library("dplyr")
library("maps")
library("mapdata")
source("~/Dropbox/R resources/codes/pois_aov.R")
library("vegan")
library("maptools")
library("RColorBrewer")
library("mapplots")
library("maps")
library("mapdata")
library("scales")
```

Dataset
========================================================
- basically the raw data straight from the field notes

```r
complete = read.csv(file="data/complete.csv", header=TRUE)
(dim(complete))
```

```
[1] 8469   64
```

```r
names(complete)
```

```
 [1] "Net"              "Regist"           "anillador"       
 [4] "Codigo"           "No_anillo"        "No_anillo."      
 [7] "Anillo.color"     "Codigo.especie"   "FAMILY"          
[10] "Genus"            "species"          "spp"             
[13] "Edad"             "Codigo.fechado"   "SEXO..M.H.D."    
[16] "Codigo.Sexado"    "SEX.MOLEC"        "MUSCULO"         
[19] "PROCLO"           "PARINC"           "GRASA"           
[22] "MUDA.CORP"        "MUDA.VUEL"        "MUDA.COLA"       
[25] "DESG.VUEL"        "PLUMJUV"          "ALA"             
[28] "COLA"             "CULMEN"           "APERTURA"        
[31] "TARSO"            "PESO"             "COJO"            
[34] "CPIEL"            "CPATAS"           "ECTOPARASITO"    
[37] "SANGRE"           "H1"               "H2"              
[40] "H3"               "H4"               "RED"             
[43] "FECHA"            "Year.x"           "ECTO.ROJO"       
[46] "Dondectos"        "Cuantosectos"     "OBSERVACIONES"   
[49] "CREST"            "Specific.Habitat" "general.habitat" 
[52] "Field.Site"       "REGION"           "PATCH.SIZE"      
[55] "Netline"          "NetlineYEAR"      "Foragero"        
[58] "SAMPLE.NUMBER"    "Categories"       "lon"             
[61] "lat"              "ele"              "Net.number"      
[64] "Year.y"          
```

Dataset cleanup
========================================================



```r
complete = complete[!is.na(complete$Regist),] #removes 77 unused lat long NA rows
dim(complete)
```

```
[1] 8392   64
```

```r
summary(complete$lon)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -73.56  -73.50  -73.44  -73.47  -73.43  -73.41 
```

```r
summary(complete$lat) #use the min and max values for map making
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -3.992  -3.950  -3.915  -3.926  -3.899  -3.876 
```

First glance
========================================================
- Make a list of all the species and sort them

```r
# Species rank curve dataset
(allsp = sort(table(complete$spp), decreasing = TRUE))
```

```

     Glyphorynchus spirurus              Dixiphia pipra 
                       1570                         669 
    Hylophylax poecilonotus            Pithys albifrons 
                        551                         456 
      Gymnopithys leucaspis        Mionectes oleagineus 
                        452                         401 
        Dendrocincla merula      Myrmotherula axillaris 
                        257                         219 
     Myrmotherula hauxwelli     Neopelma chrysocephalum 
                        198                         191 
       Lepidothrix coronata       Xiphorhynchus elegans 
                        189                         186 
   Megastictus margaritatus        Phaetornis bourcieri 
                        174                         173 
   Phaetornis superciliosus      Myrmoborus myotherinus 
                        170                         147 
      Automolus ochrolaemus        Pipra erythrocephala 
                        122                          99 
    Dendrocincla fuliginosa  Epinecrophylla haematonota 
                         96                          89 
        Schiffornis turdina            Catharus minimus 
                         82                          81 
             Xenops minutus        Corythopis torquatus 
                         76                          74 
        Thamnomanes caesius    Microcerculus marginatus 
                         72                          64 
         Thalurania furcata             Manacus manacus 
                         64                          59 
        Synallaxis rutilans   Terenotriccus  erythrurus 
                         57                          57 
          Formicarius colma        Percnostola arenarum 
                         54                          51 
      Sclerurus rufigularis          Hylophylax naevius 
                         51                          50 
          Geotrygon montana             Nonnula brunnea 
                         46                          46 
        Sclerurus mexicanus      Machaeropterus regulus 
                         42                          40 
          Malacoptila fusca          Myrmeciza castanea 
                         40                          38 
    Phlegopsis erythroptera          Threnetes leucurus 
                         37                          36 
           Myrmeciza fortis     Percnostola leucostigma 
                         35                          31 
         Catharus ustulatus        Thamnophilus murinus 
                         29                          28 
          Cnemotriccus spp.       Hyloctistes subulatus 
                         26                          25 
      Hypocnemis hypoxantha            Attila spadiceus 
                         25                          23 
    Hylophilus ochraceiceps          Chloroceryle aenea 
                         22                          21 
 Rhegmatorhina melanosticta           Turdus albicollis 
                         20                          20 
        Heliodoxa aurescens     Cnipodectes subbrunneus 
                         19                          17 
      Conopophaga peruviana      Thamnomanes ardesiacus 
                         16                          16 
     Dendrocolaptes certhia     Ramphotrigon  ruficauda 
                         14                          14 
    Deconychura stictolaema      Platyrinchus saturatus 
                         13                          13 
      Cyanocompsa cyanoides     Frederickena unduligera 
                         12                          12 
        Galbula albirostris              Celeus elegans 
                         12                          11 
        Florisuga mellivora Heterocercus aurantiivertex 
                         11                          11 
      Tachyphonus surinamus        Hypocnemis peruviana 
                         11                          10 
         Neopipo cinnamomea   Onychorhynchus  coronatus 
                         10                          10 
         Thryothorus coraya            Glaucis hirsutus 
                          9                           8 
          Hylocharis cyanea              Momotus momota 
                          8                           8 
             Capito auratus       Attila citriniventris 
                          7                           6 
        Buteo  magnirostris       Tachyphonus cristatus 
                          6                           6 
   Thamnophilus schistaceus           Cercomacra  serva 
                          6                           5 
      Euphonia xanthogaster                Habia rubica 
                          5                           5 
       Laniocera  hypopyrra          Myiobius  barbatus 
                          5                           5 
   Myrmotherula longipennis       Oryzoborus angolensis 
                          5                           5 
        Rhytipterna simplex        Veniliornis  affinis 
                          5                           5 
      Micrastur gilvicollis   Myrmotherula menestriesii 
                          4                           4 
    Phaeothlypis fulvicauda            Sclateria naevia 
                          4                           4 
 Stelgidopteryx  ruficollis           Tangara chilensis 
                          4                           4 
               Cacicus cela            Celeus grammicus 
                          3                           3 
    Chelidoptera  tenebrosa    Chlorostilbon mellisugus 
                          3                           3 
          Claravis pretiosa              Crotophaga ani 
                          3                           3 
      Gymnopithys lunulatus         Lipaugus vociferans 
                          3                           3 
     Lophotriccus  vitiosus            Monasa morphoeus 
                          3                           3 
     Philydor erythrocercum          Sclerurus caudatus 
                          3                           3 
     Selenidera reinwardtii     Caprimulgus  nigrescens 
                          3                           2 
      Cercomacra nigrescens         Chrysuronia  oenone 
                          2                           2 
       Columbina talpacotti       Deconycura longicauda 
                          2                           2 
       Euphonia rufiventris       Micrastur mirandollei 
                          2                           2 
           Neoctantes niger             Notarchus ordii 
                          2                           2 
    Pachyramphus marginatus  Pachyramphus polychopterus 
                          2                           2 
  Phoenicircus  nigricollis            Saltator grossus 
                          2                           2 
     Tyrannus melancholicus     Accipiter superciliosus 
                          2                           1 
         Amazilia fimbriata        Ammodramus aurifrons 
                          1                           1 
     Ancistrops strigilatus        Automolus infuscatus 
                          1                           1 
              Bucco tamatia     Campephilus rubricollis 
                          1                           1 
         Crypturellus  soui         Cyanerpes caeruleus 
                          1                           1 
          Cyanerpes cyaneus       Electron platyrynchus 
                          1                           1 
      Hylocharis sapphirina     Micromonacha lanceolata 
                          1                           1 
     Microrhopias quixensis              Myarchus ferox 
                          1                           1 
           Oporornis agilis  Poecilotriccus latirostris 
                          1                           1 
         Pteroglossus azara         Pygiptila stellaris 
                          1                           1 
     Ramphocaenus melanurus           Ramphocelus carbo 
                          1                           1 
    Rhynchocyclus olivaceus       Thamnophilus doliatus 
                          1                           1 
          Thraupis palmarum         Tityra semifasciata 
                          1                           1 
               Trogon rufus            Turdus lawrencii 
                          1                           1 
          Tyrannulus elatus      Xiphorhynchus guttatus 
                          1                           1 
```
Rank curve for all captures
========================================================
![plot of chunk unnamed-chunk-5](RclassIndependent-figure/unnamed-chunk-5-1.png) 
