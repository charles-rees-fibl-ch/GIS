library(plyr)
library(sf)
library(tmap)

# CH_Gemeindegrenzen ---------------------------------------------
# .. swissBOUNDARIES3D_1_3_TLM_BEZIRKSGEBIET.shp -------------------------------------------------------------

Bezirksgebiet = read_sf("//fibl.ch/FILES/Dep_FSS/2_Projects/2020_CH-SNF_LEAF_35188/02_WorkPackages/WP2_ModelDevelopment/01_Data/00_Public/CH_Gemeindegrenzen/ch.swisstopo.swissboundaries3d-gemeinde-flaeche.fill/swissBOUNDARIES3D_1_3_TLM_BEZIRKSGEBIET.shp")

tm_shape(Bezirksgebiet[Bezirksgebiet$KANTONSNUM==3,]) + # LU=3
  tm_polygons() +
  tm_grid()

# .. swissBOUNDARIES3D_1_3_TLM_BEZIRKSGEBIET.shp -------------------------------------------------------------
Gemeidegrenzen = read_sf("//fibl.ch/FILES/Dep_FSS/2_Projects/2020_CH-SNF_LEAF_35188/02_WorkPackages/WP2_ModelDevelopment/01_Data/00_Public/CH_Gemeindegrenzen/ch.swisstopo.swissboundaries3d-gemeinde-flaeche.fill/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

tm_shape(Gemeidegrenzen) + # LU=3
  tm_polygons() +
  tm_grid()


# Gesamteinzugsgebiete ---------------------------------------------
#   Gesamteinzugsgebiets-Nummer (EZGNR)
#   Surssee 117451
#   Hallwiler- & Baldeggersee 153798
#   Extract Gesamteinzugsgebiet_117451_Sempachersee
#     Gesamteinzugsgebiet_117451_Sempachersee = st_read("//fibl.ch/files/Oekonomie/Projekte-(laufend)/CH-SNF_LEAF/02_WorkPackages/WP2_ModelDevelopment/01_Data/00_Public/CH_Einzugsgebiete/Gesamteinzugsgebiete/gesamtseinzugsgebiete/Gesamteinzugsgebiete.gpkg", 
#              query="select * from Gesamteinzugsgebiete where EZGNR = 117451")
#   Save Gesamteinzugsgebiet_117451_Sempachersee
#     st_write(Gesamteinzugsgebiet_117451_Sempachersee,
#          "//fibl.ch/files/Oekonomie/Projekte-(laufend)/CH-SNF_LEAF/02_WorkPackages/WP2_ModelDevelopment/01_Data/00_Public/CH_Einzugsgebiete/Gesamteinzugsgebiete/gesamtseinzugsgebiete/Gesamteinzugsgebiet_117451_Sempachersee.gpkg")

#Gesamteinzugsgebiet_117451_Sempachersee = st_read("//fibl.ch/FILES/Dep_FSS/2_Projects/2020_CH-SNF_LEAF_35188/02_WorkPackages/WP2_ModelDevelopment/01_Data/00_Public/CH_Einzugsgebiete/Gesamteinzugsgebiete/gesamtseinzugsgebiete/Gesamteinzugsgebiet_117451_Sempachersee.gpkg")

#tm_shape(Gesamteinzugsgebiet_117451_Sempachersee) + 
  #tm_polygons() +
  #tm_grid()


