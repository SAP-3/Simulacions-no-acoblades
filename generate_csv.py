import sys
import pandas as pd

if len(sys.argv)!=5:
    print("ERROR: El nombre d'arguments no és vàlid")
    print("COMANDA ESPERADA: python generate_csv.py centil dia dir_wx dir_x6")
    sys.exit(1)


dic_dirs = {'N':0, 'NW':45, 'W':90, 'SW':135, 'S':180, 'SE':225, 'E':270, 'NW':315}

centil = sys.argv[1]
dia = int(sys.argv[2])
dir_wx = dic_dirs[sys.argv[3]]
dir_x6 = dic_dirs[sys.argv[4]]

coords_wx = [41.91780, 0.88175]
coords_x6 = [41.92173, 1.02901]
max_temp = [22, 34]

velocitats = pd.read_csv('C:/Users/abril/OneDrive/UAB/SAP/Projecte/velocitats.csv')

velocitat_wx = float(velocitats.loc[(velocitats['ESTACIO']=='wx') & (velocitats['DIA']==dia) & (velocitats['CENTIL']==centil),'VELOCITAT'])*3.6
velocitat_x6 = float(velocitats.loc[(velocitats['ESTACIO']=='x6') & (velocitats['DIA']==dia) & (velocitats['CENTIL']==centil),'VELOCITAT'])*3.6



headers = ["Station_Name", "Coord_Sys(PROJCS, GEOGCS)", "Datum(WGS84, NAD84, NAD27)", "Lat/YCoord", "Lon/XCoord", 
           "Height", "Height_Units(meters,feet)", "Speed", "Speed_Units(mph,kph,mps,kts)", "Direction(degrees)",
            "Temperature", "Temperature_Units(F,C)", "Cloud_Cover(%)", "Radius_of_Influence", "Radius_of_Influence_Units(miles,feet,meters,km)",
             "date_time"]

data_wx = ['WX', 'GEOGCS', 'WGS84', coords_wx[0], coords_wx[1], 2, 'meters', velocitat_wx, 'kph', dir_wx, max_temp[dia], 'C', 0, -1, 'km','']
data_x6 = ['X6', 'GEOGCS', 'WGS84', coords_x6[0], coords_x6[1], 2, 'meters', velocitat_x6, 'kph', dir_x6, max_temp[dia], 'C', 0, -1, 'km','']

df_wx = pd.DataFrame(data_wx, columns=headers)
df_x6 = pd.DataFrame(data_x6, columns=headers)

df_wx.to_csv('./data/wx.csv', index=False, quoting=csv.QUOTE_NONNUMERIC)
df_x6.to_csv('./data/x6.csv', index=False, quoting=csv.QUOTE_NONNUMERIC)
