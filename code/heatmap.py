import pandas as pd
import folium
from folium import plugins

df = pd.read_csv("dataverse_files/2007.csv")
airports = pd.read_csv("dataverse_files/airports.csv")

Q2_df = pd.merge(df, airports['iata'], left_on='Origin', right_on='iata', how='left')

Q2_temp_df = pd.DataFrame(Q2_df.groupby(['Year', 'Month', 'iata'], as_index=False).size())
Q2_temp_df = Q2_temp_df.rename(columns={'size': 'flight_count'})

flight_df = pd.merge(Q2_temp_df, airports[['iata', 'lat', 'long']], on='iata', how='left')
flight_df['flight_count'] /= flight_df['flight_count'].max() / 100
heat_data = []
for name, value in flight_df.groupby(['Year', 'Month']):
    heat_data.append(value[['lat', 'long', 'flight_count']].values.tolist())

m = folium.Map([34, -100], zoom_start=4, width='90%', height='90%')

hm = folium.plugins.HeatMapWithTime(heat_data, radius=30, auto_play=True,
                                    gradient={0.25: 'blue', 0.5: 'lime', 0.7: 'yellow', 0.9: 'orange', 1.0: 'red'})
hm.add_to(m)
m.save('./heatmap_with_time.html')

