import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

pollution = pd.read_csv("C:\\Users\Alex Wang\Desktop\Beijing\\2016BeijingEdit.csv",parse_dates=['Hour']) # Plotting the graph without outliers
print(pollution['AQI Value'].describe())#gives count, mean value, std, minimum, max, quartiles of pollution data

data = pollution[['Hour','AQI Value']]
data.head()

pollution.plot()
plt.show()