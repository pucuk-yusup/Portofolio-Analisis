# -*- coding: utf-8 -*-
"""Latihanseaborn.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1CP7j6AhcuYdkLiBqPUsepeUDIIMneho5
"""

!pip install seaborn --upgrade

!pip install matplotlib pandas

import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd

tips = sns.load_dataset("tips")

sns.scatterplot(data=tips, x="total_bill", y="tip")
plt.show()

sns.histplot(data=tips, x="total_bill", bins=20, kde=True)
plt.show()

sns.boxplot(data=tips, x="day", y="total_bill")
plt.show()

# Pilih hanya kolom numerik
numerical_data = tips.select_dtypes(include=['number'])

# Hitung korelasi
corr = numerical_data.corr()

# Visualisasi heatmap
sns.heatmap(corr, annot=True, cmap="coolwarm")
plt.show()

# Set tema
sns.set_theme(style="whitegrid")

# Scatter plot dengan tema
sns.scatterplot(data=tips, x="total_bill", y="tip", hue="sex", style="time")
plt.show()

# Membuat dan menyimpan heatmap
sns.heatmap(corr, annot=True, cmap="coolwarm")
plt.savefig("heatmap_output.png", dpi=300, bbox_inches="tight")  # Simpan file
plt.show()

import os
print(os.listdir())  # Menampilkan daftar file di direktori saat ini

from google.colab import files
files.download("heatmap_output.png")

#menyimpan scaterplot
sns.scatterplot(data=tips, x="total_bill", y="tip", hue="sex", style="time")
plt.savefig("scaterplot_output.png", dpi=300, bbox_inches="tight")  # Simpan file
plt.show()

#memastikan
import os
print(os.listdir())  # Menampilkan daftar file di direktori saat ini

#download
from google.colab import files
files.download("scaterplot_output.png")

#menyimpan histogram
sns.histplot(data=tips, x="total_bill", bins=20, kde=True)
plt.savefig("histogram_output.png", dpi=300, bbox_inches="tight")  # Simpan file
plt.show()

#memastikan
import os
print(os.listdir())  # Menampilkan daftar file di direktori saat ini

#download
from google.colab import files
files.download("histogram_output.png")

#menyimpan boxplot
sns.boxplot(data=tips, x= "day", y="total_bill")
plt.savefig("boxplot_output.png", dpi=300, bbox_inches="tight")  # Simpan file
plt.show()

#memastikan
import os
print(os.listdir())  # Menampilkan daftar file di direktori saat ini

#download
from google.colab import files
files.download("boxplot_output.png")