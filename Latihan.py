pip install pandas matplotlib
# Import libraries
import pandas as pd
import matplotlib.pyplot as plt

# Baca dataset dari file CSV
df = pd.read_csv('data.csv')

# Tampilkan 5 baris pertama dari dataset
print(df.head())

# Tampilkan informasi tentang dataset
print(df.info())

# Tampilkan statistik deskriptif
print(df.describe())

# Plot histogram dari satu kolom (misalnya 'Age')
plt.hist(df['Age'], bins=10, color='blue', alpha=0.7)
plt.title('Distribution of Age')
plt.xlabel('Age')
plt.ylabel('Frequency')
plt.show()

# Jika kamu ingin menyimpan plot ke file
# plt.savefig('age_distribution.png')

# Analisis sederhana: Hitung jumlah nilai unik di kolom tertentu (misalnya 'Gender')
print(df['Gender'].value_counts())



