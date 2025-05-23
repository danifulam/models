# -*- coding: utf-8 -*-
"""tarjetacredito.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1rAXDux5SXRKlsheyIKxoizDHPNMbrXuX
"""

# Importar las librerías necesarias
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import classification_report, confusion_matrix, roc_auc_score

# Cargar el archivo .txt
# Ajusta el delimitador si es necesario (tabulaciones o comas)
from google.colab import files
df=pd.read_csv('/content/drive/MyDrive/credito.txt', delimiter='\t')
# Asegurarnos de que las columnas continuas sean del tipo float64
columns_continuas = ['A2', 'A3', 'A8', 'A11', 'A14', 'A15']  # Lista de las columnas continuas

# Convertir las columnas continuas a float64
for col in columns_continuas:
    df[col] = df[col].astype('float64')

# Verificar los tipos de las columnas después de la conversión
print(df.dtypes)

# Dividir las variables predictoras (X) y la variable de respuesta (y)
X = df.drop('R1', axis=1)  # Variables predictoras (A1, A2, ..., A15)
y = df['R1']  # Variable de respuesta (si la solicitud fue aceptada: 1, rechazada: 0)

# Estandarizar las variables continuas
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# Crear y entrenar el modelo de regresión logística con todos los datos
model = LogisticRegression(random_state=42)
model.fit(X_scaled, y)

# Extraer coeficientes e intercepto
intercept = model.intercept_[0]
coefficients = model.coef_[0]

# Mostrar la ecuación completa
equation = f"logit(p) = {intercept:.4f} " + " ".join(
    [f"+ ({coeff:.4f} * {col})" for coeff, col in zip(coefficients, X.columns)]
)

print("Ecuación logística completa:")
print(equation)

# Realizar predicciones sobre todo el conjunto de datos
y_pred = model.predict(X_scaled)

# Evaluar el rendimiento del modelo con todo el conjunto de datos
print("Matriz de confusión:\n", confusion_matrix(y, y_pred))
print("\nReporte de clasificación:\n", classification_report(y, y_pred))
print("\nAUC-ROC:", roc_auc_score(y, model.predict_proba(X_scaled)[:, 1]))

import numpy as np
from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt

# Entrenar el modelo de regresión logística
model = LogisticRegression()
model.fit(X, y)

# Obtener las probabilidades predichas
y_prob = model.predict_proba(X)[:, 1]  # Probabilidades para la clase 1 (aceptada)

# Calcular la curva ROC y el área bajo la curva (AUC)
fpr, tpr, thresholds = roc_curve(y, y_prob)
roc_auc = auc(fpr, tpr)

# Graficar la curva ROC
plt.figure(figsize=(8, 6))
plt.plot(fpr, tpr, color='blue', lw=2, label=f'ROC Curve (AUC = {roc_auc:.2f})')
plt.plot([0, 1], [0, 1], color='gray', linestyle='--', lw=2)  # Línea diagonal (azar)
plt.xlabel('Tasa de Falsos Positivos (1 - Especificidad)')
plt.ylabel('Tasa de Verdaderos Positivos (Sensitividad)')
plt.title('Curva ROC')
plt.legend(loc='lower right')
plt.grid(alpha=0.5)
plt.show()