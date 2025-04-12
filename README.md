# Analisi del Disturbo dello Spettro dell’Autismo nei bambini
## Autori
- [**Luigi Giacchetti**](https://github.com/rankoll/)
- [**Chiara Puglia**](https://github.com/chiarapuglia99)

---
## Descrizione del progetto
Il progetto ha come obiettivo l'analisi di un dataset relativo allo **screening dell'Autismo nei bambini (età 4-11)**, con lo scopo di identificare le **caratteristiche comportamentali più significative** per la classificazione del disturbo dello spettro autistico (ASD - Autism Spectrum Disorder).

L'approccio adottato nel progetto combina metodi statistici e predittivi per ottenere una comprensione approfondita dei dati e delle loro implicazioni. In primo luogo, è stata condotta un'analisi esplorativa del dataset per identificare tendenze, anomalie e relazioni tra le variabili. Successivamente, è stata effettuata una selezione accurata delle caratteristiche più rilevanti, al fine di migliorare l'efficacia dei modelli predittivi. La costruzione e la valutazione di questi modelli hanno permesso di classificare con precisione i soggetti con ASD, sfruttando tecniche di machine learning. Infine, sono state applicate tecniche di clustering per individuare pattern significativi nei dati, contribuendo a distinguere i soggetti neurotipici da quelli con ASD in modo più chiaro e strutturato.
## Dataset
- 292 osservazioni
- 21 attributi tra cui 10 **caratteristiche comportamentali (AQ-10-Child)** e varie **informazioni personali/demografiche**.
## Domande di ricerca
- **RQ1**: _Quali caratteristiche comportamentali sono maggiormente predittive per una diagnosi accurata di ASD?_
- **RQ2**: _Come l’analisi di clustering basata su caratteristiche cliniche e comportamentali può distinguere soggetti con ASD da neurotipici?_
## Modelli Utilizzati
### Modelli di Classificazione utilizzati

- **Random Forest**  
  - Usato per la selezione delle caratteristiche tramite Eliminazione Ricorsiva delle Caratteristiche (RFE - Recursive Feature Elimination)
  - Ottima accuratezza in training ma meno robusto nel test

- **KNN (K-Nearest Neighbors)**  
  - Basato sulla distanza tra individui
  - Accuratezza training: ~44% | test: ~32%

- **WKNN (Weighted KNN)**  
  - Variante di KNN che assegna pesi in base alla distanza
  - Accuratezza training: ~48% | test: ~31%

- **ANN (Artificial Neural Network)**  
  - Reti neurali con due layer nascosti con Keras/Tensorflow
  - Accuratezza test: tra il 32% e il 41%  
  - Miglior accuratezza con tutte le feature comportamentali: **84%**

- **SVM (Support Vector Machines)**  
  - Elevata performance su training e test
  - Accuratezza: **96.4%**
### Modelli di Interpretabilità
**LIME + Shapley Values**
Per comprendere l’importanza delle variabili nei modelli complessi come Random Forest e ANN
### Modelli di Clustering
- **K-Means**
  - k = 2 con analisi delle componenti principali (PCA - Principal Component Analysis)
  - Silhouette Score medio: **0.42**

- **Clustering Gerarchico**  
  - Dendrogramma per la struttura dei cluster
  - Conferma la separazione tra neurotipici e ASD

- **DBSCAN (Density-Based Spatial Clustering)**  
  - Efficace nella rilevazione di pattern densi
  - Ottima corrispondenza con la variabile ClassASD
## Generazione Dati Sintetici
È stato creato un **dataset sintetico** utilizzando un Large Language Model (ChatGPT), replicando la distribuzione del dataset originale per verificare la coerenza strutturale.

## Conclusioni
Il progetto ha evidenziato come SVM sia un modello altamente performante per la classificazione dell’ASD, mentre tecniche di clustering (K-Means e clustering gerarchico) hanno mostrato una netta separazione tra soggetti con ASD e neurotipici. Le caratteristiche comportamentali A1, A5, A7, A8 risultano essere particolarmente predittive.