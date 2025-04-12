import numpy as np
import pandas as pd
import os

# Percorso del file sintetico
script_dir = os.path.dirname(__file__)
original_dataframe_file = "Autism-Child-Data.csv"
original_dataframe_path = os.path.join(script_dir, original_dataframe_file)

# Caricare il dataset originale
original_df = pd.read_csv(original_dataframe_path)

# Definire l'ordine preciso delle colonne
ordered_columns = [
    "A1_Score", "A2_Score", "A3_Score", "A4_Score", "A5_Score",
    "A6_Score", "A7_Score", "A8_Score", "A9_Score", "A10_Score",
    "age", "gender", "ethnicity", "jundice", "autism",
    "country_of_res", "used_app_before", "result", "age_desc", "relation", "ClassASD"
]

# Definire le colonne binarie An_Score
binary_cols = ["A1_Score", "A2_Score", "A3_Score", "A4_Score", "A5_Score",
               "A6_Score", "A7_Score", "A8_Score", "A9_Score", "A10_Score"]

# Contare la distribuzione di result
result_distribution = original_df["result"].value_counts(normalize=True)

# Funzione per generare dati sintetici con colonne ordinate
def generate_synthetic_data(original_df, ordered_columns):
    synthetic_df = pd.DataFrame()

    # Generare dati binari rispettando la distribuzione di result
    generated_binary_data = []
    for result_value, proportion in result_distribution.items():
        subset = original_df[original_df["result"] == result_value][binary_cols]
        sample_size = int(len(original_df) * proportion)
        sampled_rows = subset.sample(n=sample_size, replace=True).values
        generated_binary_data.append(sampled_rows)

    # Concatenare i dati generati
    synthetic_binary_data = np.vstack(generated_binary_data)
    np.random.shuffle(synthetic_binary_data)  # Mescolare i dati per variare la distribuzione

    # Aggiungere le colonne binarie al dataset sintetico
    for i, col in enumerate(binary_cols):
        synthetic_df[col] = synthetic_binary_data[:, i]
        
    # Calcolare il result come somma delle colonne A1_Score - A10_Score
    synthetic_df["result"] = synthetic_df[binary_cols].sum(axis=1)

    # Determinare ClassASD in base a result (YES se result >= 7, altrimenti NO)
    synthetic_df["ClassASD"] = np.where(synthetic_df["result"] >= 7, "YES", "NO")

    # Generare dati per colonne categoriali mantenendo le frequenze originali
    categorical_cols = [col for col in ordered_columns if col not in binary_cols + ["result", "ClassASD"]]
    for col in categorical_cols:
        synthetic_df[col] = np.random.choice(original_df[col].dropna().unique(), size=len(original_df), 
                                             p=original_df[col].value_counts(normalize=True).values)

    # Ordinare le colonne secondo la sequenza richiesta
    synthetic_df = synthetic_df[ordered_columns]

    return synthetic_df

# Generare il dataset sintetico con le nuove regole
synthetic_df = generate_synthetic_data(original_df, ordered_columns)

# Percorso del file sintetico
synthetic_dataframe_file = "Autism-Child-Data-Synthetic.csv"
synthetic_dataframe_path = os.path.join(script_dir, synthetic_dataframe_file)

# Salvare il dataset sintetico
synthetic_df.to_csv(synthetic_dataframe_path, index=False)