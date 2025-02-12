import random
import pandas as pd

# Caricamento del dataset
file_path = "C:/ASD-Child/Autism-Child-Data.csv"
df = pd.read_csv(file_path)

# Mostra le prime righe per comprendere la struttura
df.head()

# Definizione delle nuove categorie per le variabili categoriche aggiuntive
age_desc_value = "4-11 years"
relation_categories = ["Parent", "Self", "Relative", "Health care professional"]

ethnicity_categories = [
    "Others", "Middle Eastern", "White-European", "Black", "South Asian",
    "Asian", "Hispanic", "Latino", "Turkish"
]

country_categories = [
    "Afghanistan", "Argentina", "Armenia", "Australia", "Austria", "Bahrain",
    "Bangladesh", "Bhutan", "Brazil", "Bulgaria", "Canada", "China",
    "Costa Rica", "Egypt", "Europe", "Georgia", "Germany", "Ghana", "India",
    "Iraq", "Ireland", "Isle of Man", "Italy", "Japan", "Jordan", "Kutwait",
    "Latvia", "Lebanon", "Libya", "Malaysia", "Malta", "Mexico", "Nepal",
    "Netherlands", "New Zealand", "Nigeria", "Oman", "Pakistan",
    "Philippines", "Qatar", "Romania", "Russia", "Saudi Arabia",
    "South Africa", "Sweden", "Syria", "Turkey", "U.S. Outlying Islands",
    "UAE", "United Kingdom", "United States"
]

# Numero di nuovi individui da generare
num_samples = 291

# Generazione dei dati simulati con le nuove variabili
generated_data_v2 = []

for _ in range(num_samples):
    # Generazione delle caratteristiche comportamentali binarie
    behavioral_scores = [random.randint(0, 1) for _ in range(10)]
    
    # Calcolo del risultato e della classe ASD
    result = sum(behavioral_scores)
    class_asd = 1 if result >= 7 else 0
    
    # Generazione delle altre variabili con le nuove regole
    age = random.randint(4, 11)
    gender = random.choice(["m", "f"])
    ethnicity = random.choice(ethnicity_categories)
    jundice = random.choice(["yes", "no"])
    autism = random.choice(["yes", "no"])
    country_of_res = random.choice(country_categories)
    used_app_before = random.choice(["yes", "no"])
    relation = random.choice(relation_categories)
    
    # Creazione del dizionario per l'individuo con il nuovo ordine delle colonne
    individual = (
        behavioral_scores +
        [age, gender, ethnicity, jundice, autism, country_of_res, used_app_before, result, age_desc_value, relation, class_asd]
    )
    generated_data_v2.append(individual)

# Definizione delle colonne con il nuovo ordine
column_names_v2 = list(df.columns[:-1]) + ["age_desc", "relation", "ClassASD"]

# Creazione del DataFrame con i nuovi dati
generated_df_v2 = pd.DataFrame(generated_data_v2, columns=column_names_v2)

# Mostra i nuovi dati generati
generated_df_v2