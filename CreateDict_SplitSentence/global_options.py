"""Global options for analysis
"""
import os
from pathlib import Path
from typing import Dict, List, Optional, Set

# Hardware options
N_CORES: int = 8  # max number of CPU cores to use
RAM_CORENLP: str = "10G"  # max RAM allocated for parsing using CoreNLP; increase to speed up parsing
PARSE_CHUNK_SIZE: int = 100 # number of lines in the input file to process uing CoreNLP at once. Increase on workstations with larger RAM (e.g. to 1000 if RAM is 64G)  

# Directory locations
os.environ[
    "CORENLP_HOME"
] = "C:/Users/ar/sciebo/ESG/stanford-corenlp-full-2018-10-05"  # location of the CoreNLP models; use / to seperate folders
DATA_FOLDER: str = "data/"
MODEL_FOLDER: str = "models/" # will be created if does not exist
OUTPUT_FOLDER: str = "outputs/" # will be created if does not exist; !!! WARNING: existing files will be removed !!!

# Parsing and analysis options
STOPWORDS: Set[str] = set(
    Path("resources", "StopWords_Generic.txt").read_text().lower().split()
)  # Set of stopwords from https://sraf.nd.edu/textual-analysis/resources/#StopWords
PHRASE_THRESHOLD: int = 10  # threshold of the phraser module (smaller -> more phrases)
PHRASE_MIN_COUNT: int = 10  # min number of times a bigram needs to appear in the corpus to be considered as a phrase
W2V_DIM: int = 300  # dimension of word2vec vectors
W2V_WINDOW: int = 5  # window size in word2vec
W2V_ITER: int = 20  # number of iterations in word2vec
N_WORDS_DIM: int = 500  # max number of words in each dimension of the dictionary
DICT_RESTRICT_VOCAB = None # change to a fraction number (e.g. 0.2) to restrict the dictionary vocab in the top 20% of most frequent vocab

# Inputs for constructing the expanded dictionary
DIMS: List[str] = ["environmental", "social", "governance"]
SEED_WORDS: Dict[str, List[str]] = {
  
 "environmental": [
"biodiversity"
, "climate_risk"
, "clean_energy"
, "renewable_energy"
, "energy_efficiency"
, "energy_efficient"
, "environmental_fines"
, "environmental"
, "environmental_mgmt._system"
, "environmental_policy"
, "environmental_reporting"
, "environmental_report"
, "forests"
, "forest"
, "ghg_emissions"
, "ghg"
, "emissions"
, "emission"
, "ghg_policies"
, "ghg_policy"
, "gmos"
, "gmo"
, "global_compact_membership"
, "global_compact"
, "green_buildings"
, "green_building"
, "green_products"
, "green_product"
, "hazardous_waste"
, "non-ghg_air_emissions"
, "air_emission"
, "ozone-depleting_gases"
, "sustainable_packaging"
, "recycling"
, "resource_efficiency"
, "site_closure"
, "sustainable_finance"
, "toxic_spills"
, "toxic_spill"
, "waste"
, "water_use"  ], 
 
  "social": [
"access_to_basic_services"
, "basic_services"
, "anti-competitive_practices"
, "anti-competitive"
, "child_labor"
, "forced_labor"
, "collective_bargaining"
, "community_and_society"
, "customer_relationship"
, "gender_diversity"
, "electromagnetic_fields"
, "electromagnetic_field"
, "employee_development"
, "employee_training"
, "employee_turnover"
, "financial_inclusion"
, "health_and_safety"
, "human_rights"
, "human_right"
, "indigenous_rights"
, "indigenous_right"
, "labor_practices"
, "labor_practice"
, "philanthropy"
, "privacy_and_it"
, "privacy"
, "customer_privacy"
, "product_safety"
, "responsible_marketing"
, "labor_union"
, "labor_unions"  ], 
 
  "governance": [
"animal_welfare"
, "audit"
, "board_diversity"
, "business_ethics"
, "ethics"
, "ethical"
, "business_ethic"
, "corporate_governance"
, "governance"
, "lobbying"
, "lobby_work"
, "remuneration"
, "reporting_quality"  ],
}


# Create directories if not exist
Path(DATA_FOLDER, "processed", "parsed").mkdir(parents=True, exist_ok=True)
Path(DATA_FOLDER, "processed", "unigram").mkdir(parents=True, exist_ok=True)
Path(DATA_FOLDER, "processed", "bigram").mkdir(parents=True, exist_ok=True)
Path(DATA_FOLDER, "processed", "trigram").mkdir(parents=True, exist_ok=True)
Path(MODEL_FOLDER, "phrases").mkdir(parents=True, exist_ok=True)
Path(MODEL_FOLDER, "phrases").mkdir(parents=True, exist_ok=True)
Path(MODEL_FOLDER, "w2v").mkdir(parents=True, exist_ok=True)
Path(OUTPUT_FOLDER, "dict").mkdir(parents=True, exist_ok=True)
Path(OUTPUT_FOLDER, "scores").mkdir(parents=True, exist_ok=True)
Path(OUTPUT_FOLDER, "scores", "temp").mkdir(parents=True, exist_ok=True)
Path(OUTPUT_FOLDER, "scores", "word_contributions").mkdir(parents=True, exist_ok=True)
