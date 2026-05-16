import pandas as pd
data = pd.read_stata('code/reference_code/repgermany.dta')
data.to_csv('code/reference_code/repgermany.csv')