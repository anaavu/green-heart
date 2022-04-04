import os
import pandas as pd

# User inputs
models_directory = r"C:\Users\jloyd\Downloads\sbpavg_model3_htn2filteredhomes"
output_location = r"C:\Users\jloyd\Downloads"

# This is what the output file will be called (folder name of models)
resultfilename = models_directory.rsplit("\\", 2)[2]
print(resultfilename)

# Grabs list of files in models directory
files = [f for f in os.listdir(models_directory) if not f.endswith("_selected.csv") and not f.endswith("confint.csv")]

# Create greenness column
modelsdf = pd.DataFrame(columns=["term", "greenness"])

terms = pd.read_csv(os.path.join(models_directory, files[0]))

modelsdf['term'] = terms['term']


#Create columns with filenames
for file in files:
    read = pd.read_csv(os.path.join(models_directory, file)) #read in file
    filedf = pd.DataFrame(columns=[file])
    filedf[file] = read["estimate"]
    modelsdf = modelsdf.join(pd.DataFrame(filedf))

# Output
modelsdf.to_csv("{}\{}.csv".format(output_location, resultfilename))
