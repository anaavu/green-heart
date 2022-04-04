# libraries
import os
import pandas as pd

os.chdir("C:/Users/a0uppa01/University of Louisville/Envirome Institute members_group - General/GIS/COVID")
# data input
dates = ["20200525", "20200601", "20200706", "20200720", "20200727", "20200803", "20200810", "20200817", "20200824",
         "20200831", "20200907", "20200914", "20200921", "20200928", "20201005", "20201012", "20201019", "20201026",
         "20201102", "20201109", "20201116", "20201123", "20201130", "20201207", "20201214", "20201221", "20201228",
         "20210104", "20210111", "20210118", "20210125", "20210201", "20210208", "20210215", "20210222", "20210301",
         "20210308", "20210315", "20210322", "20210329", "20210405", "20210412", "20210419", "20210426", "20210503",
         "20210510", "20210517", "20210531", "20210705", "20210712", "20210719", "20210726", "20210802", "20210809",
         "20210816", "20210823", "20210830", "20210906", "20210913", "20210920", "20210927", "20211004"]
for date in dates:
    ziponly_input = pd.read_csv("GeocodingItemizedSteps/CSV/Oct11_data/ziponly_output_coimmunity/ziponly_inblocks_{}.csv".format(date), low_memory=False)

    frequency_sewer = (ziponly_input.groupby(['AREA'])[['Cases_per_block']].agg('sum'))
    frequency_sewer.to_csv("GeocodingItemizedSteps/CSV/Oct11_data/ziponly_output_coimmunity/zone_result/zone_cases_{}.csv".format(date))

