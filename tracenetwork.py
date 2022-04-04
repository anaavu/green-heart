import arcpy

# Set workspace
arcpy.env.workspace = defaultgdb = r"C:\Users\a0uppa01\Documents\ArcGIS\Projects\MyProject1\MyProject1.gdb"
# Inputs
networkloc = r"C:\Users\a0uppa01\University of Louisville\Envirome Institute members_group - General\GIS\COVID\RawData\msddata_2_RY_202011.gdb\msddata.gdb"
startingpts = "StudyID"

# Convert startingpts to layer

# Select one point
# Create a new layer
# Feed this and networkloc to Trace Tool (use Aggregated Geometry)
# Get Aggregated Lines
# Pour into Minimum Bounding Box