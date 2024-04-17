# remove all objects matching a pattern


objects <- ls()

# Define the pattern to match
pattern <- "epic_ims"

# Find objects that match the pattern
objects_to_remove <- objects[grep(pattern, objects)]

# Remove objects matching the pattern
rm(list = objects_to_remove)