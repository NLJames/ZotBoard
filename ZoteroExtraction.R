# Converting notes and annotations from zotero bibtex to grabboard
# Note that ALL edits should be made in zotero and exported as this script will 
# refresh the graboard doc each time it is run

# 1. Adjust default settings so that annotations exports with highlights and tags https://www.zotero.org/support/note_templates
#     to do this edit: extensions.zotero.annotations.noteTemplates.highlight file to say:
#     "<p>{{highlight quotes='true'}} COLOUR={{color}} TAGS={{tags join=';'}} COMMENTS={{comment}}</p>"
# 3. Extract annotations (highlighted quotes, tags, comments and highlighter colour) 
#     -> can do in bulk by selecting multiple
# 4. Export a bibtex file from zotero for the collection you want to create your grabboard
# 5. Follow script below

library(bib2df)
library(tidyverse)
library(zoo)
library(writexl)
library(openxlsx)

# This gets the data for the paper relatively sorted but not the highlighted quotes and comments
pubData <- bib2df("./demo.bib")%>%
  dplyr::select( DOI, AUTHOR, BOOKTITLE, CHAPTER, JOURNAL, TITLE, YEAR, BIBTEXKEY)

# extract quotes, comments, colur and tags with identifier for each paper
# Haven't implemented this yet but where "<<" is entered in the comments
# it means the quote should be added to the previous quote which was either on a separate page or
# added after the comments and tags were entered (since selections can't be extended after the fact)
notes <- read.delim(header=F,"./demo.bib") %>%
  filter(grepl("^@|^\\``|.)}$", V1)) %>%
  separate(V1, into = c("BIBTEXKEY","quote"), sep = "``") %>%
  separate(BIBTEXKEY, into = c("BIBTEXKEY","noteDates"), sep = "\\(") %>%
  separate(quote, into = c("quote","colour"), sep = "COLOUR=") %>%
  separate(colour, into = c("colour","tags"), sep = "TAGS=") %>%
  separate(tags, into = c("tags","comments"), sep = "COMMENTS=") %>%
  mutate(BIBTEXKEY = substring(BIBTEXKEY, 10, nchar(BIBTEXKEY)-1),
         BIBTEXKEY = na_if(BIBTEXKEY, ""),
         BIBTEXKEY = na.locf(BIBTEXKEY),
         noteDates = substring(noteDates, 0, nchar(noteDates)-2),
         noteDates = na_if(noteDates, ""),
         noteDates = na.locf(noteDates, na.rm = FALSE))%>%
  filter(!is.na(quote)) %>%
  # Only take unique entries, so repeated annotation extraction isn't a problem
  # but changes in annotations will be included.
  # In the future it may be possible to keep only the latest one using the noteDates column
  distinct(colour, tags,quote,  comments, .keep_all = T) %>%
  left_join(pubData, .) %>%
  dplyr::select(DOI:YEAR, noteDates, tags, colour, BIBTEXKEY, quote, comments)

print(unique(notes$colour))
createWorkbook(notes)

yellow <- createStyle(bgFill = "#ffd400")
red <- createStyle(bgFill = "#fb5c89")
green <- createStyle(bgFill = "#7cc868")
blue <- createStyle(bgFill = "#69b0f1")
purple <- createStyle(bgFill = "#a28ae5")

conditionalFormatting(notes, cols="colour",)

head(notes)

# There may be issues here when re-writing the file over the old one casuing corruption
write_xlsx(notes, './Grabboard.xlsx')


# Further ideas
# could make it so that there is a column for the section it should be for (taken from tags)
# Then each tag after that becomes tag1, tag2...etc.

