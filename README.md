# Template code for a basic consultation example analysis with dummy data.

A common issue with the analysis of consultation responses is standardising data from several different sources of data. Often these are given as standard data (we will call this "main"), the easy read counterpart, the hard copies for both of these respectively.

The data cleaning problem arises due to the questions and possible responses to some questions being different between the data sources. For example...

- Main question: To what extent do you agree with [thing A]?
  - Possible responses: Strongly agree, Agree, Disagree, Strongly disagree, NA

- Corresponding easy read question: How much do you agree or disagree with [thing A]?
  - Possible responses: Definitely agree, Agree, Disagree, Definitely disagree, NA


### Guide to making a manifest file

The manifest is an excel spreadsheet that you fill out to give enough information to the code to standardise the data into one tidy table.

The first sheet in [manifest.xlsx](https://github.com/DataS-DHSC/consultation_example/blob/main/input/manifest/manifest.xlsx) should contain the following:

| Column         | Description                                                 |
|---------------:|:------------------------------------------------------------|
| `question`       | The exact column headers in your main data file.            |
| `type`           | The type of data entered into each (multi-choice, long text, etc). |
| `col_id`         | The column number, starting at one from the left side.      |
| `question_easy`  | The same as `question`, but for easy read data.             |
| `type_easy`      | The same as `type`, but for easy read data.                 |
| `col_id_easy`    | The same as `col_id`, but for easy read data.               |
| `assumed_easy`   | Assumed responses to give to missing columns in the easy read data. |
| `omit`           | Irrelevant columns to delete. This should contain only TRUE or FALSE. |
| `custom_map`     | The name of the sheet in manifest.xlsx with the custom mappings for certain questions(s). |


For all other columns that have different data that can be mapped from easy read to main, fill in said mappings in a new sheet called map, followed by a digit that corresponds to the col_id of the main question. 

If your data has any common answer type that is used more than once in your data (yes/no, likert, etc.), instead of a digit, just use a custom name such as "map_yesno" or "map_likert" as used in the example.

The example manifest file contains map_yesno, map_likert, map2, map4, and map6 relating to the eight questions that need standardising between main and easy read versions of the data.

In each of these sheets, give the mappings for main, easy read, then the final mappings that everything should be set to.

For example, the map_likert sheet may look something like this...

| main | easy | final |
|:-----|:-----|:------|
| Strongly agree | Definitely agree | Strongly agree |
| Agree	| Agree | Agree |
| Disagree | Disagree | Disagree |
| Strongly disagree | Definitely disagree | Strongly disagree |
| NA | NA | NA |


...and the one of the map sheets may look something like this:

| main | easy | final |
|:-----|:-----|:------|
|East Midlands | East Midlands | East Midlands |
| East of England | East of England | East of England |
| London | London | London |
| North East England | North | North |
| North West England | North | North |
| South East England | South | South |
| South West England | South | South |
| West Midlands | West Midlands | West Midlands |
| Yorkshire and the Humber | Yorkshire and the Humber | Yorkshire and the Humber |
| Prefer not to say | I do not want to say | Prefer not to say |
| NA | NA | NA |


Filling out the manifest carefully should ensure all your data is cleaned and standardised, without the hassle of adding lots of extra code.


### Further tweaking of the data cleaning code

Now you've made the manifest file, you may find there are a handful of more fiddly bits of data cleaning to do.

**Firstly, you'll need to add some metadata to [config.yml]().**

| general_params      | Description
|--------------------:|:-------------------------------------------------------|
| date_main           | The closing date of the main consultation. |
| date_easy           | The closing date of the easy read consultation. |
| input_dir           | Directory or path (leave as is). |
| output_dir          | Directory or path (leave as is). |
| responses_dir       | Directory or path (leave as is). |
| manifest_dir        | Directory or path (leave as is). |
| output_frequency    | Directory or path (leave as is). |
| qa_file_dir         | Directory or path (leave as is). |
| responses_prefix    | Directory or path (leave as is). |
| main_suffix         | Directory or path (leave as is). |
| easy_suffix         | Directory or path (leave as is). |
| hard_copies_flag    | Directory or path (leave as is). |
| manifest_file       | Directory or path (leave as is). |
| include_hard_copies | Do you have hard copies to add (emails, postal, etc)? |
| include_easyread    | Do you have easy read data to add? |

**Secondly, you need to make sure your data files are named as expected.**

You may have noticed the config has given paths, directories, and parts of filenames. You need to make sure your data are named to fit these. This example shows the main data, the easy read, the hard copies for both of these respectively. These are put into the [responses](https://github.com/DataS-DHSC/consultation_example/tree/main/input/responses) folder.

- responses_2024-01-09.xlsx
- responses_2024-01-09_easyread.xlsx
- responses_2024-01-09_hard_copies.xlsx
- responses_2024-01-09_hard_copies_easyread.xlsx

**Finally, due to the potentially unique nature of remaining data quality issues, the rest of the data cleaning and extra table generation is left to you to complete.**

We have including some useful code in this example in [run_load_data()](https://github.com/DataS-DHSC/consultation_example/blob/main/R/1-load-data.R) such as:

- standardising free text data into ONS age categories using the text_to_age() function.
- how to output a table for just individuals.
- how to output a frequency table of counts by location.


### Running the data cleaning code to get the final big table

Running the [run_load_data()](https://github.com/DataS-DHSC/consultation_example/blob/main/R/1-load-data.R) line in main.R should output the cleaned and standardised table.




*This github repo is currently a work in progress. Please contact DataScience@dhsc.gov.uk if you have any questions and/or ideas for improvements.*
