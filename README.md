
*This github repo is currently a work in progress. Please contact DataScience@dhsc.gov.uk if you have any questions or ideas for improvements.*

In the R folder, you will find quarto files with template code to run a basic consultation analysis. Feel free to run through these files as they are, or copy and paste out into your own scripts.
- [01_load_data.qmd](https://github.com/DataS-DHSC/consultation_example/blob/main/R/01_load_data.qmd)
- [02_pre-process_responses.qmd](https://github.com/DataS-DHSC/consultation_example/blob/main/R/02_pre-process_responses.qmd)
- [03_lda_analysis.qmd](https://github.com/DataS-DHSC/consultation_example/blob/main/R/03_lda_analysis.qmd)
- [04_output_for_labellers.qmd](https://github.com/DataS-DHSC/consultation_example/blob/main/R/04_output_for_labellers.qmd)

## Usage
### Random Seeds

For reproducibility we give the lda functions a random seed. We recommend setting this to the long-form of a date relevant to the project, e.g. 20250102. This ensures that different projects are using different seeds, which has become an issue in random simulations. e.g. some projects had statistical quirks because almost every project was using one of "42", "1234", ... etc. 

For best practise we would recommend using a second seed as a sensitivity analysis - asking whether the results are similar-enough if the seed is changed.

### Statistical Disclosure

*If in doubt on this section, consult a member of the Government Statistical Service (GSS)*

The outputs of this library should not be immediately published, but will likely go into the consultation response publication. It is important that individuals are not identifiable from the outputs. 

Before publication, counts should be rounded to the nearest 5 and counts below 10 should be suppressed. 

This should be considered within the context that a consultation is not a survey, and the outputs are not intended to be representative of the population.