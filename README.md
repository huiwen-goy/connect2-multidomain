# connect2-multidomain

This is the project where we did a second round of data collection from the Connect pool that did the original stigma-related survey. This survey was done in Qualtrics, and included questions on multiple domains of health (sensory, cognitive, physical, mental and psychosocial), as well as measures of social participation and general- and domain-specific quality of life. The majority of participants agreed to let us access their Connect clinic data, which included a full audiogram and information on hearing aid uptake.    

The Rmd file contains the code for: merging Qualtrics data with Connect data, recoding and creating new variables, a bit of cleaning, and comparisons of the three groups on aspects of health and well-being. The new variables are the "scored" versions compiled from individual items (see information in the Refs folder).

In general, any cells showing as NA in the raw dataset are actual missing items that participants chose not to respond to, or were skipped by Qualtrics because they were conditional items. When a questionnaire item actually included "Not Applicable" as a response option, the answer was coded as a numeric response in Qualtrics and recoded to NA as needed during scoring.
