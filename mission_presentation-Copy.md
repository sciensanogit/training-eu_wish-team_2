Here are your missions!!!

# Session 1 (30/10/2025) ----

## Mission 1\_1 ----

* Clone the online repository of your team locally on your computer using RStudio
* Each member creates a copy of the mission1-member1.R file, rename it with its initials, code a line displaying a text in the Console, and save the changes.
* One member adapts the 00\_main.R file to source all files created by the other members
* Each member select the files modified, commit them in github and push their version on the online repository. To push, you'll need to log into github.
* Each member pulls the changes and run the modified scripts. This message should appear in the console "- This scripts was coded by Member 1, - This script was coded by member 2, etc..."

## Mission 1\_2 ----

* Decide how to collaborate with github, split the work and meet every 20 min
* Adapt mission2.R
* save a graph for the treatment plant "Aalst" and "Oostende" between 2024-09-01 and 2025-09-01 for the target "SARS SARS-CoV-2 E gene"
* Adapt the yaxis to match the one displayed in Murals
* Adapt the xaxis to match the one displayed in Murals
* Add a past two weeks moving average line
* Display the mean of "E gen", "N1 gen", "N2 gen" as "SARS" instead of "E gen" only
* Display the viral ratio of measure "SARS" over "PMMV"
* Display values below LOQ in red, and the others in green
* Translate the graph in two languages of your choice using a function

# Session 2 (11/12/2025) ----

## Mission 2 ----

* Run mission1\_2.R to produce and save a graph
* Run mission2.R file to produce a .html report and open it with an internet browser
* adapt .qmd file with sentences explaining the content displayed
* Adapt the graph to
  -- Display nice xaxis, yaxis
  -- Display the mean of "E gen", "N1 gen", "N2 gen" as "SARS" instead of "E gen" only
  -- Display the national viral ratio of measure "SARS" over "PMMV" weighted on population covered
  -- Add a past two weeks moving average line
* Adapt the table
  -- to show last ten dates of the national viral ratio
  -- to have nice headers
* extra steps if you have time
  -- produce .html and .docx files with the quarto render
  -- Display LOQ in red on graph (values below 1000 c/c)
  -- produce reports in two languages
  -- produce report for two different dates (2025-08-24 and 2025-09-01)
  -- add a graph of RSV and influenza in the reports
