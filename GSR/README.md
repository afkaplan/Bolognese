# Bolognese: Clitic Allomorphy with Gradient Symbolic Representations
Files for analysis of Bolognese clitic allomorphy using GSRs

Here you can find R files for work presented at WCCFL 41 (Rubin & Kaplan 2023) and an updated version presented at LSRL 54 (Rubin & Kaplan 2024). The files included are:

For Rubin & Kaplan (2023):
1. inputs_candidates.R: this specifies, for each input, the candidates to be considered and their constraint violations.  Put activities here.
2. NoisyHG.R: this file runs NHG.  Put constraint weights here.

To use these files, first run inputs_candidates.R, then run NoisyHG.R.


For Rubin & Kaplan (2024):
1. inputs_candidates_optionalEpenthesis.R: this specifies, for each input, the candidates to be considered and their constraint violations.  Put activities here.
2. NoisyHG_optionalEpenthesis.R: this file runs NHG.  Put constraint weights here.

To use these files, first run inputs_candidates_optionalEpenthesis.R, then run NoisyHG_optionalEpenthesis.R.  As suggested by the file names, the 2024 analysis is expanded to include data involving optional vowel epenthesis occurring when a clitic precedes certain kinds of verb-initial complex clusters.  At the time of this writing, we're not certain which kinds of clusters trigger epenthesis (maybe ones involving sibilants?), so we treat them with a constraint called "NoComplicated," a cover constraint for whatever the real factors are.

A note on the content of the files:  You will see that some constraints (and their weights) are commented out.  Though these constraints plausibly contribute to the phenomena at issue, we found them to be unnecessary.  They are retained in the R files for anyone who wants to explore them.  (You may need to edit the inputs_candidates files to mark candidates' violations of these constraints.)

For definitions of the constraints, see the corresponding handout/poster at https://afkaplan.github.io/handouts_posters/
