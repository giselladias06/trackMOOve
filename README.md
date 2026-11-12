# trackMOOve

TrackMOOve is an interactive dashboard for analysing animal movement networks and supporting epidemiological surveillance. It shows descriptive statistics on movements, network metrics (cohesion and node centrality), and contact chains for a single holding, so that investigators can trace direct and indirect contacts of farms under investigation.

The dashboard is built with R Shiny and was developed as a specialist-course final project (TCC) in Data Science and Analytics, USP/ESALQ MBA, 2025.

Data

The data in this repository is simulated. It describes a fictional country, Sentara, not a real one. The sample covers cattle movements from 1 January to 28 June 2024, with 15,000 movements, 10 abattoirs, and about 7,400 farms.

The fictional country and its farm register come from the Sentara repository: https://github.com/EarlyWarningAnimalHealth/Sentara. See the metadata file in data/ for how the sample was generated.

To use real movement data, replace data/sentara_movements_sample_2024_h1.csv with a file that has the same columns (see below).

Repository contents
trackmoove/
├── global.R      # loads libraries and the data once
├── ui.R          # user interface
├── server.R      # calculations and outputs
├── data/
│   ├── sentara_movements_sample_2024_h1.csv
│   ├── sentara_abattoirs_sample.csv
│   └── sentara_movements_sample_2024_h1_metadata.md
└── www/
    ├── custom.css
    └── TrackMOOve.png


The movements file must have these columns:

Column -	Description
movement_id	- Movement record identifier
issue_date	- Date of the movement (YYYY-MM-DD)
purpose	- Purpose of the movement (Fattening, Slaughter, Breeding, Other)
origin_farm_id	- Origin holding
destination_id	- Destination holding or abattoir
inspection_type	- Inspection type of the destination abattoir (SIF, SIE, SIM), or NA for farms
head_count -	Number of animals in the movement
origin_district -	District of the origin holding
destination_district -	District of the destination holding or abattoir
