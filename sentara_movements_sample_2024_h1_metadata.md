# Sentara simulated cattle movements — sample, January to June 2024

## Summary

| | |
|---|---|
| File(s) | `sentara_movements_sample_2024_h1.csv`, `sentara_abattoirs_sample.csv` |
| Description | Simulated bovine movements between holdings and abattoirs, in the record layout used by the TrackMOOve dashboard. Sample for testing and demonstration only |
| Surveillance type / domain | Animal movements (sample for testing dashboards and network methods) |
| Spatial coverage | National; all 5 provinces and 42 districts |
| Spatial resolution | Farm (`origin_farm_id`, `destination_id` when a farm) for holdings; district for district names; abattoirs are located by district only |
| Temporal coverage | `2024-01-01` to `2024-06-28` (weekdays only) |
| Temporal resolution | Daily (one movement date per record) |
| Number of records | 15,000 movements; 10 abattoirs |
| Version added | Not yet released (sample for review) |
| Related outbreaks | None |

## How the data were generated

Simulated with a Python script kept outside this repository, using random seed `20240630`. Inputs: the farm register (`sentara_farm_register.csv`) and the district lookup (`sentara_admin_units.csv`), both from the Sentara repository.

- **Scope**: bovine movements only (dairy and beef cattle). Only the 16,383 holdings with at least one head of cattle can be a source or a farm destination.
- **Sources**: chosen with weight proportional to herd size raised to the power 0.6. Forty large beef farms (500 or more beef cattle) get a tenfold higher weight, to create some hub farms.
- **Purpose (`purpose`)**: drawn as Fattening 42%, Slaughter 28%, Breeding 20%, Other 10%. Fattening destinations are weighted by beef cattle, Breeding by dairy cattle, and Other by total cattle. In the dashboard's original Portuguese these are Engorda, Abate, Reprodução and Outros.
- **Abattoirs**: 10 fictional sites, 6 SIF, 2 SIE and 2 SIM. Slaughter movements go to them with weight 3 for SIF and 1 for SIE or SIM. One abattoir is placed in each province, and the rest in random districts.
- **Province preference**: 80% of farm-to-farm movements and 60% of slaughter movements stay within the source's province.
- **Head count (`head_count`)**: lognormal, median about 25 head for farm destinations and about 35 for abattoirs. Capped at the source farm's cattle and at least 1 head.
- **Dates**: uniform over weekdays in the period. Movements from a farm to itself are not allowed.
- **Movement IDs**: sequential from 600001, assigned after sorting by date. They do not refer to real permits.
- **Inspection type (`inspection_type`)**: `NA` for farm destinations; `SIF`, `SIE` or `SIM` for abattoirs.

Checks run by the script before writing: no self-movements, all farm and abattoir IDs exist in their source tables, abattoir inspection type matches the destination, head counts are at least 1, and dates fall inside the period.

## Variables

Movements file, `sentara_movements_sample_2024_h1.csv`:

| Column | Type | Unit | Description | Allowed values / range |
|---|---|---|---|---|
| `movement_id` | integer | — | Movement record identifier (simulated) | 600001 onward |
| `issue_date` | date | — | Date of the movement | `YYYY-MM-DD`, 2024-01-01 to 2024-06-28 |
| `purpose` | text | — | Purpose of the movement | `Fattening`, `Slaughter`, `Breeding`, `Other` |
| `origin_farm_id` | integer | — | Origin holding; equals `farm_id` in the farm register | 1–50000 (cattle farms only) |
| `destination_id` | integer | — | Destination holding or abattoir | 1–50000 (farms) or 50001–50010 (abattoirs) |
| `inspection_type` | text | — | Inspection type of the destination abattoir; `NA` if the destination is a farm | `SIF`, `SIE`, `SIM`, `NA` |
| `head_count` | integer | head | Number of cattle in the movement | 1 onward; never more than the origin's cattle |
| `origin_district` | text | — | District of the origin holding | District names from `sentara_admin_units.csv` |
| `destination_district` | text | — | District of the destination holding or abattoir | District names from `sentara_admin_units.csv` |

Abattoirs file, `sentara_abattoirs_sample.csv`:

| Column | Type | Unit | Description | Allowed values / range |
|---|---|---|---|---|
| `abattoir_id` | integer | — | Abattoir identifier used in `destination_id` | 50001–50010 |
| `name` | text | — | Placeholder name | `Abattoir 01` to `Abattoir 10` |
| `inspection_type` | text | — | Inspection type | `SIF`, `SIE`, `SIM` |
| `district_code`, `district` | text | — | District where the abattoir is placed | Codes in `sentara_admin_units.csv` |
| `province_code`, `province` | text | — | Province of that district | Five Sentara provinces |

Inspection type codes are kept from Brazil's official system: SIF is federal inspection, SIE is state inspection and SIM is municipal inspection.

## Known limitations

- **Simulated, not observed.** Nothing here describes a real animal, farm, or movement.
- **Original Portuguese names.** Earlier versions of the dashboard read Portuguese column names (`data_emissao`, `finalidade_fix`, `cod_prop_origem`, `cod_prop_destino`, `tipo`, `quant_animais`, `municipio.origem`, `municipio.destino`). The current dashboard and this file use the English names above. Older code will not read this file without renaming those columns.
- **Missing values use `NA`.** The repository conventions ask for empty cells for missing values. Empty cells would be the convention-compliant choice for `inspection_type`.
- **Abattoir IDs break the farm ID rule.** Abattoirs use 50001–50010, outside the `farm_id` range of 1–50000 in the conventions. This is intentional for the sample but should be settled before any release.
- **Abattoirs are not in the repository.** There is no facilities table yet (`02_animal_population/facilities/` is empty), so the abattoirs are invented for this sample.
- **Weak hub structure.** The 10 largest origin farms account for only about 4.6% of animals moved. The network will look less concentrated than the real Mato Grosso data described in the dashboard paper. Increasing the hub parameters in the generation script (kept outside this repository) would strengthen the hubs.
- **Movements are independent.** No animal chains, seasonality, disease events, or herd dynamics. Farms do not receive animals they later send on in a chain.
- **Only six months.** The sample covers the first half of 2024 and no other period.
- **Municipalities are districts.** Sentara has no municipal level, so the district columns contain district names.

## Suggested uses

Testing and demonstrating the TrackMOOve dashboard and network metrics, teaching exercises on animal movement networks, and checking the contact-chain tools on a dataset without confidentiality concerns. Not suited for estimating real transmission risk or for parameter inference.
