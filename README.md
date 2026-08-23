# stier-2022-value-of-information — monitoring VOI near tipping points (Proc B 2022)

*Created: 2026-08-23 (README added in the Phase 5 structure pass)*

| | |
|---|---|
| **Research program** | ocean-resilience |
| **Product** | Stier et al. 2022 *Proc. R. Soc. B* — "Avoiding critical thresholds through effective monitoring" |
| **Status** | completed (published) |
| **DOI** | [10.1098/rspb.2022.0526](https://doi.org/10.1098/rspb.2022.0526) — see `CITATION.cff` |
| **Data** | simulation only (operating models + MSE output in `output/`); no field data |

Management-strategy-evaluation model asking how the value of monitoring information
changes as a harvested/coral population approaches a tipping point (depensation).
Monitoring value rises the longer a resource lingers near the threshold; higher
monitoring precision raises yield and recovery capacity; precautionary buffers that
trigger increased precision as stocks decline cut monitoring cost while raising profit.

## Structure
- `code/` — numbered pipeline (`0_libraries.R` → `15_…`): operating models, MSE
  simulations, and per-figure simulation/visualization scripts
- `output/` — simulation `.Rdata`
- `CORAL_README.md` / `CORAL_VOI_IMPROVEMENTS.md` — the coral-restoration VOI
  extension built on the same framework (post-publication work)
- `drive-recovery/` — files recovered from the project's Drive folder (Phase 2)
