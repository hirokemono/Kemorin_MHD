# Validation evidence and limits

The diagnostic calculates independent weighted sums of `Bz`, `max(Bz,0)`,
and `max(-Bz,0)` over ICB-CMB, using physical RTP samples already available
from the nonlinear backward transform. Its output is first-order volume
integrals, not energy or a grid average. It adds no spherical transform.

## Local evidence

- GNU/Open MPI: production kernel compiled with bounds checks; synthetic
  positive, negative and mixed-sign fields passed on 1/2/4 ranks, including
  radial, latitude and longitude partitions, empty partitions and changed strides.
- Two-step normal solver benchmark: L=7, 16 configured radial intervals,
  12 Gaussian latitudes, 24 longitudes, on 1/2/6 ranks. Differences between
  decompositions were below 6e-13; independent snapshot comparison below 1e-11.
- Disabled normal solver: no diagnostic file was created.
- Standard `sph_snapshot`: two real low-resolution restarts passed on their
  original 160 ranks (5 radial x 32 horizontal), L=63, 225 actual radial samples,
  96 Gaussian latitudes and 192 longitudes. Spatial resolution was preserved.

| Restart | Full step | Saved time | Mz | Pplus | Pminus |
|---|---:|---:|---:|---:|---:|
| run6 / rst.1561 | 312200000 | 624.3999986076 | -1.24594476212182 | 5.28818294754386 | 6.53412770966569 |
| run11 / rst.1603 | 320600000 | 641.1999985652 | 5.46426551892421 | 11.0796119380440 | 5.61534641911981 |

Independent integration used exported Cartesian `magnetic_field` component 3,
unique physical-grid nodes and separately reconstructed spherical quadrature.
It excluded visualization pole copies and duplicate nodes. Maximum absolute
errors were 1.0249e-11 for run6 and 2.0611e-11 for run11. Closure residuals
`Mz-Pplus+Pminus` were 0 and 2.6645e-15. Exact values are in
`real_restart_results.json`. The large restart/VTK files are not part of this patch.

## Handoff branch

This feature branch starts at `a75698cc79132fbc32fa977f8bc71e62c51c63d9`, which
already contains the tangent-cylinder monitor. Only the axial diagnostic,
its control/hook changes, tests and handoff documentation are included.
Unrelated working-tree changes, binaries and local build settings are excluded.
The numerical/source integration was tested in the earlier local working tree;
the isolated handoff branch's kernel tests were rerun against local compiled
dependency objects. A clean build of the entire handoff branch remains an HPCI
validation task; do not mistake the reused local dependency objects for that test.

## What has not been established

- Intel compiler/MPI compatibility and clean HPCI build.
- Production-cadence overhead and checkpoint/restart behavior on HPCI.
- SGS time-evolution support: the prefix can be parsed, but there is no SGS
  timestep sampling hook yet. SGS-capable snapshot support is a separate path.
- Full-sphere (ICB index zero), folded-grid runtime or higher-resolution convergence.
- A fixed Mz/g10 ratio: both inner and outer boundary terms matter for a shell.
- Complete reversal history from only two snapshots.

Use `HPCI_AGENT.md` for the remaining build, support and short-run checks.
