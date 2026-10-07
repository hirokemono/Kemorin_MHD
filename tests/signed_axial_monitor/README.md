# Signed axial magnetic-field monitor

For the normal `sph_mhd` solver (the noviz, psf, vizs, and mini entry
points sharing `SPH_analyzer_MHD`), add this item to `sph_monitor_ctl`:

```text
begin sph_monitor_ctl
  signed_axial_field_integral_prefix 'monitor/signed_Bz'
  ! existing monitor settings ...
end sph_monitor_ctl
```

Create the parent directory before running. The solver appends to
`monitor/signed_Bz.dat` at the ordinary `i_step_check_ctl` monitor interval,
including the initial state when its step falls on that interval. Omit the
keyword to disable both computation and output. Existing files are appended;
use a new prefix when you want a fresh series. A restart can repeat a saved
step, so deduplicate overlapping steps during analysis.

Columns are `t_step`, `time`, `Bz_volume_integral`, `Bz_positive_integral`,
and `Bz_negative_magnitude_integral`. Values are nondimensional volume
integrals over ICB-CMB, not energies or volume averages. The signed value is
accumulated independently; it should equal positive minus negative magnitude
within floating-point roundoff. Choose old/opposite polarity afterwards.

The monitor uses the normal nonlinear backward buffer in spherical
components: `Bz = Br*cos(theta) - Btheta*sin(theta)`. It adds no spherical
transform. Weights use the existing global nonuniform radial trapezoid
matrix times `r²`, Gaussian angular weights normalized to sum to two, and
`2*pi/Nphi_global`. Global radial weights include intervals spanning MPI
partitions. Grid strides and local-to-global indices are respected. Pole
copies in visualization meshes are excluded. Azimuthal folding is treated
as repeated identical sectors in the full-shell integral.

The monitor also supports `sph_snapshot` (the SGS-capable snapshot path),
`sph_snapshot_noviz`, `sph_snapshot_psf`, and `sph_snapshot_vizs`. Each
reconstructed snapshot is sampled when its saved step meets the ordinary
monitor interval. Set snapshot start and finish to the saved step for a
single-file test, preserving the restart-file increment. Merged restart
files enforce their saved MPI domain count. SGS time-evolution solvers do
not yet sample the diagnostic.
The C/GUI control editor does not expose the new setting yet; use the text
control file.

## Build and tests

The source directory's Makefile discovers `.f90` files automatically. Regenerate
build dependencies using the repository's `make makemake` workflow before
rebuilding `sph_mhd`, so the new module is included and dependent type modules
are recompiled. The generated `work/Makefile` from an older build will not
contain the new module until regenerated.

With the normal compiled dependency objects in `work`, run:

```sh
bash tests/signed_axial_monitor/run.sh
```

An alternate dependency directory can be passed as the first argument. Tests
compile the production kernel with bounds checks and test positive, negative,
and mixed-sign axial fields, nonuniform radial weights, shell exclusion,
longitude-fast storage, empty partitions, 1/2/4-rank decomposition, output
closure, timestamps, and the disabled path.

For an independent exported-snapshot check (Python and NumPy required):

```sh
python tests/signed_axial_monitor/check_snapshot.py \
  field/out.1.vtk.gz monitor/signed_Bz.dat 1 12 24
```

The final arguments are step, Gaussian latitude count, and longitude count.
This checker expects a complete shell-only legacy ASCII VTK volume snapshot
with `magnetic_field`, and matches spherical-grid quadrature using unique
physical nodes. It excludes pole copies and duplicate mesh nodes. It does
not integrate the hexahedral mesh approximation.

Validation performed in an isolated build: synthetic tests on 1/2/4 ranks;
a two-step dynamo benchmark at L=7, 16 configured radial intervals, 12 Gaussian latitudes,
24 longitudes on 1/2/6 ranks; snapshot checks at steps 1 and 2; and a disabled
solver run. Solver integrals differed by less than `6e-13` between decompositions,
and exported-snapshot quadrature agreed within `1e-11` absolute. Production
performance, folded-grid runtime validation, full-sphere behavior, and reversal
physics have not been tested.

Real-data validation of run6/rst.1561 and run11/rst.1603 is recorded in
`VALIDATION.md`. Both completed on
160 ranks at their original spatial resolution, and independent Cartesian
VTK integration agreed within `2.1e-11` absolute.

HPCI clean-build, executable selection, and restart/performance validation
instructions are in `HPCI_AGENT.md`.
