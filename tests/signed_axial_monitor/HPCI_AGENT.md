# Task for the AI agent on HPCI: validate the signed axial-field monitor

You have access to the HPCI source tree, compiler/MPI modules, scheduler and
restart data. Perform the clean-build, short-run/restart/performance, and
production-executable checks described below. The final deliverable is an
HPCI readiness report with commands, job IDs, logs, results and remaining issues.
This request authorizes isolated test builds and short scheduler jobs. A long
production run and replacement of the established production executable are
not requested in this handoff.

## Context and patch scope

Base commit: `a75698cc79132fbc32fa977f8bc71e62c51c63d9`.
Local branch: `feature/signed-axial-field-monitor-hpci`.
Patch: `signed-axial-field-monitor.patch`; feature commit and checksum are in
the accompanying handoff manifest.

The patch adds the optional text control item:

```text
begin sph_monitor_ctl
  signed_axial_field_integral_prefix 'monitor/signed_Bz'
end sph_monitor_ctl
```

Merge this item into the existing block; do not replace the other settings.
Create the parent directory. Absence of the keyword disables the calculation.
Output at the ordinary `i_step_check_ctl` interval is:

```text
t_step time Bz_volume_integral Bz_positive_integral Bz_negative_magnitude_integral
```

These are nondimensional ICB-CMB volume integrals of `Bz`, `max(Bz,0)` and
`max(-Bz,0)`. The independently accumulated first column of field values
must equal positive minus negative magnitude within scaled roundoff.
The diagnostic uses the existing nonlinear physical buffer; no extra transform.

Read site/repository instructions and inspect the current worktree before any
mutation. Preserve unrelated changes, production binaries, controls and data.
Do not delete or reset an existing checkout. Keep build/test artifacts in a
new isolated worktree and new scheduler work directories.

## Preparation: apply the feature in isolation

If it has not already been applied, use a clean branch from the exact base:

```sh
git cat-file -e a75698cc79132fbc32fa977f8bc71e62c51c63d9^{commit}
git worktree add -b hpci/signed-axial-field-validation \
  /YOUR/SCRATCH/signed-axial-source a75698cc79132fbc32fa977f8bc71e62c51c63d9
cd /YOUR/SCRATCH/signed-axial-source
git apply --check /YOUR/TRANSFER/signed-axial-field-monitor.patch
git am /YOUR/TRANSFER/signed-axial-field-monitor.patch
```

Substitute real site paths. If the production branch has additional required
changes, inspect them and integrate into a separate test branch; do not reset
production to the base or drop those changes. Resolve conflicts by preserving
both features and report them. `git am` may create a different commit hash because committer metadata changes.
On the exact base, verify that `git rev-parse HEAD^{tree}` matches the manifest
source-tree hash; do not require identical commit hashes after `git am`. If the exact base
is unavailable, report that and fetch the relevant history through the site's
normal authorized repository workflow; do not assume a different base equivalent.

## Step 4: identify the production executable before scheduling tests

Do this early, even though it was listed as step 4 in the discussion.
Inspect the actual submission script, executable path/checksum and source call
chain. Identify whether it uses normal `SPH_analyze_MHD` or SGS time evolution.
Inspect the restart's integer width/endian, domain count, actual radial bounds,
azimuthal folding and monitor cadence. Preserve the simulation nondimensionalization.

Supported hooks in this patch:

- Normal time evolution sharing `SPH_analyzer_MHD`: normal noviz/psf/vizs/mini
  variants. Establish the exact filename via the site's generated Makefile.
- `sph_snapshot`: SGS-capable snapshot path using `SPH_analyzer_SGS_snap`.
- Normal snapshot noviz/psf/vizs variants sharing `SPH_analyzer_snap_w_vizs`.

**SGS time evolution in `SPH_analyzer_SGS_MHD` is not hooked.** Parsing the
prefix is not proof of support. If that is the production executable, report
this as a production blocker and identify the small initialization/sampling
hook needed. Do not substitute a normal solver for an SGS run or silently claim
readiness. Snapshot validation can proceed if appropriate.

Full-sphere ICB index zero and folded-grid runtime have not been validated;
assess these if production uses them. Do not use shell-only reference values
as proof of full-sphere support.

## Step 2: clean Intel/MPI build

1. Record `git rev-parse HEAD`, branch, loaded modules, compiler and MPI versions,
   compiler flags, FFT/BLAS/HDF5/zlib linkage and the precision/integer profile.
2. Use the established HPCI configure/build recipe in the isolated checkout.
   The tracked generated Makefiles may contain a different machine's paths:
   regenerate them from the site's configuration rather than using them blindly.
   Inspect `./configure --help`, the working site recipe, `configure.ac`, and the
   root/source Makefiles. Load the site's actual Intel and MPI versions rather
   than inventing module names. Do not change production compiler options or
   integer width just to make a test pass.
3. Regenerate dependencies with the appropriate `make makemake` (or
   `make makemake64` for the actual 64-bit profile), then build the required
   solver and snapshot targets from empty test object/module directories.
   The source Makefile discovers `.f90` files automatically. Confirm
   `t_signed_axial_field_monitor.o` is included in dependencies and archives.
   Do not reuse old `.mod`, `.o`, or `.a` files from another compiler/profile.
4. Save configure, compilation and link logs. Explicitly check OpenMP array
   reduction/collapse, assumed-shape arrays, `newunit`, and MPI argument kinds.
   These are already accepted by the local GNU toolchain, but Intel/HPCI must
   be checked independently. Inspect compiler warnings affecting correctness.
5. Run the synthetic kernel tests inside an approved compute allocation.
   `tests/signed_axial_monitor/run.sh` accepts compiler and launcher overrides:

```sh
TEST_MPIFC=mpiifort \
TEST_FFLAGS='-O0 -g -qopenmp -check all -traceback' \
TEST_MPIRUN=mpiexec \
TEST_MPI_RANKS='1 2 4' \
bash tests/signed_axial_monitor/run.sh /YOUR/ISOLATED/OBJECT_DIRECTORY
```

This is an example for Intel classic, not a guarantee those wrapper/flag names
exist on the site. Substitute its verified wrapper (e.g. the installed ifort/ifx
MPI wrapper), bounds-check flags and launcher. `TEST_MPIRUN_FLAGS` accepts extra
launcher arguments, `TEST_OMP_THREADS` defaults to 2, and `TEST_MPI_RANKS` selects
rank counts. Adapt invocation to the scheduler allocation; do not launch MPI on
a login node or oversubscribe compute resources. All dependency objects must
match the selected compiler, MPI and integer profile. Preserve test output.

## Step 3: short snapshot and actual-solver checks

### A. Real restart snapshots

Use an isolated output directory and fresh monitor prefix for each test. Preserve
original restart inputs and all original controls/output. Merged restart files
can enforce their saved MPI domain count. The local real-data tests required
160 ranks in 5 x 32 decomposition; a six-rank attempt was rejected before the
calculation. Use the restart's actual decomposition, not a guessed smaller one.

If the same low-resolution files are available on HPCI, process exactly these
saved states using `sph_snapshot`, with start=finish equal to the full saved step:

| File index | Full step | Time | Mz | Pplus | Pminus |
|---|---:|---:|---:|---:|---:|
| 1561 | 312200000 | 624.3999986076 | -1.24594476212182 | 5.28818294754386 | 6.53412770966569 |
| 1603 | 320600000 | 641.1999985652 | 5.46426551892421 | 11.0796119380440 | 5.61534641911981 |

Those files have L=63, 225 radial samples, 96 Gaussian latitudes and 192
longitudes. Their original restart increment is 200000. Read actual time from
the restart. Do not compare a high-resolution parent restart to these numbers.
The exact local results are in `real_restart_results.json`. If these files are
not available, use an available restart and independently exported snapshot;
state explicitly that the known-reference comparison was not performed.

Compare all three integrals and timestamps. As an initial cross-toolchain
criterion for the same field/grid, use `atol=1e-10, rtol=1e-9`; investigate
larger differences rather than loosening tolerance without an explanation.
Finite values and nonnegative Pplus/Pminus are required. Check independently
that `abs(Mz-Pplus+Pminus) <= 1e-11*max(Pplus+Pminus,1)` at every record.

Export a full shell-only legacy ASCII VTK magnetic vector and run:

```sh
python tests/signed_axial_monitor/check_snapshot.py \
  field/out.1561.vtk.gz monitor/signed_Bz.dat 312200000 96 192
```

The VTK filename is an example; derive the real field-file index from the actual
`i_step_field_ctl` interval. The checker's arguments are actual VTK path,
monitor path, full timestep, Ntheta and Nphi. It excludes duplicated nodes and
pole copies and matches spherical quadrature. For different output formats,
perform equivalent independent physical-grid integration rather than assuming
VTK element integration is identical.

### B. Short production-solver restart test

Run the identified supported production solver for a bounded window, initially
200 steps and at most 1000 if needed for timing statistics and within the site's
short-job resource budget. Use a copy of the production control with the same
physics, grid, decomposition, dt policy and MPI/OpenMP allocation. Redirect all
outputs into a new test directory. Make only the diagnostic and short-test
cadence/end-step changes necessary. Enable a monitor interval giving several
records and arrange one or two selected full-field validation times.

Run paired tests from the same input with the same patched binary:

- monitor keyword absent (disabled);
- monitor enabled, otherwise identical.

Check every record's finiteness, positivity, closure, expected cadence and
saved/advanced timestamps. The enabled monitor must not change the evolving
fields or existing diagnostics beyond normal reproducibility tolerance.
Disabled mode must not create the new monitor file. Compare at least one actual
solver output against independent snapshot integration, not only snapshot mode.

### C. Checkpoint/resume

Arrange a checkpoint during the short test and continue from it in a separate
staged directory. Original restart inputs must never be output targets.
**Changing the restart interval changes the filename index:** this code uses
`set_IO_step(full_step, rst_step) = full_step / increment`. If a shorter test
checkpoint interval is used, stage a copied input under the index expected by
the modified interval; preserve the saved header step/time. Never blindly
change `i_step_rst_ctl` and assume the original filename still matches.

Check initial/resumed timestamps, closure, file headers and appended records.
An overlap at the restart step is allowed by the current append behavior and
must be documented/deduplicated for analysis. Compare continued state with the
uninterrupted test, allowing the solver's established restart tolerance; if
restart differences already exist, compare the same protocol with the monitor
disabled so they are not misattributed to this diagnostic. Check that the first
resumed diagnostic uses reconstructed current magnetic data, not a stale buffer.

### D. Performance

Measure disabled/enabled runs at the intended production monitor cadence,
with equal thread/rank placement and identical other output schedules. Avoid
large snapshot output in the timing-only pair. Exclude startup and warm-up from
per-step timing, use multiple comparable windows/repeats when practical, and
record variability. Report median disabled/enabled time per timestep, absolute
and percentage overhead, extra monitor storage, and whether differences exceed
measurement noise. Include I/O and MPI reduction cost. Do not assert negligible
cost only because there is no extra spherical transform. Use the site's/user's
performance budget; if none is specified, report measurements for review rather
than inventing a pass threshold.

## Required report and completion criteria

Write `HPCI_SIGNED_AXIAL_READINESS.md` in the handoff/test directory with:

- source commit, exact executable/source path and checksum;
- compiler/MPI/precision profile, build recipe and clean-build logs;
- production solver support: normal vs SGS, geometry and actual domain count;
- short-job IDs, controls, input restart checksums, saved steps/times;
- synthetic, known-reference, snapshot and actual-solver results;
- maximum closure/snapshot errors, disabled behavior and restart findings;
- reproducibility and measured overhead with uncertainty;
- fixes made on the isolated branch, if any, and their revalidation;
- explicit `READY`, `NOT READY`, or `INCOMPLETE`, with the evidence and limits.

Declare READY only for the tested supported production executable after clean
build, appropriate short-run, restart and correctness checks pass, and the
performance result is acceptable to the stated budget or subsequent review.
Compile errors, unsupported SGS stepping, stale timestamps, unexplained field
changes or failed volume validation are blockers. Missing tests should be
reported as incomplete, not converted into passes. Finish the report before
requesting any long production run or promotion of the executable.
