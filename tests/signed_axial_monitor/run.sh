#!/usr/bin/env bash
# Compiles the production kernel using existing, matching dependency objects.
set -euo pipefail
repo_dir=$(cd "$(dirname "$0")/../.." && pwd)
object_dir=${1:-"$repo_dir/work"}
object_dir=$(cd "$object_dir" && pwd)
test_dir=$(mktemp -d "${TMPDIR:-/tmp}/signed-axial-test.XXXXXX")
trap 'rm -rf "$test_dir"' EXIT
read -r -a compiler <<< "${TEST_MPIFC:-mpif90}"
read -r -a flags <<< "${TEST_FFLAGS:--O0 -g -fopenmp -fcheck=all}"
read -r -a launcher <<< "${TEST_MPIRUN:-mpirun}"
read -r -a launch_flags <<< "${TEST_MPIRUN_FLAGS:-}"
read -r -a ranks_list <<< "${TEST_MPI_RANKS:-1 2 4}"
cd "$test_dir"
"${compiler[@]}" "${flags[@]}" -I "$object_dir" -c \
  "$repo_dir/MHD/Fortran_src/MHD_src/sph_MHD/t_signed_axial_field_monitor.f90"
"${compiler[@]}" "${flags[@]}" -I . -I "$object_dir" \
  "$repo_dir/tests/signed_axial_monitor/test_signed_axial_monitor.f90" \
  t_signed_axial_field_monitor.o \
  "$object_dir/radial_int_for_sph_spec.o" \
  "$object_dir/calypso_mpi.o" "$object_dir/calypso_mpi_real.o" \
  "$object_dir/transfer_to_long_integers.o" \
  "$object_dir/set_parallel_file_name.o" -o test_monitor
for ranks in "${ranks_list[@]}"; do
  [[ "$ranks" =~ ^[1-9][0-9]*$ ]] || { echo "Invalid MPI rank count: $ranks" >&2; exit 1; }
  mkdir "run$ranks"
  (cd "run$ranks"; OMP_NUM_THREADS="${TEST_OMP_THREADS:-2}" \
    "${launcher[@]}" "${launch_flags[@]}" -np "$ranks" ../test_monitor)
done
