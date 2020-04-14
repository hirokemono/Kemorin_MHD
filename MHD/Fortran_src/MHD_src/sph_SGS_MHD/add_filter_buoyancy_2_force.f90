!>@file   add_filter_buoyancy_2_force.f90
!!@brief  module add_filter_buoyancy_2_force
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine add_filtered_buo_to_explicit(fl_prop, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
!
      module add_filter_buoyancy_2_force
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_physical_property
      use t_phys_address
      use t_phys_data
!
      implicit none
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine add_filtered_buo_to_explicit(fl_prop, ipol, rj_fld)
!
      use cal_vorticity_terms_adams
!
      type(fluid_property), intent(in) :: fl_prop
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(fl_prop%iflag_4_filter_gravity .gt. id_turn_OFF) then
!$omp parallel
        call add_buoyancy_to_vort_force                               &
     &     (ipol%exp_work, ipol%rot_frc_by_filter%i_buoyancy,         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!$omp end parallel
      end if
      if(fl_prop%iflag_4_filter_comp_buo .gt. id_turn_OFF) then
!$omp parallel
        call add_buoyancy_to_vort_force                               &
     &     (ipol%exp_work, ipol%rot_frc_by_filter%i_comp_buo,         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!$omp end parallel
      end if
!
      end subroutine add_filtered_buo_to_explicit
!
!*   ------------------------------------------------------------------
!
      end module add_filter_buoyancy_2_force
