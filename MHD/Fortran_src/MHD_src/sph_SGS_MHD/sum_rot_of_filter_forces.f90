!>@file   sum_rot_of_filter_forces.f90
!!@brief  module sum_rot_of_filter_forces
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine sum_filter_forces_to_explicit                        &
!!     &         (fl_prop, ipol, ipol_LES, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
!
      module sum_rot_of_filter_forces
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_physical_property
      use t_base_field_labels
      use t_explicit_term_labels
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
      subroutine sum_filter_forces_to_explicit                          &
     &         (fl_prop, ipol, ipol_LES, rj_fld)
!
      use cal_vorticity_terms_adams
!
      type(fluid_property), intent(in) :: fl_prop
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel
      if(fl_prop%iflag_4_filter_inertia) then
        call add_rot_advection_to_force                                 &
     &     (ipol%exp_work, ipol_LES%rot_frc_by_filter,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(fl_prop%iflag_4_filter_lorentz) then
        call add_lorentz_to_vort_force                                  &
     &     (ipol%exp_work, ipol_LES%rot_frc_by_filter,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(fl_prop%iflag_4_filter_gravity) then
        call add_buoyancy_to_vort_force                                 &
     &     (ipol%exp_work, ipol_LES%rot_frc_by_filter%i_buoyancy,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(fl_prop%iflag_4_filter_comp_buo) then
        call add_buoyancy_to_vort_force                                 &
     &     (ipol%exp_work, ipol_LES%rot_frc_by_filter%i_comp_buo,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!$omp end parallel
!
      end subroutine sum_filter_forces_to_explicit
!
!*   ------------------------------------------------------------------
!
      end module sum_rot_of_filter_forces
