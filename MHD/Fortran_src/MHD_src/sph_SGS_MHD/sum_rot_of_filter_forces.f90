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
!!     &         (fl_prop, ipol_exp, ipol_div_fil_frc, rj_fld)
!!      subroutine sum_div_of_filtered_forces                           &
!!     &         (fl_prop, ipol_base, ipol_div_fil_frc, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(base_force_address), intent(in) :: ipol_div_fil_frc
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
      use t_base_force_labels
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
     &         (fl_prop, ipol_exp, ipol_div_fil_frc, rj_fld)
!
      use cal_vorticity_terms_adams
!
      type(fluid_property), intent(in) :: fl_prop
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_div_fil_frc
      type(phys_data), intent(inout) :: rj_fld
!
      logical :: flag
!
      flag =   fl_prop%iflag_4_filter_inertia                           &
     &    .or. fl_prop%iflag_4_filter_lorentz                           &
     &    .or. fl_prop%iflag_4_filter_gravity                           &
     &    .or. fl_prop%iflag_4_filter_comp_buo
      if(flag .eqv. .FALSE.) return
!
!$omp parallel
      if(fl_prop%iflag_4_filter_inertia) then
        call subtract_advection_to_force                                &
     &     (ipol_exp%i_forces, ipol_div_fil_frc%i_m_advect,             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(fl_prop%iflag_4_filter_lorentz) then
        call add_each_force_to_rot_forces                               &
     &     (ipol_exp%i_forces, ipol_div_fil_frc%i_lorentz,              &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(fl_prop%iflag_4_filter_gravity) then
        call add_buoyancy_to_vort_force                                 &
     &     (ipol_exp%i_forces, ipol_div_fil_frc%i_buoyancy,             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(fl_prop%iflag_4_filter_comp_buo) then
        call add_buoyancy_to_vort_force                                 &
     &     (ipol_exp%i_forces, ipol_div_fil_frc%i_comp_buo,             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!$omp end parallel
!
      end subroutine sum_filter_forces_to_explicit
!
!*   ------------------------------------------------------------------
!
      subroutine sum_div_of_filtered_forces                             &
     &         (fl_prop, ipol_base, ipol_div_fil_frc, rj_fld)
!
      use cal_div_of_forces
!
      type(fluid_property), intent(in) :: fl_prop
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_div_fil_frc
      type(phys_data), intent(inout) :: rj_fld
!
      logical :: flag
!
      flag =   fl_prop%iflag_4_filter_inertia                           &
     &    .or. fl_prop%iflag_4_filter_lorentz                           &
     &    .or. fl_prop%iflag_4_filter_gravity                           &
     &    .or. fl_prop%iflag_4_filter_comp_buo
      if(flag .eqv. .FALSE.) return
!
!$omp parallel
!      if(fl_prop%iflag_4_filter_inertia) then
!        call add_div_advection_to_force                                &
!     &     (ipol_base%i_press, ipol_div_fil_frc%i_m_advect,            &
!     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!      end if
!
      if(fl_prop%iflag_4_filter_lorentz) then
        call add_term_to_div_force                                      &
     &     (ipol_base%i_press, ipol_div_fil_frc%i_lorentz,              &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(fl_prop%iflag_4_filter_gravity) then
        call add_term_to_div_force                                      &
     &     (ipol_base%i_press, ipol_div_fil_frc%i_buoyancy,             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(fl_prop%iflag_4_filter_comp_buo) then
        call add_term_to_div_force                                      &
     &     (ipol_base%i_press, ipol_div_fil_frc%i_comp_buo,             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!$omp end parallel
!
      end subroutine sum_div_of_filtered_forces
!
! ----------------------------------------------------------------------
!
      end module sum_rot_of_filter_forces
