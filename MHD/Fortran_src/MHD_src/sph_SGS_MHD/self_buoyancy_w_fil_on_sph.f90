!>@file   self_buoyancy_w_fil_on_sph.f90
!!@brief  module self_buoyancy_w_fil_on_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate buoyancy at specific radius
!!
!!@verbatim
!!      subroutine r_buoyancy_on_sphere_w_filter(kr, sph_rj, ipol,      &
!!     &          fl_prop, ref_param_T, ref_param_C, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param kr  Radial grid ID
!
      module self_buoyancy_w_fil_on_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
      private :: filter_r_buoyancies_on_sphere
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine r_buoyancy_on_sphere_w_filter(kr, sph_rj, ipol,        &
     &          fl_prop, ref_param_T, ref_param_C, rj_fld)
!
      use t_control_parameter
      use t_physical_property
      use t_reference_scalar_param
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
!
      use self_buoyancy_on_sphere
!
      integer(kind= kint), intent(in) :: kr
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      call r_buoyancy_on_sphere(kr, sph_rj, ipol%base, ipol%div_forces, &
     &    fl_prop, ref_param_T, ref_param_C, rj_fld)
      call filter_r_buoyancies_on_sphere                                &
     &   (kr, sph_rj, ipol%filter_fld, ipol%div_frc_by_filter,          &
     &    fl_prop, ref_param_T, ref_param_C, rj_fld)
!
      end subroutine r_buoyancy_on_sphere_w_filter
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine filter_r_buoyancies_on_sphere                          &
     &         (kr, sph_rj, ipol_fil, ipol_div_fil_frc,                 &
     &          fl_prop, ref_param_T, ref_param_C, rj_fld)
!
      use t_physical_property
      use t_reference_scalar_param
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
!
      use self_buoyancy_on_sphere
!
      integer(kind= kint), intent(in) :: kr
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_fil
      type(base_force_address), intent(in) :: ipol_div_fil_frc
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (fl_prop%iflag_4_filter_gravity .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_r_buoyancy_on_sphere'
        call cal_r_buoyancy_on_sphere(kr, fl_prop%coef_buo,             &
     &      ipol_fil%i_temp, ipol_div_fil_frc%i_buoyancy,               &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if (fl_prop%iflag_4_filter_comp_buo .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_r_buoyancy_on_sphere'
        call cal_r_buoyancy_on_sphere(kr, fl_prop%coef_comp_buo,        &
     &      ipol_fil%i_light, ipol_div_fil_frc%i_comp_buo,              &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine filter_r_buoyancies_on_sphere
!
!-----------------------------------------------------------------------
!
      end module self_buoyancy_w_fil_on_sph
