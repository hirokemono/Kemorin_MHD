!>@file   cal_momentum_eq_explicit.f90
!!@brief  module cal_momentum_eq_explicit
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2010
!
!>@brief Time integration for momentum equation by explicit scheme
!!
!!@verbatim
!!      subroutine sel_explicit_sph(i_step, dt, MHD_prop, sph_MHD_bc,   &
!!     &                            sph, ipol, rj_fld)
!!        type(sph_grids), intent(in) ::  sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param i_step  time step
!
      module cal_momentum_eq_explicit
!
      use m_precision
!
      use t_control_parameter
      use t_physical_property
      use t_spheric_parameter
!
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
!
      implicit  none
!
      private :: sel_explicit_sph_momentum
      private :: sel_explicit_sph_induction
      private :: sel_explicit_sph_temp, sel_explicit_sph_comp
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph(i_step, dt, MHD_prop, sph_MHD_bc,     &
     &                            sph, ipol, rj_fld)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(sph_grids), intent(in) ::  sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_explicit_sph_momentum(i_step, dt,                        &
     &    MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U, sph, ipol, rj_fld)
      call sel_explicit_sph_induction(i_step, dt, MHD_prop%cd_prop,     &
     &                                ipol, rj_fld)
!!
      call sel_explicit_sph_temp(i_step, dt,                            &
     &    MHD_prop%ht_prop, sph_MHD_bc%sph_bc_T, sph, ipol, rj_fld)
      call sel_explicit_sph_comp(i_step, dt,                            &
     &    MHD_prop%cp_prop, sph_MHD_bc%sph_bc_C, sph, ipol, rj_fld)
!
      end subroutine sel_explicit_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_momentum(i_step, dt,                  &
     &          fl_prop, sph_bc_U, sph, ipol, rj_fld)
!
      use cal_vorticity_terms_adams
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(sph_grids), intent(in) ::  sph
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(fl_prop%iflag_scheme .eq. id_explicit_euler) then
        call cal_vorticity_eq_euler(sph%sph_rj, fl_prop, sph_bc_U,      &
     &      ipol%base, ipol%exp_work, ipol%diffusion,                   &
     &      dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(i_step .eq. 1) then
        call cal_vorticity_eq_euler(sph%sph_rj, fl_prop, sph_bc_U,      &
     &      ipol%base, ipol%exp_work, ipol%diffusion,                   &
     &      dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call set_ini_adams_inertia(fl_prop, ipol%exp_work,              &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call cal_vorticity_eq_adams(sph%sph_rj, fl_prop, sph_bc_U,      &
     &      ipol%base, ipol%exp_work, ipol%diffusion,                   &
     &      dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_explicit_sph_momentum
!
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_induction(i_step, dt, cd_prop,        &
     &                                      ipol, rj_fld)
!
      use cal_explicit_terms
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cd_prop%iflag_Bevo_scheme .eq. id_no_evolution) return
      if(cd_prop%iflag_Bevo_scheme .eq. id_explicit_euler) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                  'cal_diff_induction_MHD_euler'
          call cal_diff_induction_MHD_euler                             &
     &       (cd_prop, ipol%base, ipol%forces, ipol%diffusion,          &
     &        dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(i_step .eq. 1) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                  'cal_diff_induction_MHD_euler'
          call cal_diff_induction_MHD_euler                             &
     &       (cd_prop, ipol%base, ipol%forces, ipol%diffusion,          &
     &        dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
          if(iflag_debug .gt. 0) write(*,*)                             &
     &              'set_ini_adams_mag_induct'
          call set_ini_adams_mag_induct(ipol%exp_work, ipol%forces,     &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'cal_diff_induction_MHD_adams'
          call cal_diff_induction_MHD_adams(cd_prop,                    &
     &        ipol%base, ipol%exp_work, ipol%forces, ipol%diffusion,    &
     &        dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_explicit_sph_induction
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_temp(i_step, dt,                      &
     &          ht_prop, sph_bc_T, sph, ipol, rj_fld)
!
      use explicit_scalars_sph
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(sph_grids), intent(in) ::  sph
      type(scalar_property), intent(in) :: ht_prop
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ht_prop%iflag_scheme .eq. id_no_evolution) return
      if(ht_prop%iflag_scheme .eq. id_explicit_euler) then
        call explicit_temp_sph_euler                                    &
     &     (dt, sph%sph_rj, ht_prop, sph_bc_T,                          &
     &      ipol%base, ipol%forces, ipol%diffusion, rj_fld)
      else if(i_step .eq. 1) then
        call explicit_temp_sph_euler                                    &
     &     (dt, sph%sph_rj, ht_prop, sph_bc_T,                          &
     &      ipol%base, ipol%forces, ipol%diffusion, rj_fld)
        call first_temp_prev_step_adams                                 &
     &     (sph%sph_rj, ht_prop, sph_bc_T,                              &
     &      ipol%base, ipol%exp_work, ipol%forces, rj_fld)
      else
        call explicit_temp_sph_adams                                    &
     &     (dt, sph%sph_params, sph%sph_rj, ht_prop, sph_bc_T,          &
     &      ipol%base, ipol%exp_work, ipol%forces, ipol%diffusion,      &
     &      rj_fld)
      end if
!
      end subroutine sel_explicit_sph_temp
!
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph_comp(i_step, dt,                      &
     &          cp_prop, sph_bc_C, sph, ipol, rj_fld)
!
      use explicit_scalars_sph
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(sph_grids), intent(in) ::  sph
      type(scalar_property), intent(in) :: cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_C
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cp_prop%iflag_scheme .eq. id_no_evolution) return
      if(cp_prop%iflag_scheme .eq. id_explicit_euler) then
        call explicit_comp_sph_euler                                    &
     &     (dt, sph%sph_rj, cp_prop, sph_bc_C,                          &
     &      ipol%base, ipol%forces, ipol%diffusion, rj_fld)
!
      else if(i_step .eq. 1) then
        call explicit_comp_sph_euler                                    &
     &     (dt, sph%sph_rj, cp_prop, sph_bc_C,                          &
     &      ipol%base, ipol%forces, ipol%diffusion, rj_fld)
        call first_comp_prev_step_adams                                 &
     &     (sph%sph_rj, cp_prop, sph_bc_C,                              &
     &      ipol%base, ipol%exp_work, ipol%forces, rj_fld)
      else
        call explicit_comp_sph_adams                                    &
     &     (dt, sph%sph_params, sph%sph_rj, cp_prop, sph_bc_C,          &
     &      ipol%base, ipol%exp_work, ipol%forces, ipol%diffusion,      &
     &      rj_fld)
      end if
!
      end subroutine sel_explicit_sph_comp
!
! ----------------------------------------------------------------------
!
      end module cal_momentum_eq_explicit
