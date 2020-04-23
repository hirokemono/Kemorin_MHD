!>@file   cal_sol_sph_SGS_MHD_crank.f90
!!@brief  module cal_sol_sph_SGS_MHD_crank
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Update fields for MHD dynamo model
!!
!!@verbatim
!!      subroutine s_cal_sol_sph_MHD_crank                              &
!!     &         (dt, sph_rj, r_2nd, MHD_prop, sph_MHD_bc, leg,         &
!!     &          ipol, sph_MHD_mat, rj_fld)
!!      subroutine set_sph_field_to_start(sph_rj, r_2nd,                &
!!     &          MHD_prop, sph_MHD_bc, leg, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine update_after_magne_sph(sph_rj, r_2nd,                &
!!     &          cd_prop, sph_bc_B, leg, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module cal_sol_sph_SGS_MHD_crank
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use const_sph_radial_grad
      use const_sph_rotation
      use const_sph_diffusion
!
      use t_control_parameter
      use t_physical_property
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_radial_matrices_sph_MHD
      use t_coef_fdm2_MHD_boundaries
      use t_work_4_sph_trans
!
      implicit none
!
      private :: update_after_vorticity_sph
      private :: update_after_heat_sph, update_after_composit_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_sol_sph_MHD_crank                                &
     &         (dt, sph_rj, r_2nd, MHD_prop, sph_MHD_bc, leg,           &
     &          ipol, sph_MHD_mat, rj_fld)
!
      use self_buoyancy_w_filter_sph
      use cal_sol_sph_fluid_crank
      use const_sph_radial_grad
      use cal_sol_sph_MHD_crank
!
      real(kind = kreal), intent(in) :: dt
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
!
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
      type(phys_data), intent(inout) :: rj_fld
!
!      integer(kind = kint) :: j, k, inod
!
!*-----  time evolution   -------------
!*
!      call check_ws_spectr(sph_rj, ipol, rj_fld)
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
!         Input:    ipol%base%i_vort to ipol%base%i_vort+2
!         Solution: ipol%base%i_velo to ipol%base%i_velo+2
        if (iflag_debug .gt. 0)                                         &
     &       write(*,*) 'cal_sol_velo_by_vort_sph_crank'
        call cal_sol_velo_by_vort_sph_crank                             &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_U,       &
     &      sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,         &
     &      sph_MHD_mat%band_vp_evo, sph_MHD_mat%band_vt_evo,           &
     &      ipol, rj_fld)
        call const_grad_vp_and_vorticity                                &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_U,       &
     &      sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,         &
     &      leg%g_sph_rj, ipol%base%i_velo, ipol%base%i_vort, rj_fld)
      end if
!
!  Input: ipol%base%i_temp,  Solution: ipol%base%i_temp
      if(iflag_debug.gt.0) write(*,*) 'cal_sol_scalar_sph_crank'
      if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        call cal_sol_scalar_sph_crank(dt, sph_rj,                       &
     &      MHD_prop%ht_prop, sph_MHD_bc%sph_bc_T, sph_MHD_bc%bcs_T,    &
     &      sph_MHD_mat%band_temp_evo, sph_MHD_mat%band_temp00_evo,     &
     &      ipol%base%i_temp, rj_fld, sph_MHD_mat%x00_w_center)
      end if
!
!  Input: ipol%base%i_light,  Solution: ipol%base%i_light
      if(iflag_debug.gt.0) write(*,*) 'cal_sol_scalar_sph_crank'
      if(MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        call cal_sol_scalar_sph_crank(dt, sph_rj,                       &
     &      MHD_prop%cp_prop, sph_MHD_bc%sph_bc_C, sph_MHD_bc%bcs_C,    &
     &      sph_MHD_mat%band_comp_evo, sph_MHD_mat%band_comp00_evo,     &
     &      ipol%base%i_light, rj_fld, sph_MHD_mat%x00_w_center)
      end if
!
!  Input:    ipol%base%i_magne, to ipol%base%i_magne+2
!  Solution: ipol%base%i_magne, to ipol%base%i_magne+2
      if(iflag_debug.gt.0) write(*,*) 'cal_sol_magne_sph_crank'
      if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call cal_sol_magne_sph_crank                                    &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_B, sph_MHD_bc%bcs_B,       &
     &      sph_MHD_mat%band_bp_evo, sph_MHD_mat%band_bt_evo,           &
     &      leg%g_sph_rj, ipol, rj_fld)
        call const_grad_bp_and_current                                  &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_B, sph_MHD_bc%bcs_B,       &
     &      leg%g_sph_rj, ipol%base%i_magne, ipol%base%i_current,       &
     &      rj_fld)
      end if
!
!*  ---- update after evolution ------------------
!      call check_vs_spectr(sph_rj, ipol, rj_fld)
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        call update_after_vorticity_sph                                 &
     &     (sph_rj, r_2nd, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,       &
     &      sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,         &
     &      leg, ipol, rj_fld)
        call cal_rot_self_buo_sph_SGS_MHD(sph_rj, ipol,                 &
     &      MHD_prop, sph_MHD_bc%sph_bc_U, rj_fld)
      end if
!
      if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        call update_after_heat_sph(sph_rj, r_2nd, MHD_prop%ht_prop,     &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%bcs_T,                      &
     &      sph_MHD_bc%fdm2_center, leg, ipol, rj_fld)
      end if
      if(MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        call update_after_composit_sph(sph_rj, r_2nd, MHD_prop%cp_prop, &
     &      sph_MHD_bc%sph_bc_C, sph_MHD_bc%bcs_C,                      &
     &      sph_MHD_bc%fdm2_center, leg, ipol, rj_fld)
      end if
      if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call update_after_magne_sph(sph_rj, r_2nd, MHD_prop%cd_prop,    &
     &      sph_MHD_bc%sph_bc_B, leg, ipol, rj_fld)
      end if
!
      end subroutine s_cal_sol_sph_MHD_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_field_to_start(sph_rj, r_2nd,                  &
     &          MHD_prop, sph_MHD_bc, leg, ipol, rj_fld)
!
      use const_sph_radial_grad
      use self_buoyancy_w_filter_sph
      use cal_sol_sph_MHD_crank
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol%base%i_vort .gt. 0) then
        call const_grad_vp_and_vorticity                                &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_U,       &
     &      sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,         &
     &      leg%g_sph_rj, ipol%base%i_velo, ipol%base%i_vort, rj_fld)
      end if
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug.gt.0) write(*,*) 'update_after_vorticity_sph'
        call update_after_vorticity_sph                                 &
     &     (sph_rj, r_2nd, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,       &
     &      sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,         &
     &      leg, ipol, rj_fld)
        if(iflag_debug.gt.0) write(*,*) 'cal_rot_self_buo_sph_SGS_MHD'
        call cal_rot_self_buo_sph_SGS_MHD(sph_rj, ipol,                 &
     &      MHD_prop, sph_MHD_bc%sph_bc_U, rj_fld)
      end if
!
      if(iflag_debug.gt.0) write(*,*) 'update_after_heat_sph'
      call update_after_heat_sph(sph_rj, r_2nd, MHD_prop%ht_prop,       &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%bcs_T,                        &
     &    sph_MHD_bc%fdm2_center, leg, ipol, rj_fld)
      if(iflag_debug.gt.0) write(*,*) 'update_after_composit_sph'
      call update_after_composit_sph(sph_rj, r_2nd, MHD_prop%cp_prop,   &
     &    sph_MHD_bc%sph_bc_C, sph_MHD_bc%bcs_C,                        &
     &    sph_MHD_bc%fdm2_center, leg, ipol, rj_fld)
!
      if(ipol%base%i_current .gt. 0) then
        call const_grad_bp_and_current                                  &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_B, sph_MHD_bc%bcs_B,       &
     &      leg%g_sph_rj, ipol%base%i_magne, ipol%base%i_current,       &
     &      rj_fld)
      end if
!
      call update_after_magne_sph(sph_rj, r_2nd, MHD_prop%cd_prop,      &
     &    sph_MHD_bc%sph_bc_B, leg, ipol, rj_fld)
!
      end subroutine set_sph_field_to_start
!
! -----------------------------------------------------------------------
!
      end module cal_sol_sph_SGS_MHD_crank
