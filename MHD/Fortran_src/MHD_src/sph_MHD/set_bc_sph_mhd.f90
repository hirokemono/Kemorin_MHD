!>@file   set_bc_sph_mhd.f90
!!@brief  module set_bc_sph_mhd
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine s_set_bc_sph_mhd(bc_IO, sph_params, sph_rj,          &
!!     &          radial_rj_grp, MHD_prop, MHD_BC, sph_MHD_bc)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(group_data), intent(in) :: radial_rj_grp
!!        real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1))
!!        type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!!      subroutine check_bc_sph_mhd(MHD_prop, sph_MHD_bc)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!@endverbatim
!
      module set_bc_sph_mhd
!
      use m_precision
!
      use m_machine_parameter
      use m_boundary_condition_IDs
!
      use t_control_parameter
      use t_physical_property
      use t_spheric_parameter
      use t_group_data
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_spheric_rj_data
      use t_bc_data_list
      use t_sph_boundary_input_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_bc_sph_mhd(bc_IO, sph_params, sph_rj,            &
     &          radial_rj_grp, MHD_prop, MHD_BC, h_rho, sph_MHD_bc)
!
      use m_base_field_labels
!
      use set_bc_flag_sph_velo
      use set_bc_sph_scalars
!
      use cal_fdm_coefs_4_boundaries
      use coef_fdm2_to_center
      use coef_fdm2_free_ICB
      use coef_fdm2_free_CMB
      use coef_fdm3e_hdiv_ICB
      use coef_fdm3e_hdiv_CMB
      use set_sph_bc_magne_sph
!
      type(boundary_spectra), intent(in) :: bc_IO
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(group_data), intent(in) :: radial_rj_grp
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MHD_BC_lists), intent(in) :: MHD_BC
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1))
!
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
      integer(kind = kint) :: kst
!
!
      if (MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*) 'set_sph_bc_velo_sph'
        call set_sph_bc_velo_sph(bc_IO, sph_rj, radial_rj_grp,          &
     &      sph_params%radius_ICB, sph_params%radius_CMB,               &
     &      MHD_BC%velo_BC%nod_BC, MHD_BC%velo_BC%surf_BC,              &
     &      sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_U)
!
!
        call cal_fdm_coefs_4_BCs                                        &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                   &
     &      sph_MHD_bc%sph_bc_U)
!
        kst = sph_MHD_bc%sph_bc_U%kr_in
        call cal_fdm2_ICB_free_vp(h_rho(kst),                           &
     &      sph_rj%radius_1d_rj_r(kst), sph_MHD_bc%fdm2_free_ICB)
        call cal_fdm2_ICB_free_vt(h_rho(kst),                           &
     &      sph_rj%radius_1d_rj_r(kst), sph_MHD_bc%fdm2_free_ICB)
!
        call cal_fdm3e_ICB_hdiv_vp(sph_rj%radius_1d_rj_r(kst),          &
     &                             sph_MHD_bc%fdm3e_ICB)
        call cal_fdm3e_ICB_free_hdiv_vp(sph_MHD_bc%fdm2_free_ICB,       &
     &                                  sph_MHD_bc%fdm3e_ICB,           &
     &                                  sph_MHD_bc%fdm3e_free_ICB)
!
        kst = sph_MHD_bc%sph_bc_U%kr_out
        call cal_fdm2_CMB_free_vp(h_rho(kst),                           &
     &      sph_rj%radius_1d_rj_r(kst-1), sph_MHD_bc%fdm2_free_CMB)
        call cal_fdm2_CMB_free_vt(h_rho(kst),                           &
     &      sph_rj%radius_1d_rj_r(kst-1), sph_MHD_bc%fdm2_free_CMB)
!
        call cal_fdm3e_CMB_hdiv_vp(sph_rj%radius_1d_rj_r(kst-2),        &
     &                             sph_MHD_bc%fdm3e_CMB)
        call cal_fdm3e_CMB_free_hdiv_vp(sph_MHD_bc%fdm2_free_CMB,       &
     &                                  sph_MHD_bc%fdm3e_CMB,           &
     &                                  sph_MHD_bc%fdm3e_free_CMB)
      end if
!
!
      if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*) 'set_sph_bc_temp_sph'
        call set_sph_bc_temp_sph(bc_IO, sph_rj, radial_rj_grp,          &
     &      MHD_BC%temp_BC%nod_BC, MHD_BC%temp_BC%surf_BC,              &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%bcs_T)
        call cal_fdm_coefs_4_BCs                                        &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                   &
     &      sph_MHD_bc%sph_bc_T)
      end if
!
      if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*) 's_set_sph_bc_magne_sph'
        call s_set_sph_bc_magne_sph(bc_IO, sph_rj, radial_rj_grp,       &
     &      CTR_nod_grp_name, CTR_sf_grp_name,                          &
     &      MHD_BC%magne_BC%nod_BC, MHD_BC%magne_BC%surf_BC,            &
     &      sph_MHD_bc%sph_bc_B, sph_MHD_bc%bcs_B)
        call cal_fdm_coefs_4_BCs                                        &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                   &
     &      sph_MHD_bc%sph_bc_B)
      end if
!
      if(MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*) 'set_sph_bc_composition_sph'
        call set_sph_bc_composition_sph(bc_IO, sph_rj, radial_rj_grp,   &
     &      MHD_BC%light_BC%nod_BC, MHD_BC%light_BC%surf_BC,            &
     &      sph_MHD_bc%sph_bc_C, sph_MHD_bc%bcs_C)
        call cal_fdm_coefs_4_BCs                                        &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                   &
     &      sph_MHD_bc%sph_bc_C)
      end if
!
!      Set FDM matrices for Center
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_2nd_to_center_fixed_fdm'
      call cal_2nd_to_center_fixed_fdm(sph_rj%radius_1d_rj_r(1),        &
     &                                 sph_MHD_bc%fdm2_center)
      call cal_2nd_center_fix_df_fdm(sph_rj%radius_1d_rj_r(1),          &
     &                               sph_MHD_bc%fdm2_center)
      call cal_2nd_center_fixed_fdm(sph_rj%radius_1d_rj_r(1),           &
     &                              sph_MHD_bc%fdm2_center)
!
!      Check data
      call check_bc_sph_mhd(MHD_prop, sph_MHD_bc)
!
      end subroutine s_set_bc_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine check_bc_sph_mhd(MHD_prop, sph_MHD_bc)
!
      use m_base_field_labels
!
      use set_bc_flag_sph_velo
      use set_bc_sph_scalars
!
      use cal_fdm_coefs_4_boundaries
      use coef_fdm2_to_center
      use coef_fdm2_free_ICB
      use coef_fdm2_free_CMB
      use coef_fdm3e_hdiv_ICB
      use coef_fdm3e_hdiv_CMB
      use set_sph_bc_magne_sph
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
!
      if(iflag_debug .gt. 1) then
        write(*,*) 'sph_bc_U%iflag_icb', sph_MHD_bc%sph_bc_U%kr_in,     &
     &          sph_MHD_bc%sph_bc_U%iflag_icb
        write(*,*) 'sph_bc_U%iflag_cmb', sph_MHD_bc%sph_bc_U%kr_out,    &
     &          sph_MHD_bc%sph_bc_U%iflag_cmb
        write(*,*) 'sph_bc_T%iflag_icb', sph_MHD_bc%sph_bc_T%kr_in,     &
     &          sph_MHD_bc%sph_bc_T%iflag_icb
        write(*,*) 'sph_bc_T%iflag_cmb', sph_MHD_bc%sph_bc_T%kr_out,    &
     &          sph_MHD_bc%sph_bc_T%iflag_cmb
        write(*,*) 'sph_bc_B%iflag_icb', sph_MHD_bc%sph_bc_B%kr_in,     &
     &          sph_MHD_bc%sph_bc_B%iflag_icb
        write(*,*) 'sph_bc_B%iflag_cmb', sph_MHD_bc%sph_bc_B%kr_out,    &
     &          sph_MHD_bc%sph_bc_B%iflag_cmb
        write(*,*) 'sph_bc_C%iflag_icb', sph_MHD_bc%sph_bc_C%kr_in,     &
     &          sph_MHD_bc%sph_bc_C%iflag_icb
        write(*,*) 'sph_bc_C%iflag_cmb', sph_MHD_bc%sph_bc_C%kr_out,    &
     &          sph_MHD_bc%sph_bc_C%iflag_cmb
      end if
!
      if (iflag_debug .eq. iflag_full_msg) then
        if (MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
          call check_fdm_coefs_4_BC2                                    &
     &       (velocity%name, sph_MHD_bc%sph_bc_U)
          call check_3rd_ele_BC_vpol_fdm(50, sph_MHD_bc%fdm3e_ICB)
          call check_coef_fdm_free_ICB(50, sph_MHD_bc%fdm2_free_ICB)
          call check_coef_fdm_free_CMB(50, sph_MHD_bc%fdm2_free_CMB)
          call check_3rd_ele_BC_vpol_fdm(50, sph_MHD_bc%fdm3e_free_ICB)
          call check_3rd_ele_BC_vpol_fdm(50, sph_MHD_bc%fdm3e_free_CMB)
        end if
!
        if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution)     &
     &   then
          call check_fdm_coefs_4_BC2                                    &
     &       (magnetic_field%name, sph_MHD_bc%sph_bc_B)
        end if
        if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
          call check_fdm_coefs_4_BC2                                    &
     &       (temperature%name,  sph_MHD_bc%sph_bc_T)
        end if
        if(MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
          call check_fdm_coefs_4_BC2                                    &
     &       (composition%name, sph_MHD_bc%sph_bc_C)
        end if
!
        call check_coef_fdm_fix_dr_2ctr(sph_MHD_bc%fdm2_center)
      end if
!
      end subroutine check_bc_sph_mhd
!
! -----------------------------------------------------------------------
!
      end module set_bc_sph_mhd
