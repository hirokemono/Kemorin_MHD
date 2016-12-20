!>@file   set_bc_sph_mhd.f90
!!@brief  module set_bc_sph_mhd
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine s_set_bc_sph_mhd(sph_params, sph_rj, radial_rj_grp, &
!!     &         CTR_nod_grp_name, CTR_sf_grp_name)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(group_data), intent(in) :: radial_rj_grp
!!@endverbatim
!
      module set_bc_sph_mhd
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_boundary_condition_IDs
      use m_phys_labels
!
      use t_spheric_parameter
      use t_group_data
!
      implicit none
!
      private :: set_sph_bc_magne_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_bc_sph_mhd(sph_params, sph_rj, radial_rj_grp,   &
     &         CTR_nod_grp_name, CTR_sf_grp_name)
!
      use m_phys_labels
      use m_boundary_params_sph_MHD
      use set_bc_flag_sph_velo
      use set_bc_sph_scalars
!
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use m_coef_fdm_to_center
      use cal_fdm_coefs_4_boundaries
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(group_data), intent(in) :: radial_rj_grp
      character(len=kchara), intent(in) :: CTR_nod_grp_name
      character(len=kchara), intent(in) :: CTR_sf_grp_name
!
      integer(kind = kint) :: kst, ked
!
!
      if (evo_velo%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*) 'set_sph_bc_velo_sph'
        call set_sph_bc_velo_sph(sph_rj, radial_rj_grp,                 &
     &      sph_params%radius_ICB, sph_params%radius_CMB)
!
        call cal_fdm_coefs_4_BCs                                        &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, sph_bc_U)
!
        kst = sph_bc_U%kr_in
        ked = sph_bc_U%kr_in + 1
        call cal_fdm2_ICB_free_vp(sph_rj%radius_1d_rj_r(kst:ked))
        call cal_fdm2_ICB_free_vt(sph_rj%radius_1d_rj_r(kst:ked))
!
        kst = sph_bc_U%kr_out-1
        ked = sph_bc_U%kr_out
        call cal_fdm2_CMB_free_vp(sph_rj%radius_1d_rj_r(kst:ked))
        call cal_fdm2_CMB_free_vt(sph_rj%radius_1d_rj_r(kst:ked))
      end if
!
      if (evo_temp%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*) 'set_sph_bc_temp_sph'
        call set_sph_bc_temp_sph(sph_rj, radial_rj_grp)
        call cal_fdm_coefs_4_BCs                                       &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, sph_bc_T)
      end if
!
      if (evo_magne%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*) 'set_sph_bc_magne_sph'
        call set_sph_bc_magne_sph(sph_rj, radial_rj_grp,               &
     &      CTR_nod_grp_name, CTR_sf_grp_name)
        call cal_fdm_coefs_4_BCs                                       &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, sph_bc_B)
      end if
!
      if (evo_comp%iflag_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*) 'set_sph_bc_composition_sph'
        call set_sph_bc_composition_sph(sph_rj, radial_rj_grp)
        call cal_fdm_coefs_4_BCs                                       &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, sph_bc_C)
      end if
!
!      Set FDM matrices for Center
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_2nd_to_center_fixed_fdm'
      call cal_2nd_to_center_fixed_fdm(sph_rj%radius_1d_rj_r(1:2))
      call cal_2nd_center_fix_df_fdm(sph_rj%radius_1d_rj_r(1))
      call cal_2nd_center_fixed_fdm(sph_rj%radius_1d_rj_r(1:2))
!
!      Check data
!
      if(iflag_debug .gt. 1) then
        write(*,*) 'sph_bc_U%iflag_icb', sph_bc_U%kr_in,                &
     &          sph_bc_U%iflag_icb
        write(*,*) 'sph_bc_U%iflag_cmb', sph_bc_U%kr_out,               &
     &          sph_bc_U%iflag_cmb
        write(*,*) 'sph_bc_T%iflag_icb', sph_bc_T%kr_in,                &
     &          sph_bc_T%iflag_icb
        write(*,*) 'sph_bc_T%iflag_cmb', sph_bc_T%kr_out,               &
     &          sph_bc_T%iflag_cmb
        write(*,*) 'sph_bc_B%iflag_icb', sph_bc_B%kr_in,                &
     &          sph_bc_B%iflag_icb
        write(*,*) 'sph_bc_B%iflag_cmb', sph_bc_B%kr_out,               &
     &          sph_bc_B%iflag_cmb
        write(*,*) 'sph_bc_C%iflag_icb', sph_bc_C%kr_in,                &
     &          sph_bc_C%iflag_icb
        write(*,*) 'sph_bc_C%iflag_cmb', sph_bc_C%kr_out,               &
     &          sph_bc_C%iflag_cmb
!
        if (evo_temp%iflag_scheme .gt. id_no_evolution) then
          call check_sph_boundary_spectra(fhd_temp,                     &
     &        sph_rj%nidx_rj(2), sph_rj%idx_gl_1d_rj_j, sph_bc_T)
        end if
        if (evo_comp%iflag_scheme .gt. id_no_evolution) then
          call check_sph_boundary_spectra(fhd_light,                    &
     &        sph_rj%nidx_rj(2), sph_rj%idx_gl_1d_rj_j, sph_bc_C)
        end if
      end if
!
      if (iflag_debug .eq. iflag_full_msg) then
        if (evo_velo%iflag_scheme .gt. id_no_evolution) then
          call check_fdm_coefs_4_BC2(fhd_velo,  sph_bc_U)
          call check_coef_fdm_free_ICB
          call check_coef_fdm_free_CMB
        end if
!
        if (evo_magne%iflag_scheme .gt. id_no_evolution) then
          call check_fdm_coefs_4_BC2(fhd_magne, sph_bc_B)
        end if
        if (evo_temp%iflag_scheme .gt. id_no_evolution) then
          call check_fdm_coefs_4_BC2(fhd_temp,  sph_bc_T)
        end if
        if (evo_comp%iflag_scheme .gt. id_no_evolution) then
          call check_fdm_coefs_4_BC2(fhd_light, sph_bc_C)
        end if
!
        call check_coef_fdm_fix_dr_2ctr
      end if
!
      end subroutine s_set_bc_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_magne_sph(sph_rj, radial_rj_grp,            &
     &          CTR_nod_grp_name, CTR_sf_grp_name)
!
      use m_boundary_params_sph_MHD
      use m_bc_data_list
      use m_surf_data_list
      use set_bc_sph_scalars
!
      character(len=kchara), intent(in) :: CTR_nod_grp_name
      character(len=kchara), intent(in) :: CTR_sf_grp_name
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rj_grp
!
      integer(kind = kint) :: i
      integer(kind = kint) :: igrp_icb, igrp_cmb
!
!
      call find_both_sides_of_boundaries(sph_rj, radial_rj_grp,         &
     &    magne_nod, magne_surf, sph_bc_B, igrp_icb, igrp_cmb)
!
      sph_bc_B%iflag_icb = iflag_sph_insulator
      sph_bc_B%iflag_cmb = iflag_sph_insulator
!
!
      i = abs(igrp_icb)
      if(igrp_icb .lt. 0) then
        if(sph_bc_B%icb_grp_name .eq. CTR_sf_grp_name) then
          if(magne_surf%ibc_type(i) .eq. iflag_sph_2_center) then
            sph_bc_B%iflag_icb =  iflag_sph_fill_center
          else if(magne_surf%ibc_type(i) .eq. iflag_sph_clip_center)    &
     &        then
            sph_bc_B%iflag_icb =  iflag_sph_fix_center
          end if
!
        else if(magne_surf%ibc_type(i) .eq. iflag_pseudo_vacuum) then
          sph_bc_B%iflag_icb =  iflag_radial_magne
        end if
      else
        if(sph_bc_B%icb_grp_name .eq. CTR_nod_grp_name) then
          if(magne_nod%ibc_type(i) .eq. iflag_sph_2_center) then
            sph_bc_B%iflag_icb =  iflag_sph_fill_center
          else if(magne_nod%ibc_type(i) .eq. iflag_sph_clip_center)     &
     &        then
            sph_bc_B%iflag_icb =  iflag_sph_fix_center
          end if
!
        else if(magne_nod%ibc_type(i) .eq. iflag_pseudo_vacuum) then
            sph_bc_B%iflag_icb =  iflag_radial_magne
        end if
      end if
!
!
      i = abs(igrp_cmb)
      if(igrp_icb .lt. 0) then
        if(magne_surf%ibc_type(i) .eq. iflag_pseudo_vacuum) then
          sph_bc_B%iflag_cmb =  iflag_radial_magne
        end if
      else
        if(magne_nod%ibc_type(i) .eq. iflag_pseudo_vacuum) then
          sph_bc_B%iflag_cmb =  iflag_radial_magne
        end if
      end if
!
      end subroutine set_sph_bc_magne_sph
!
! -----------------------------------------------------------------------
!
      end module set_bc_sph_mhd
