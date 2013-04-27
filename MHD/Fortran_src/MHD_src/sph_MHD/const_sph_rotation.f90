!> @file  const_sph_rotation.f90
!!      module const_sph_rotation
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate curl of fields
!!
!!@verbatim
!!      subroutine const_sph_vorticity
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: ipol%i_vort, itor%i_vort, idpdr%i_vort
!!
!!      subroutine const_sph_current
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: ipol%i_current, itor%i_current, idpdr%i_current
!!
!!      subroutine const_sph_rotation_uxb(is_fld, is_rot)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!
!!      subroutine const_sph_rotation_no_bc(kr_inside, kr_outside,      &
!!     &          coef_fdm_fix_in_2, coef_fdm_fix_out_2, is_fld, is_rot)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!
!!      subroutine const_sph_force_rot2(is_fld, is_rot)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!
!!      subroutine const_sph_viscous_by_vort2
!!        Input:    ipol%i_vort, itor%i_vort
!!        Solution: ipol%i_v_diffuse, itor%i_v_diffuse, idpdr%i_v_diffuse
!!
!!      subroutine const_sph_mag_diffuse_by_j
!!        Input:    ipol%i_current, itor%i_current
!!        Solution: ipol%i_b_diffuse, itor%i_b_diffuse, idpdr%i_b_diffuse
!!@endverbatim
!!
!!@n @param kr_inside     Radial ID for inner boundary
!!@n @param kr_outside    RAdial ID for outer boundary
!!@n @param coef_fdm_fix_in_2(0:2,3)
!!             Finite difference matrix for inner boundary
!!@n @param coef_fdm_fix_out_2(0:2,3)
!!             Finite difference matrix for outer boundary
!!
!!@n @param is_fld      Input spectr field address
!!@n @param is_rot      Address of curl of field
!
      module const_sph_rotation
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use cal_sph_exp_rotation
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_vorticity
!
      use m_control_params_sph_MHD
      use m_sph_phys_address
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
!
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      if(iflag_icb_velocity .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_rot2(ipol%i_velo, ipol%i_vort)
      else if(iflag_icb_velocity .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rigid_rot2(ipol%i_velo, ipol%i_vort)
      else
        call cal_sph_nod_icb_rigid_rot2(ipol%i_velo, ipol%i_vort)
      end if
!
      if(iflag_cmb_velocity .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_rot2(ipol%i_velo, ipol%i_vort)
      else
        call cal_sph_nod_cmb_rigid_rot2(ipol%i_velo, ipol%i_vort)
      end if
!
      kr_st = nlayer_ICB+1
      kr_ed = nlayer_CMB-1
      call cal_sph_nod_vect_rot2(kr_st, kr_ed,                          &
     &    ipol%i_velo, ipol%i_vort)
!
      end subroutine const_sph_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_current
!
      use m_control_params_sph_MHD
      use m_sph_phys_address
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_icb_qvac
      use cal_sph_exp_nod_cmb_qvac
      use set_sph_exp_nod_center
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      if(iflag_icb_magne .eq. iflag_sph_fill_center) then
        kr_st = itwo
        call cal_sph_nod_center_rot2(ipol%i_magne, ipol%i_current)
      else if(iflag_icb_magne .eq. iflag_radial_magne) then
        kr_st = nlayer_ICB+1
        call cal_sph_nod_icb_qvc_rot2(ipol%i_magne, ipol%i_current)
      else
        kr_st = nlayer_ICB+1
        call cal_sph_nod_icb_ins_rot2(ipol%i_magne, ipol%i_current)
      end if
!
      kr_ed = nlayer_CMB-1
      call cal_sph_nod_vect_rot2(itwo, kr_ed,                           &
     &    ipol%i_magne, ipol%i_current)
!
      if(iflag_cmb_magne .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_rot2(ipol%i_magne, ipol%i_current)
      else
        call cal_sph_nod_cmb_ins_rot2(ipol%i_magne, ipol%i_current)
      end if
!
      end subroutine const_sph_current
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_rotation_uxb(is_fld, is_rot)
!
      use m_control_params_sph_MHD
      use m_sph_phys_address
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_icb_qvac
      use cal_sph_exp_nod_cmb_qvac
      use set_sph_exp_nod_center
!
      integer(kind = kint), intent(in) :: is_fld, is_rot
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      if(iflag_icb_magne .eq. iflag_sph_fill_center) then
        kr_st = itwo
        call cal_sph_nod_center_rot2(is_fld, is_rot)
      else if(iflag_icb_magne .eq. iflag_radial_magne) then
        kr_st = nlayer_ICB+1
        call cal_sph_nod_icb_qvc_vp_rot2(is_fld, is_rot)
      else
        kr_st = nlayer_ICB+1
        call cal_sph_nod_icb_ins_vp_rot2(is_fld, is_rot)
      end if
!
      kr_ed = nlayer_CMB- 1
      call cal_sph_nod_vect_w_div_rot2(kr_st, kr_ed, is_fld, is_rot)
!
      if(iflag_cmb_magne .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_vp_rot2(is_fld, is_rot)
      else
        call cal_sph_nod_cmb_ins_vp_rot2(is_fld, is_rot)
      end if
!
      end subroutine const_sph_rotation_uxb
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_rotation_no_bc(kr_inside, kr_outside,        &
     &          coef_fdm_fix_in_2, coef_fdm_fix_out_2, is_fld, is_rot)
!
      use cal_sph_exp_nod_none_bc
!
      integer(kind = kint), intent(in) :: kr_inside, kr_outside
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: coef_fdm_fix_in_2(0:2,3)
      real(kind = kreal), intent(in) :: coef_fdm_fix_out_2(0:2,3)
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      call cal_sph_nod_nobc_in_rot2(coef_fdm_fix_in_2, kr_inside,       &
     &    is_fld, is_rot)
      call cal_sph_nod_nobc_out_rot2(coef_fdm_fix_out_2, kr_outside,    &
     &    is_fld, is_rot)
!
      kr_st = kr_inside + 1
      kr_ed = kr_outside- 1
      call cal_sph_nod_vect_rot2(kr_st, kr_ed, is_fld, is_rot)
!
      end subroutine const_sph_rotation_no_bc
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_force_rot2(is_fld, is_rot)
!
      use m_control_params_sph_MHD
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
!
      integer(kind = kint), intent(in) :: is_fld, is_rot
      integer(kind = kint) :: kr_st, kr_ed
!
!
      if(iflag_icb_velocity .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_rot2(is_fld, is_rot)
      else
        call cal_sph_nod_icb_rigid_rot2(is_fld, is_rot)
      end if
!
      if(iflag_cmb_velocity .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_rot2(is_fld, is_rot)
      else
        call cal_sph_nod_cmb_rigid_rot2(is_fld, is_rot)
      end if
!
      kr_st = nlayer_ICB+1
      kr_ed = nlayer_CMB-1
      call cal_sph_nod_vect_w_div_rot2(kr_st, kr_ed, is_fld, is_rot)
!
      end subroutine const_sph_force_rot2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_viscous_by_vort2
!
      use m_control_params_sph_MHD
      use m_sph_phys_address
      use m_physical_property
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
      use cal_sph_exp_fixed_scalar
      use cal_inner_core_rotation
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      kr_st = nlayer_ICB+1
      kr_ed = nlayer_CMB-1
      call cal_sph_nod_diffuse_by_rot2(kr_st, kr_ed, coef_d_velo,       &
     &    ipol%i_vort, ipol%i_v_diffuse)
!
      if(iflag_icb_velocity .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_diffuse2(coef_d_velo,                 &
     &      ipol%i_velo, ipol%i_v_diffuse)
      else
        call cal_sph_nod_icb_rigid_diffuse2(coef_d_velo,                &
     &      ipol%i_velo, ipol%i_v_diffuse)
      end if
      call cal_dsdr_sph_icb_nobc_2(ipol%i_v_diffuse, idpdr%i_v_diffuse)
!
      if(iflag_icb_velocity .eq. iflag_rotatable_ic) then
        call cal_icore_viscous_drag_explicit(coef_d_velo,               &
     &      ipol%i_vort, itor%i_v_diffuse)
      end if
!
      if(iflag_cmb_velocity .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_diffuse2(coef_d_velo,                 &
     &      ipol%i_velo, ipol%i_v_diffuse)
      else
        call cal_sph_nod_cmb_rigid_diffuse2(coef_d_velo, ipol%i_velo,   &
     &      ipol%i_v_diffuse)
      end if
      call cal_dsdr_sph_cmb_nobc_2(ipol%i_v_diffuse, idpdr%i_v_diffuse)
!
      end subroutine const_sph_viscous_by_vort2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_mag_diffuse_by_j
!
      use m_control_params_sph_MHD
      use m_sph_phys_address
      use m_physical_property
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_icb_qvac
      use cal_sph_exp_nod_cmb_qvac
      use set_sph_exp_nod_center
!
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      if(iflag_icb_magne .eq. iflag_sph_fill_center) then
        kr_st = itwo
        call cal_sph_nod_center_diffuse2(coef_d_magne,                  &
     &      ipol%i_magne, ipol%i_b_diffuse)
        call cal_dsdr_sph_center_2(ipol%i_b_diffuse)
      else if(iflag_icb_magne .eq. iflag_radial_magne) then
        kr_st = nlayer_ICB+1
        call cal_sph_nod_icb_qvc_diffuse2(coef_d_magne,                 &
     &      ipol%i_magne, ipol%i_b_diffuse)
        call cal_dsdr_sph_icb_nobc_2(ipol%i_b_diffuse,                  &
     &      idpdr%i_b_diffuse)
      else
        kr_st = nlayer_ICB+1
        call cal_sph_nod_icb_ins_diffuse2(coef_d_magne,                 &
     &      ipol%i_magne, ipol%i_b_diffuse)
        call cal_dsdr_sph_icb_nobc_2(ipol%i_b_diffuse,                  &
     &      idpdr%i_b_diffuse)
      end if
!
      kr_ed = nlayer_CMB-1
      call cal_sph_nod_diffuse_by_rot2(kr_st, kr_ed, coef_d_magne,      &
     &    ipol%i_current,    &
     &    ipol%i_b_diffuse)
!
      if(iflag_cmb_magne .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_diffuse2(coef_d_magne,                 &
     &      ipol%i_magne, ipol%i_b_diffuse)
      else
        call cal_sph_nod_cmb_ins_diffuse2(coef_d_magne,                 &
     &      ipol%i_magne, ipol%i_b_diffuse)
      end if
      call cal_dsdr_sph_cmb_nobc_2(ipol%i_b_diffuse, idpdr%i_b_diffuse)
!
      end subroutine const_sph_mag_diffuse_by_j
!
! -----------------------------------------------------------------------
!
      end module const_sph_rotation
