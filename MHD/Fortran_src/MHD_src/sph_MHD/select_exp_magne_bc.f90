!> @file  select_exp_magne_bc.f90
!!      module select_exp_magne_bc
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Select boundary condition routines for magnetic field
!!
!!@verbatim
!!      subroutine sel_bc_grad_bp_and_current(sph_bc_B,                 &
!!     &           is_magne, is_current, ntot_phys_rj, d_rj)
!!     &           is_magne, is_current)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: idpdr%i_magne,
!!                  ipol%i_current, itor%i_current, idpdr%i_current
!!      subroutine sel_bc_grad_poloidal_magne                           &
!!     &         (sph_bc_B, is_magne, ntot_phys_rj, d_rj)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: idpdr%i_magne
!!
!!      subroutine sel_bc_sph_current                                   &
!!     &         (sph_bc_B, is_magne, is_current, ntot_phys_rj, d_rj)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: ipol%i_current, itor%i_current, idpdr%i_current
!!      subroutine sel_bc_sph_rotation_uxb                              &
!!     &         (sph_bc_B, is_fld, is_rot, ntot_phys_rj, d_rj)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!      subroutine sel_bc_sph_magnetic_diffusion(sph_bc_B, coef_diffuse,&
!!     &          is_magne, is_ohmic, ids_ohmic, ntot_phys_rj, d_rj)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: ipol%i_b_diffuse, itor%i_b_diffuse, idpdr%i_b_diffuse
!!@endverbatim
!!
!!@param sph_bc_B  Structure for basic boundary condition parameters
!!                 for magnetic field
!!
!!@param coef_diffuse   Diffusion coefficient
!!
!!@param is_magne    Spherical hermonics data address
!!                   for poloidal magnetic field
!!@param is_current    Spherical hermonics data address
!!                     for current density
!!@param is_ohmic    Spherical hermonics data address
!!                   for poloidal ohmic dissipation
!!@param ids_ohmic    Spherical hermonics data address
!!                   for radial derivative of poloidal ohmic dissipation
!!@param is_fld      Spherical hermonics data address for input vector
!!@param is_rot      Spherical hermonics data address for curl of field
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module select_exp_magne_bc
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
!
      use t_boundary_params_sph_MHD
!
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_icb_qvac
      use cal_sph_exp_nod_cmb_qvac
      use set_sph_exp_nod_center
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_bc_grad_bp_and_current(sph_bc_B,                   &
     &           is_magne, is_current, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_current
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call sph_center_fld_and_curl                                    &
     &     (nnod_rj, nidx_rj(2), ar_1d_rj(ione,2),                      &
     &      is_magne, is_current, ntot_phys_rj, d_rj)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_b_and_j                                &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,        &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      is_magne, is_current, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_icb_ins_b_and_j                                &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,        &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      is_magne, is_current, ntot_phys_rj, d_rj)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_b_and_j                                &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,       &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_cmb_ins_b_and_j                                &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,       &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_bc_grad_bp_and_current
!
! -----------------------------------------------------------------------
!
      subroutine sel_bc_grad_poloidal_magne                             &
     &         (sph_bc_B, is_magne, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_dsdr_sph_center_2(nnod_rj, nidx_rj(2), is_magne,       &
     &      ntot_phys_rj, d_rj)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_mag2(nnod_rj, nidx_rj(2),              &
     &      sph_bc_B%kr_in, is_magne, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_icb_ins_mag2                                   &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_in,                        &
     &      sph_bc_B%r_ICB, is_magne, ntot_phys_rj, d_rj)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_mag2                                   &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_out,                       &
     &      is_magne, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_cmb_ins_mag2                                   &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_out,                       &
     &      sph_bc_B%r_CMB, is_magne, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_bc_grad_poloidal_magne
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_bc_sph_current                                     &
     &         (sph_bc_B, is_magne, is_current, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_current
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_rot2                                    &
     &     (nnod_rj, nidx_rj(2), ar_1d_rj(ione,2),                      &
     &      is_magne, is_current, ntot_phys_rj, d_rj)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_rot2                                   &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,        &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      is_magne, is_current, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_icb_ins_rot2                                   &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,        &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      is_magne, is_current, ntot_phys_rj, d_rj)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_rot2                                   &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,       &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_cmb_ins_rot2                                   &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,       &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_bc_sph_current
!
! -----------------------------------------------------------------------
!
      subroutine sel_bc_sph_rotation_uxb                                &
     &         (sph_bc_B, is_fld, is_rot, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_fld, is_rot
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_rot2                                    &
     &     (nnod_rj, nidx_rj(2), ar_1d_rj(ione,2), is_fld, is_rot,      &
     &      ntot_phys_rj, d_rj)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_vp_rot2                                &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_in,                        &
     &      is_fld, is_rot, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_icb_ins_vp_rot2                                &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_in,                        &
     &      sph_bc_B%r_ICB, is_fld, is_rot, ntot_phys_rj, d_rj)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_vp_rot2                                &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_out,                       &
     &      is_fld, is_rot, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_cmb_ins_vp_rot2                                &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_out,                       &
     &      sph_bc_B%r_CMB, is_fld, is_rot, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_bc_sph_rotation_uxb
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_bc_sph_magnetic_diffusion(sph_bc_B, coef_diffuse,  &
     &          is_magne, is_ohmic, ids_ohmic, ntot_phys_rj, d_rj)
!
      use cal_sph_exp_fixed_scalar
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne
      integer(kind = kint), intent(in) :: is_ohmic, ids_ohmic
      real(kind = kreal), intent(in) :: coef_diffuse
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_diffuse2                                &
     &     (nnod_rj, nidx_rj(2), ar_1d_rj(ione,2), coef_diffuse,        &
     &      is_magne, is_ohmic, ntot_phys_rj, d_rj)
        call cal_dsdr_sph_center_2(nnod_rj, nidx_rj(2), is_ohmic,       &
     &      ntot_phys_rj, d_rj)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_diffuse2                               &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,        &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      coef_diffuse, is_magne, is_ohmic, ntot_phys_rj, d_rj)
        call cal_dsdr_sph_no_bc_in_2(nnod_rj, nidx_rj(2),               &
     &      sph_bc_B%kr_in, sph_bc_B%fdm2_fix_fld_ICB,                  &
     &      is_ohmic, ids_ohmic, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_icb_ins_diffuse2                               &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,        &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      coef_diffuse, is_magne, is_ohmic, ntot_phys_rj, d_rj)
        call cal_dsdr_sph_no_bc_in_2(nnod_rj, nidx_rj(2),               &
     &      sph_bc_B%kr_in, sph_bc_B%fdm2_fix_fld_ICB,                  &
     &      is_ohmic, ids_ohmic, ntot_phys_rj, d_rj)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_diffuse2                               &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,       &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      coef_diffuse, is_magne, is_ohmic, ntot_phys_rj, d_rj)
      else
        call cal_sph_nod_cmb_ins_diffuse2                               &
     &     (nnod_rj, nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,       &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      coef_diffuse, is_magne, is_ohmic, ntot_phys_rj, d_rj)
      end if
      call cal_dsdr_sph_no_bc_out_2(nnod_rj, nidx_rj(2),                &
     &    sph_bc_B%kr_out, sph_bc_B%fdm2_fix_fld_CMB,                   &
     &    is_ohmic, ids_ohmic, ntot_phys_rj, d_rj)
!
      end subroutine sel_bc_sph_magnetic_diffusion
!
! -----------------------------------------------------------------------
!
      end module select_exp_magne_bc
