!>@file   set_address_sph_trans_d_SGS.f90
!!@brief  module set_address_sph_trans_d_SGS
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_trans_sph_d_SGS(iphys, ipol, trns_SGS2,&
!!    &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!      subroutine check_address_trans_sph_d_SGS                        &
!!     &         (ipol, idpdr, itor, iphys, trns_SGS2)
!!@endverbatim
!
      module set_address_sph_trans_d_SGS
!
      use m_precision
!
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
      private :: b_trans_address_vect_diff_SGS
      private :: b_trans_address_scl_diff_SGS
!      private :: f_trans_address_vector_snap
!      private :: f_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_d_SGS(iphys, ipol, trns_SGS2,  &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(phys_address), intent(in) :: iphys, ipol
      type(address_4_sph_trans), intent(inout) :: trns_SGS2
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint) :: nscltsr_rtp_2_rj, nscltsr_rj_2_rtp
!
!
      call b_trans_address_vect_diff_SGS                                &
     &   (ipol, iphys, trns_SGS2%nvector_rj_2_rtp, trns_SGS2%b_trns)
      call b_trans_address_scl_diff_SGS                                 &
     &   (ipol, iphys, trns_SGS2%nvector_rj_2_rtp,                      &
     &    trns_SGS2%nscalar_rj_2_rtp, trns_SGS2%b_trns)
      trns_SGS2%ntensor_rj_2_rtp = 0
!
!      call f_trans_address_vector_snap                                 &
!     &   (ipol, trns_SGS2%nvector_rtp_2_rj, trns_SGS2%f_trns)
!      call f_trans_address_scalar_snap                                 &
!     &   (ipol, trns_SGS2%nvector_rtp_2_rj,                            &
!     &    trns_SGS2%nscalar_rtp_2_rj, trns_SGS2%f_trns)
       trns_SGS2%nvector_rtp_2_rj = 0
       trns_SGS2%nscalar_rtp_2_rj = 0
       trns_SGS2%ntensor_rtp_2_rj = 0
!
!
      nscltsr_rtp_2_rj                                                  &
     &      = trns_SGS2%nscalar_rj_2_rtp + 6*trns_SGS2%ntensor_rj_2_rtp
      trns_SGS2%ncomp_rj_2_rtp                                          &
     &      = 3*trns_SGS2%nvector_rj_2_rtp + nscltsr_rtp_2_rj
!
      nscltsr_rj_2_rtp                                                  &
     &      = trns_SGS2%nscalar_rtp_2_rj + 6*trns_SGS2%ntensor_rtp_2_rj
      trns_SGS2%ncomp_rtp_2_rj                                          &
     &      = 3*trns_SGS2%nvector_rtp_2_rj + nscltsr_rj_2_rtp
!
!
      ncomp_sph_trans = max(ncomp_sph_trans, trns_SGS2%ncomp_rtp_2_rj)
      ncomp_sph_trans = max(ncomp_sph_trans, trns_SGS2%ncomp_rj_2_rtp)
!
      nvector_sph_trans                                                 &
     &       = max(nvector_sph_trans, trns_SGS2%nvector_rj_2_rtp)
      nvector_sph_trans                                                 &
     &       = max(nvector_sph_trans, trns_SGS2%nvector_rtp_2_rj)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rtp_2_rj)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rj_2_rtp)
!
      end subroutine set_addresses_trans_sph_d_SGS
!
!-----------------------------------------------------------------------
!
      subroutine check_address_trans_sph_d_SGS                          &
     &         (ipol, idpdr, itor, iphys, trns_SGS2)
!
      use check_address_sph_trans
!
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(in) :: trns_SGS2
!
!
      write(*,*) 'addresses of spherical transform for diff. of SGS'
!
      call check_add_trans_sph_MHD                                      &
     &   (ipol, idpdr, itor, iphys, trns_SGS2%b_trns, trns_SGS2%f_trns, &
     &    trns_SGS2%ncomp_rj_2_rtp, trns_SGS2%nvector_rj_2_rtp,         &
     &    trns_SGS2%nscalar_rj_2_rtp, trns_SGS2%ncomp_rtp_2_rj,         &
     &    trns_SGS2%nvector_rtp_2_rj, trns_SGS2%nscalar_rtp_2_rj)
!
      end subroutine check_address_trans_sph_d_SGS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_vect_diff_SGS                          &
     &         (ipol, iphys, nvector_rj_2_rtp, b_trns)
!
      type(phys_address), intent(in) :: ipol, iphys
      integer(kind = kint), intent(inout) :: nvector_rj_2_rtp
      type(phys_address), intent(inout) :: b_trns
!
!
      nvector_rj_2_rtp = 0
!
      call add_vec_trans_flag                                           &
     &   (ipol%i_SGS_rot_inertia, iphys%i_SGS_rot_inertia,              &
     &    nvector_rj_2_rtp, b_trns%i_SGS_rot_inertia)
!
      call add_vec_trans_flag                                           &
     &   (ipol%i_SGS_rot_Lorentz, iphys%i_SGS_rot_Lorentz,              &
     &    nvector_rj_2_rtp, b_trns%i_SGS_rot_Lorentz)
!
      call add_vec_trans_flag                                           &
     &   (ipol%i_SGS_induction, iphys%i_SGS_induction,                  &
     &    nvector_rj_2_rtp, b_trns%i_SGS_induction)
!
      end subroutine b_trans_address_vect_diff_SGS
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scl_diff_SGS(ipol, iphys,              &
     &          nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns)
!
      type(phys_address), intent(in) :: ipol, iphys
      integer(kind = kint), intent(in) :: nvector_rj_2_rtp
      integer(kind = kint), intent(inout) :: nscalar_rj_2_rtp
      type(phys_address), intent(inout) :: b_trns
!
!
      nscalar_rj_2_rtp = 0
!
      call add_scalar_trans_flag(ipol%i_SGS_div_inertia,                &
     &    iphys%i_SGS_div_inertia, nvector_rj_2_rtp,                    &
     &    nscalar_rj_2_rtp, b_trns%i_SGS_div_inertia)
!
      call add_scalar_trans_flag(ipol%i_SGS_div_Lorentz,                &
     &    iphys%i_SGS_div_Lorentz, nvector_rj_2_rtp,                    &
     &    nscalar_rj_2_rtp, b_trns%i_SGS_div_Lorentz)
!
      call add_scalar_trans_flag(ipol%i_SGS_div_h_flux,                 &
     &    iphys%i_SGS_div_h_flux, nvector_rj_2_rtp,                     &
     &    nscalar_rj_2_rtp, b_trns%i_SGS_div_h_flux)
!
      call add_scalar_trans_flag(ipol%i_SGS_div_c_flux,                 &
     &    iphys%i_SGS_div_c_flux, nvector_rj_2_rtp,                     &
     &    nscalar_rj_2_rtp, b_trns%i_SGS_div_c_flux)
!
      end subroutine b_trans_address_scl_diff_SGS
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_d_SGS
