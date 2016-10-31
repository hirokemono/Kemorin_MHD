!>@file   set_address_sph_trans_SGS.f90
!!@brief  module set_address_sph_trans_SGS
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_trans_sph_SGS(ipol, trns_MHD,          &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!      subroutine check_address_trans_sph_SGS                          &
!!     &         (ipol, idpdr, itor, iphys, trns_MHD)
!!        type(phys_address), intent(in) :: ipol, idpdr, itor
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!@endverbatim
!
      module set_address_sph_trans_SGS
!
      use m_precision
!
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
      private :: b_trans_address_vector_SGS
      private :: f_trans_address_vector_SGS
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_SGS(ipol, trns_SGS,            &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use m_control_parameter
!
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint) :: nscltsr_rtp_2_rj, nscltsr_rj_2_rtp
!
      call b_trans_address_vector_SGS                                   &
     &   (ipol, trns_SGS%nvector_rj_2_rtp, trns_SGS%b_trns)
      trns_SGS%nscalar_rj_2_rtp = 0
      trns_SGS%ntensor_rj_2_rtp = 0
!
      call f_trans_address_vector_SGS                                   &
     &   (ipol, trns_SGS%nvector_rtp_2_rj, trns_SGS%f_trns)
      trns_SGS%nscalar_rtp_2_rj = 0
      trns_SGS%ntensor_rtp_2_rj = 0
!
      nscltsr_rtp_2_rj                                                  &
     &      = trns_SGS%nscalar_rj_2_rtp + 6*trns_SGS%ntensor_rj_2_rtp
      trns_SGS%ncomp_rj_2_rtp                                           &
     &      = 3*trns_SGS%nvector_rj_2_rtp + nscltsr_rtp_2_rj
!
      nscltsr_rj_2_rtp                                                  &
     &      = trns_SGS%nscalar_rtp_2_rj + 6*trns_SGS%ntensor_rtp_2_rj
      trns_SGS%ncomp_rtp_2_rj                                           &
     &      = 3*trns_SGS%nvector_rtp_2_rj + nscltsr_rj_2_rtp
!
      ncomp_sph_trans                                                   &
     &      = max(trns_SGS%ncomp_rj_2_rtp, trns_SGS%ncomp_rtp_2_rj)
      nvector_sph_trans                                                 &
     &      = max(trns_SGS%nvector_rj_2_rtp, trns_SGS%nvector_rtp_2_rj)
      nscalar_sph_trans = max(nscltsr_rtp_2_rj, nscltsr_rj_2_rtp)
!
      end subroutine set_addresses_trans_sph_SGS
!
!-----------------------------------------------------------------------
!
      subroutine check_address_trans_sph_SGS                            &
     &         (ipol, idpdr, itor, iphys, trns_SGS)
!
      use check_address_sph_trans
!
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(in) :: trns_SGS
!
!
      write(*,*) 'addresses of spherical transform for SGS term'
!
      call check_add_trans_sph_MHD                                      &
     &   (ipol, idpdr, itor, iphys, trns_SGS%b_trns, trns_SGS%f_trns,   &
     &    trns_SGS%ncomp_rj_2_rtp, trns_SGS%nvector_rj_2_rtp,           &
     &    trns_SGS%nscalar_rj_2_rtp, trns_SGS%ncomp_rtp_2_rj,           &
     &    trns_SGS%nvector_rtp_2_rj, trns_SGS%nscalar_rtp_2_rj)
!
      end subroutine check_address_trans_sph_SGS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_vector_SGS                             &
     &         (ipol, nvector_rj_2_rtp, b_trns)
!
      use m_control_parameter
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(inout) :: nvector_rj_2_rtp
      type(phys_address), intent(inout) :: b_trns
!
!
      nvector_rj_2_rtp = 0
!
!   dual filtered current density
      if(ipol%i_SGS_inertia .gt. izero) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_SGS_inertia = 3*nvector_rj_2_rtp - 2
      end if
!   dual filtered current density
      if(ipol%i_SGS_Lorentz .gt. izero) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_SGS_Lorentz = 3*nvector_rj_2_rtp - 2
      end if
!   dual filtered current density
      if(ipol%i_SGS_vp_induct .gt. izero) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_SGS_vp_induct = 3*nvector_rj_2_rtp - 2
      end if
!   dual filtered current density
      if(ipol%i_SGS_h_flux .gt. izero) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_SGS_h_flux = 3*nvector_rj_2_rtp - 2
      end if
!   dual filtered current density
      if(ipol%i_SGS_c_flux .gt. izero) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_SGS_c_flux = 3*nvector_rj_2_rtp - 2
      end if
!
!
!   dual filtered current density
      if(ipol%i_wide_SGS_inertia .gt. izero) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_wide_SGS_inertia = 3*nvector_rj_2_rtp - 2
      end if
!   dual filtered current density
      if(ipol%i_wide_SGS_Lorentz .gt. izero) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_wide_SGS_Lorentz = 3*nvector_rj_2_rtp - 2
      end if
!   dual filtered current density
      if(ipol%i_wide_SGS_vp_induct .gt. izero) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_wide_SGS_vp_induct = 3*nvector_rj_2_rtp - 2
      end if
!   dual filtered current density
      if(ipol%i_wide_SGS_h_flux .gt. izero) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_wide_SGS_h_flux = 3*nvector_rj_2_rtp - 2
      end if
!   dual filtered current density
      if(ipol%i_wide_SGS_c_flux .gt. izero) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_wide_SGS_c_flux = 3*nvector_rj_2_rtp - 2
      end if
!
      end subroutine b_trans_address_vector_SGS
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_vector_SGS                             &
     &         (ipol, nvector_rtp_2_rj, f_trns)
!
      use m_control_parameter
!
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(inout) :: f_trns
      integer(kind = kint), intent(inout) :: nvector_rtp_2_rj
!
!
      nvector_rtp_2_rj = 0
!
!   SGS advection flag
      if(ipol%i_SGS_inertia .gt. izero) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 1
        f_trns%i_SGS_inertia = 3*nvector_rtp_2_rj - 2
      end if
!
!   SGS Lorentz force flag
      if(ipol%i_SGS_Lorentz .gt. izero) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 1
        f_trns%i_SGS_Lorentz = 3*nvector_rtp_2_rj - 2
      end if
!
!   SGS induction flag
      if(ipol%i_SGS_vp_induct .gt. izero) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 1
        f_trns%i_SGS_vp_induct = 3*nvector_rtp_2_rj - 2
      end if
!
!   SGS heat flux flag
      if(ipol%i_SGS_h_flux .gt. izero) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 1
        f_trns%i_SGS_h_flux = 3*nvector_rtp_2_rj - 2
      end if
!
!   SGS composition flux flag
      if(ipol%i_SGS_c_flux .gt. izero) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 1
        f_trns%i_SGS_c_flux = 3*nvector_rtp_2_rj - 2
      end if
!
      end subroutine f_trans_address_vector_SGS
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_SGS
