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
!!      subroutine set_addresses_trans_sph_SGS(ipol, trns_SGS,          &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!      subroutine set_addresses_trans_sph_Csim(ipol, trns_Csim,        &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_Csim
!!      subroutine check_address_trans_sph_SGS                          &
!!     &         (ipol, idpdr, itor, iphys, trns_SGS)
!!        type(phys_address), intent(in) :: ipol, idpdr, itor
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(in) :: trns_SGS
!!      subroutine check_address_trans_sph_Csim                         &
!!     &         (ipol, idpdr, itor, iphys, trns_Csim)
!!        type(phys_address), intent(in) :: ipol, idpdr, itor
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(in) :: trns_Csim
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
      private :: f_trans_address_scalar_SGS, f_trans_address_vector_SGS
      private :: b_trans_address_scalar_Csim
      private :: f_trans_address_scalar_Csim
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
!      call f_trans_address_scalar_SGS                                   &
!     &   (ipol, trns_SGS%nvector_rtp_2_rj, trns_SGS%nscalar_rtp_2_rj,   &
!     &    trns_SGS%f_trns)
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
      subroutine set_addresses_trans_sph_Csim(ipol, trns_Csim,          &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use m_control_parameter
!
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_Csim
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint) :: nscltsr_rtp_2_rj, nscltsr_rj_2_rtp
!
      trns_Csim%nvector_rj_2_rtp = 0
      call b_trans_address_scalar_Csim                                  &
     &   (ipol, trns_Csim%nvector_rj_2_rtp, trns_Csim%nscalar_rj_2_rtp, &
     &    trns_Csim%b_trns)
      trns_Csim%ntensor_rj_2_rtp = 0
!
      trns_Csim%nvector_rtp_2_rj = 0
      call f_trans_address_scalar_Csim                                  &
     &   (ipol, trns_Csim%nvector_rtp_2_rj, trns_Csim%nscalar_rtp_2_rj, &
     &    trns_Csim%f_trns)
      trns_Csim%ntensor_rtp_2_rj = 0
!
      nscltsr_rtp_2_rj                                                  &
     &    = trns_Csim%nscalar_rj_2_rtp + 6*trns_Csim%ntensor_rj_2_rtp
      trns_Csim%ncomp_rj_2_rtp                                          &
     &    = 3*trns_Csim%nvector_rj_2_rtp + nscltsr_rtp_2_rj
!
      nscltsr_rj_2_rtp                                                  &
     &    = trns_Csim%nscalar_rtp_2_rj + 6*trns_Csim%ntensor_rtp_2_rj
      trns_Csim%ncomp_rtp_2_rj                                          &
     &    = 3*trns_Csim%nvector_rtp_2_rj + nscltsr_rj_2_rtp
!
      ncomp_sph_trans                                                   &
     &    = max(trns_Csim%ncomp_rj_2_rtp, trns_Csim%ncomp_rtp_2_rj)
      nvector_sph_trans                                                 &
     &    = max(trns_Csim%nvector_rj_2_rtp, trns_Csim%nvector_rtp_2_rj)
      nscalar_sph_trans = max(nscltsr_rtp_2_rj, nscltsr_rj_2_rtp)
!
      end subroutine set_addresses_trans_sph_Csim
!
!-----------------------------------------------------------------------
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
!
      subroutine check_address_trans_sph_Csim                           &
     &         (ipol, idpdr, itor, iphys, trns_Csim)
!
      use check_address_sph_trans
!
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(in) :: trns_Csim
!
!
      write(*,*)                                                        &
     &      'addresses of spherical transform for model coefficients'
!
      call check_add_trans_sph_MHD                                      &
     &   (ipol, idpdr, itor, iphys, trns_Csim%b_trns, trns_Csim%f_trns, &
     &    trns_Csim%ncomp_rj_2_rtp, trns_Csim%nvector_rj_2_rtp,         &
     &    trns_Csim%nscalar_rj_2_rtp, trns_Csim%ncomp_rtp_2_rj,         &
     &    trns_Csim%nvector_rtp_2_rj, trns_Csim%nscalar_rtp_2_rj)
!
      end subroutine check_address_trans_sph_Csim
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
!   filtered Inertia
      call add_vector_trans_flag(ipol%i_SGS_inertia,                    &
     &    nvector_rj_2_rtp, b_trns%i_SGS_inertia)
!   filtered Lorentz force
      call add_vector_trans_flag(ipol%i_SGS_Lorentz,                    &
     &    nvector_rj_2_rtp, b_trns%i_SGS_Lorentz)
!   filtered induction
      call add_vector_trans_flag(ipol%i_SGS_vp_induct,                  &
     &    nvector_rj_2_rtp, b_trns%i_SGS_vp_induct)
!   filtered heat flux
      call add_vector_trans_flag(ipol%i_SGS_h_flux,                     &
     &    nvector_rj_2_rtp, b_trns%i_SGS_h_flux)
!   filtered composition flux
      call add_vector_trans_flag(ipol%i_SGS_c_flux,                     &
     &    nvector_rj_2_rtp, b_trns%i_SGS_c_flux)
!
!
!   dual filtered Inertia
      call add_vector_trans_flag(ipol%i_wide_SGS_inertia,               &
     &    nvector_rj_2_rtp, b_trns%i_wide_SGS_inertia)
!   dual filtered Lorentz force
      call add_vector_trans_flag(ipol%i_wide_SGS_Lorentz,               &
     &    nvector_rj_2_rtp, b_trns%i_wide_SGS_Lorentz)
!   dual filtered induction
      call add_vector_trans_flag(ipol%i_wide_SGS_vp_induct,             &
     &    nvector_rj_2_rtp, b_trns%i_wide_SGS_vp_induct)
!   dual filtered heat flux
      call add_vector_trans_flag(ipol%i_wide_SGS_h_flux,                &
     &    nvector_rj_2_rtp, b_trns%i_wide_SGS_h_flux)
!   dual filtered composition flux
      call add_vector_trans_flag(ipol%i_wide_SGS_c_flux,                &
     &    nvector_rj_2_rtp, b_trns%i_wide_SGS_c_flux)
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
!   SGS advection flag
      call add_vector_trans_flag(ipol%i_SGS_inertia,                    &
     &    nvector_rtp_2_rj, f_trns%i_SGS_inertia)
!   SGS Lorentz force flag
      call add_vector_trans_flag(ipol%i_SGS_Lorentz,                    &
     &    nvector_rtp_2_rj, f_trns%i_SGS_Lorentz)
!   SGS induction flag
      call add_vector_trans_flag(ipol%i_SGS_vp_induct,                  &
     &    nvector_rtp_2_rj, f_trns%i_SGS_vp_induct)
!   SGS heat flux flag
      call add_vector_trans_flag(ipol%i_SGS_h_flux,                     &
     &    nvector_rtp_2_rj, f_trns%i_SGS_h_flux)
!   SGS composition flux flag
      call add_vector_trans_flag(ipol%i_SGS_c_flux,                     &
     &    nvector_rtp_2_rj, f_trns%i_SGS_c_flux)
!
!
!
      end subroutine f_trans_address_vector_SGS
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_SGS                             &
     &         (ipol, nvector_rtp_2_rj, nscalar_rtp_2_rj, f_trns)
!
      use m_control_parameter
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nvector_rtp_2_rj
      integer(kind = kint), intent(inout) :: nscalar_rtp_2_rj
      type(phys_address), intent(inout) :: f_trns
!
!
      nscalar_rtp_2_rj = 0
!   work of Reynolds stress
      call add_scalar_trans_flag                                        &
     &   (ipol%i_reynolds_wk, nvector_rtp_2_rj, nscalar_rtp_2_rj,       &
     &    f_trns%i_reynolds_wk)
!   work of SGS buoyancy
      call add_scalar_trans_flag                                        &
     &   (ipol%i_SGS_buo_wk, nvector_rtp_2_rj, nscalar_rtp_2_rj,        &
     &    f_trns%i_SGS_buo_wk)
!   work of SGS compositional buoyancy
      call add_scalar_trans_flag                                        &
     &   (ipol%i_SGS_comp_buo_wk, nvector_rtp_2_rj, nscalar_rtp_2_rj,   &
     &    f_trns%i_SGS_comp_buo_wk)
!
      end subroutine f_trans_address_scalar_SGS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_Csim                            &
     &         (ipol, nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns)
!
      use m_control_parameter
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nvector_rj_2_rtp
      integer(kind = kint), intent(inout) :: nscalar_rj_2_rtp
      type(phys_address), intent(inout) :: b_trns
!
!
      nscalar_rj_2_rtp = 0
!   SGS advection flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_m_flux,                &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns%i_Csim_SGS_m_flux)
!   SGS Lorentz force flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_Lorentz,               &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp,                           &
     &    b_trns%i_Csim_SGS_Lorentz)
!   SGS induction flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_induction,             &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp,                           &
     &    b_trns%i_Csim_SGS_induction)
!   SGS heat flux flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_h_flux,                &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns%i_Csim_SGS_h_flux)
!   SGS composition flux flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_c_flux,                &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns%i_Csim_SGS_c_flux)
!
!   SGS buoyancy
      call add_scalar_trans_flag(ipol%i_Csim_SGS_buoyancy,              &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp,                           &
     &    b_trns%i_Csim_SGS_buoyancy)
!   SGS compostional buoyancy
      call add_scalar_trans_flag(ipol%i_Csim_SGS_comp_buo,              &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp,                           &
     &    b_trns%i_Csim_SGS_comp_buo)
!
      end subroutine b_trans_address_scalar_Csim
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_Csim                            &
     &         (ipol, nvector_rtp_2_rj, nscalar_rtp_2_rj, f_trns)
!
      use m_control_parameter
!
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(inout) :: f_trns
      integer(kind = kint), intent(in) :: nvector_rtp_2_rj
      integer(kind = kint), intent(inout) :: nscalar_rtp_2_rj
!
!
      nscalar_rtp_2_rj = 0
!   SGS advection flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_m_flux,                &
     &    nvector_rtp_2_rj, nscalar_rtp_2_rj, f_trns%i_Csim_SGS_m_flux)
!   SGS Lorentz force flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_Lorentz,               &
     &    nvector_rtp_2_rj, nscalar_rtp_2_rj,                           &
     &    f_trns%i_Csim_SGS_Lorentz)
!   SGS induction flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_induction,             &
     &    nvector_rtp_2_rj, nscalar_rtp_2_rj,                           &
     &    f_trns%i_Csim_SGS_induction)
!   SGS heat flux flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_h_flux,                &
     &    nvector_rtp_2_rj, nscalar_rtp_2_rj, f_trns%i_Csim_SGS_h_flux)
!   SGS composition flux flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_c_flux,                &
     &    nvector_rtp_2_rj, nscalar_rtp_2_rj, f_trns%i_Csim_SGS_c_flux)
!
!   SGS buoyancy
      call add_scalar_trans_flag(ipol%i_Csim_SGS_buoyancy,              &
     &    nvector_rtp_2_rj, nscalar_rtp_2_rj,                           &
     &    f_trns%i_Csim_SGS_buoyancy)
!   SGS compostional buoyancy
      call add_scalar_trans_flag(ipol%i_Csim_SGS_comp_buo,              &
     &    nvector_rtp_2_rj, nscalar_rtp_2_rj,                           &
     &    f_trns%i_Csim_SGS_comp_buo)
!
      end subroutine f_trans_address_scalar_Csim
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_SGS
