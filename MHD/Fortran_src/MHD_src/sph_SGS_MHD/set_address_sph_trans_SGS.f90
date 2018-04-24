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
!!      subroutine set_addresses_trans_sph_DYNS                         &
!!     &         (SPH_MHD, iphys, trns_DYNS,                            &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_DYNS
!!      subroutine set_addresses_trans_sph_Csim(ipol, trns_Csim,        &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_Csim
!!      subroutine check_address_trans_sph_SGS                          &
!!     &         (ipol, idpdr, itor, iphys, trns_SGS)
!!        type(phys_address), intent(in) :: ipol, idpdr, itor
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(in) :: trns_DYNS
!!@endverbatim
!
      module set_address_sph_trans_SGS
!
      use m_precision
!
      use t_phys_address
      use t_SPH_mesh_field_data
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
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      call b_trans_address_vector_SGS                                   &
     &   (ipol, trns_SGS%nvector_rj_2_rtp, trns_SGS%b_trns)
      call b_trans_address_scalar_SGS                                   &
     &   (ipol, trns_SGS%nvector_rj_2_rtp, trns_SGS%nscalar_rj_2_rtp,   &
     &    trns_SGS%b_trns)
      trns_SGS%ntensor_rj_2_rtp = 0
!
      call f_trans_address_vector_SGS                                   &
     &   (ipol, trns_SGS%nvector_rtp_2_rj, trns_SGS%f_trns)
      call f_trans_address_scalar_SGS                                   &
     &   (ipol, trns_SGS%nvector_rtp_2_rj, trns_SGS%nscalar_rtp_2_rj,   &
     &    trns_SGS%f_trns)
      trns_SGS%ntensor_rtp_2_rj = 0
!
      call count_num_fields_4_sph_trans(trns_SGS, ncomp_sph_trans,      &
     &   nvector_sph_trans, nscalar_sph_trans)
!
      end subroutine set_addresses_trans_sph_SGS
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_DYNS                           &
     &         (SPH_MHD, iphys, trns_DYNS,                              &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_dyns
      use address_fwd_sph_trans_dyns
!
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_DYNS
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint):: icou
!
!
      call b_trans_address_vector_DYNS(SPH_MHD%ipol, trns_DYNS)
      call b_trans_address_scalar_DYNS(SPH_MHD%ipol, trns_DYNS)
      trns_DYNS%ntensor_rj_2_rtp = 0
!
      call f_trans_address_vector_DYNS(SPH_MHD%ipol, trns_DYNS)
      call f_trans_address_scalar_DYNS(SPH_MHD%ipol, trns_DYNS)
      trns_DYNS%ntensor_rtp_2_rj = 0
!
      call count_num_fields_4_sph_trans(trns_DYNS, ncomp_sph_trans,     &
     &   nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table for dynamnic SGS'
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_DYNS%nvector_rj_2_rtp
        write(*,*) 'nscalar_rj_2_rtp ', trns_DYNS%nscalar_rj_2_rtp
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      icou = 0
      call set_b_trans_vector_field_DYNS                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_DYNS)
      call set_b_trans_scalar_field_dyns                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_DYNS)
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'nvector_rtp_2_rj ', trns_DYNS%nvector_rtp_2_rj
        write(*,*) 'nscalar_rtp_2_rj ', trns_DYNS%nscalar_rtp_2_rj
        write(*,*) 'Address for forward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      icou = 0
      call set_f_trans_vector_field_DYNS                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_DYNS)
      call set_f_trans_scalar_field_DYNS                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_DYNS)
!
      end subroutine set_addresses_trans_sph_DYNS
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_Csim(ipol, trns_Csim,          &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_Csim
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
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
      call count_num_fields_4_sph_trans(trns_Csim, ncomp_sph_trans,     &
     &   nvector_sph_trans, nscalar_sph_trans)
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
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(inout) :: nvector_rj_2_rtp
      type(phys_address), intent(inout) :: b_trns
!
!
      nvector_rj_2_rtp = 0
!
!   wide filtered velocity
      call add_vector_trans_flag(ipol%i_wide_fil_velo,                  &
     &    nvector_rj_2_rtp, b_trns%i_wide_fil_velo)
!   wide filtered vorticity
      call add_vector_trans_flag(ipol%i_wide_fil_vort,                  &
     &    nvector_rj_2_rtp, b_trns%i_wide_fil_vort)
!   wide filtered magnetic field
      call add_vector_trans_flag(ipol%i_wide_fil_magne,                 &
     &    nvector_rj_2_rtp, b_trns%i_wide_fil_magne)
!   wide filtered current density
      call add_vector_trans_flag(ipol%i_wide_fil_current,               &
     &    nvector_rj_2_rtp, b_trns%i_wide_fil_current)
!
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
!   wide filtered Inertia
      call add_vector_trans_flag(ipol%i_wide_SGS_inertia,               &
     &    nvector_rj_2_rtp, b_trns%i_wide_SGS_inertia)
!   wide filtered Lorentz force
      call add_vector_trans_flag(ipol%i_wide_SGS_Lorentz,               &
     &    nvector_rj_2_rtp, b_trns%i_wide_SGS_Lorentz)
!   wide filtered induction
      call add_vector_trans_flag(ipol%i_wide_SGS_vp_induct,             &
     &    nvector_rj_2_rtp, b_trns%i_wide_SGS_vp_induct)
!   wide filtered heat flux
      call add_vector_trans_flag(ipol%i_wide_SGS_h_flux,                &
     &    nvector_rj_2_rtp, b_trns%i_wide_SGS_h_flux)
!   wide filtered composition flux
      call add_vector_trans_flag(ipol%i_wide_SGS_c_flux,                &
     &    nvector_rj_2_rtp, b_trns%i_wide_SGS_c_flux)
!
!   dual filtered Inertia
      call add_vector_trans_flag(ipol%i_dbl_SGS_inertia,                &
     &    nvector_rj_2_rtp, b_trns%i_dbl_SGS_inertia)
!   dual filtered Lorentz force
      call add_vector_trans_flag(ipol%i_dbl_SGS_Lorentz,                &
     &    nvector_rj_2_rtp, b_trns%i_dbl_SGS_Lorentz)
!   dual filtered induction
      call add_vector_trans_flag(ipol%i_dbl_SGS_vp_induct,              &
     &    nvector_rj_2_rtp, b_trns%i_dbl_SGS_vp_induct)
!   dual filtered heat flux
      call add_vector_trans_flag(ipol%i_dbl_SGS_h_flux,                 &
     &    nvector_rj_2_rtp, b_trns%i_dbl_SGS_h_flux)
!   dual filtered composition flux
      call add_vector_trans_flag(ipol%i_dbl_SGS_c_flux,                 &
     &    nvector_rj_2_rtp, b_trns%i_dbl_SGS_c_flux)
!
      end subroutine b_trans_address_vector_SGS
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_SGS                             &
     &         (ipol, nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns)
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nvector_rj_2_rtp
      integer(kind = kint), intent(inout) :: nscalar_rj_2_rtp
      type(phys_address), intent(inout) :: b_trns
!
!
      nscalar_rj_2_rtp = 0
!
!   wide filtered temperature
      call add_scalar_trans_flag(ipol%i_wide_fil_temp,                  &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns%i_wide_fil_temp)
!   wide filtered composition
      call add_scalar_trans_flag(ipol%i_wide_fil_comp,                  &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns%i_wide_fil_comp)
!
      end subroutine b_trans_address_scalar_SGS
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_vector_SGS                             &
     &         (ipol, nvector_rtp_2_rj, f_trns)
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
      end subroutine f_trans_address_vector_SGS
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_SGS                             &
     &         (ipol, nvector_rtp_2_rj, nscalar_rtp_2_rj, f_trns)
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nvector_rtp_2_rj
      integer(kind = kint), intent(inout) :: nscalar_rtp_2_rj
      type(phys_address), intent(inout) :: f_trns
!
!
      nscalar_rtp_2_rj = 0
!!   work of Reynolds stress
!!      call add_scalar_trans_flag                                      &
!!     &   (ipol%i_reynolds_wk, nvector_rtp_2_rj, nscalar_rtp_2_rj,     &
!!     &    f_trns%i_reynolds_wk)
!!   work of SGS buoyancy
!!      call add_scalar_trans_flag                                      &
!!     &   (ipol%i_SGS_buo_wk, nvector_rtp_2_rj, nscalar_rtp_2_rj,      &
!!     &    f_trns%i_SGS_buo_wk)
!!   work of SGS compositional buoyancy
!!      call add_scalar_trans_flag                                      &
!!     &   (ipol%i_SGS_comp_buo_wk, nvector_rtp_2_rj, nscalar_rtp_2_rj, &
!!     &    f_trns%i_SGS_comp_buo_wk)
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
      end subroutine f_trans_address_scalar_SGS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_Csim                            &
     &         (ipol, nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns)
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
