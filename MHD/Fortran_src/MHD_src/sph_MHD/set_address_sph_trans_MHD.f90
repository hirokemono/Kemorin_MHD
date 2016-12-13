!>@file   set_address_sph_trans_MHD.f90
!!@brief  module set_address_sph_trans_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_trans_sph_MHD(ipol, trns_MHD,          &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!      subroutine check_address_trans_sph_MHD                          &
!!     &         (ipol, idpdr, itor, trns_MHD, ncomp_sph_trans)
!!        type(phys_address), intent(in) :: ipol, idpdr, itor
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!@endverbatim
!
      module set_address_sph_trans_MHD
!
      use m_precision
!
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
      private :: b_trans_address_vector_MHD
      private :: b_trans_address_scalar_MHD
      private :: f_trans_address_vector_MHD
      private :: f_trans_address_scalar_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_MHD(ipol, trns_MHD,            &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use m_control_parameter
!
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint) :: nscltsr_rtp_2_rj, nscltsr_rj_2_rtp
!
      call b_trans_address_vector_MHD                                   &
     &   (ipol, trns_MHD%nvector_rj_2_rtp, trns_MHD%b_trns)
      call b_trans_address_scalar_MHD                                   &
     &   (ipol, trns_MHD%nvector_rj_2_rtp, trns_MHD%nscalar_rj_2_rtp,   &
     &    trns_MHD%b_trns)
      trns_MHD%ntensor_rj_2_rtp = 0
!
      call f_trans_address_vector_MHD                                   &
     &   (ipol, trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns)
      call f_trans_address_scalar_MHD                                   &
     &   (trns_MHD%nvector_rtp_2_rj, trns_MHD%nscalar_rtp_2_rj,         &
     &    trns_MHD%f_trns)
      trns_MHD%ntensor_rtp_2_rj = 0
!
      nscltsr_rtp_2_rj                                                  &
     &      = trns_MHD%nscalar_rj_2_rtp + 6*trns_MHD%ntensor_rj_2_rtp
      trns_MHD%ncomp_rj_2_rtp                                           &
     &      = 3*trns_MHD%nvector_rj_2_rtp + nscltsr_rtp_2_rj
!
      nscltsr_rj_2_rtp                                                  &
     &      = trns_MHD%nscalar_rtp_2_rj + 6*trns_MHD%ntensor_rtp_2_rj
      trns_MHD%ncomp_rtp_2_rj                                           &
     &      = 3*trns_MHD%nvector_rtp_2_rj + nscltsr_rj_2_rtp
!
      ncomp_sph_trans                                                   &
     &      = max(trns_MHD%ncomp_rj_2_rtp, trns_MHD%ncomp_rtp_2_rj)
      nvector_sph_trans                                                 &
     &      = max(trns_MHD%nvector_rj_2_rtp, trns_MHD%nvector_rtp_2_rj)
      nscalar_sph_trans = max(nscltsr_rtp_2_rj, nscltsr_rj_2_rtp)
!
      end subroutine set_addresses_trans_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine check_address_trans_sph_MHD                            &
     &         (ipol, idpdr, itor, iphys, trns_MHD, ncomp_sph_trans)
!
      use check_address_sph_trans
!
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(in) :: trns_MHD
      integer(kind = kint), intent(in) :: ncomp_sph_trans
!
!
      write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
      write(*,*) 'addresses of spherical transform for MHD'
!
      call check_add_trans_sph_MHD                                      &
     &   (ipol, idpdr, itor, iphys, trns_MHD%b_trns, trns_MHD%f_trns,   &
     &    trns_MHD%ncomp_rj_2_rtp, trns_MHD%nvector_rj_2_rtp,           &
     &    trns_MHD%nscalar_rj_2_rtp, trns_MHD%ncomp_rtp_2_rj,           &
     &    trns_MHD%nvector_rtp_2_rj, trns_MHD%nscalar_rtp_2_rj)
!
      end subroutine check_address_trans_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_vector_MHD                             &
     &         (ipol, nvector_rj_2_rtp, b_trns)
!
      use m_control_parameter
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(inout) :: nvector_rj_2_rtp
      type(phys_address), intent(inout) :: b_trns
!
      nvector_rj_2_rtp = 0
!   velocity flag
      if(       iflag_t_evo_4_velo .gt.     id_no_evolution             &
     &     .or. iflag_t_evo_4_magne .gt.    id_no_evolution             &
     &     .or. iflag_t_evo_4_temp .gt.     id_no_evolution             &
     &     .or. evo_comp%iflag_scheme .gt. id_no_evolution) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_velo = 3*nvector_rj_2_rtp - 2
      end if
!   vorticity flag
      if(       iflag_t_evo_4_velo .gt. id_no_evolution) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_vort = 3*nvector_rj_2_rtp - 2
      end if
!   magnetic field flag
      if(       iflag_t_evo_4_magne .gt. id_no_evolution                &
     &     .or. iflag_4_lorentz .gt.     id_turn_OFF) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_magne = 3*nvector_rj_2_rtp - 2
      end if
!   current density flag
      if(iflag_4_lorentz .gt. id_turn_OFF) then
        nvector_rj_2_rtp = nvector_rj_2_rtp + 1
        b_trns%i_current = 3*nvector_rj_2_rtp - 2
      end if
!
!
!   filtered velocity
      call add_vector_trans_flag(ipol%i_filter_velo,                    &
     &    nvector_rj_2_rtp, b_trns%i_filter_velo)
!   filtered vorticity
      call add_vector_trans_flag(ipol%i_filter_vort,                    &
     &    nvector_rj_2_rtp, b_trns%i_filter_vort)
!   filtered magnetic field
      call add_vector_trans_flag(ipol%i_filter_magne,                   &
     &    nvector_rj_2_rtp, b_trns%i_filter_magne)
!   filtered current density
      call add_vector_trans_flag(ipol%i_filter_current,                 &
     &    nvector_rj_2_rtp, b_trns%i_filter_current)
!
!   dual filtered velocity
      call add_vector_trans_flag(ipol%i_wide_fil_velo,                  &
     &    nvector_rj_2_rtp, b_trns%i_wide_fil_velo)
!   dual filtered vorticity
      call add_vector_trans_flag(ipol%i_wide_fil_vort,                  &
     &    nvector_rj_2_rtp, b_trns%i_wide_fil_vort)
!   dual filtered magnetic field
      call add_vector_trans_flag(ipol%i_wide_fil_magne,                 &
     &    nvector_rj_2_rtp, b_trns%i_wide_fil_magne)
!   dual filtered current density
      call add_vector_trans_flag(ipol%i_wide_fil_current,               &
     &    nvector_rj_2_rtp, b_trns%i_wide_fil_current)
!
      end subroutine b_trans_address_vector_MHD
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_MHD                             &
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
!   temperature flag
      if(iflag_t_evo_4_temp .gt. id_no_evolution) then
        nscalar_rj_2_rtp = nscalar_rj_2_rtp + 1
        b_trns%i_temp = nscalar_rj_2_rtp + 3*nvector_rj_2_rtp
      end if
!   composition flag
      if(evo_comp%iflag_scheme .gt. id_no_evolution) then
        nscalar_rj_2_rtp = nscalar_rj_2_rtp + 1
        b_trns%i_light = nscalar_rj_2_rtp + 3*nvector_rj_2_rtp
      end if
!
!   filtered temperature
      call add_scalar_trans_flag(ipol%i_filter_temp,                    &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns%i_filter_temp)
!   filtered composition
      call add_scalar_trans_flag(ipol%i_filter_comp,                    &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns%i_filter_comp)
!
!   dual filtered temperature
      call add_scalar_trans_flag(ipol%i_wide_fil_temp,                  &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns%i_wide_fil_temp)
!   dual filtered composition
      call add_scalar_trans_flag(ipol%i_wide_fil_comp,                  &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns%i_wide_fil_comp)
!
      end subroutine b_trans_address_scalar_MHD
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_vector_MHD                             &
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
!   advection flag
      if(iflag_t_evo_4_velo .gt. id_no_evolution) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 1
        f_trns%i_m_advect = 3*nvector_rtp_2_rj - 2
!   Coriolis flag
        if(iflag_4_coriolis .gt. id_turn_OFF) then
          nvector_rtp_2_rj = nvector_rtp_2_rj + 1
          f_trns%i_coriolis = 3*nvector_rtp_2_rj - 2
        end if
        if(iflag_4_coriolis .gt. id_turn_OFF) then
          nvector_rtp_2_rj =      nvector_rtp_2_rj + 1
          f_trns%i_rot_Coriolis = 3*nvector_rtp_2_rj - 2
        end if
!   Lorentz flag
        if(iflag_4_lorentz .gt. id_turn_OFF) then
          nvector_rtp_2_rj = nvector_rtp_2_rj + 1
          f_trns%i_lorentz = 3*nvector_rtp_2_rj - 2
        end if
      end if
!
!   induction flag
      if(iflag_t_evo_4_magne .gt. id_no_evolution) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 1
        f_trns%i_vp_induct =  3*nvector_rtp_2_rj - 2
      end if
!
!   heat flux flag
      if(iflag_t_evo_4_temp .gt. id_no_evolution) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 1
        f_trns%i_h_flux = 3*nvector_rtp_2_rj - 2
      end if
!
!   composition flux flag
      if(evo_comp%iflag_scheme .gt. id_no_evolution) then
        nvector_rtp_2_rj = nvector_rtp_2_rj + 1
        f_trns%i_c_flux = 3*nvector_rtp_2_rj - 2
      end if
!
!
!   filtered advection flag
      call add_vector_trans_flag(ipol%i_SGS_inertia,                    &
     &    nvector_rtp_2_rj, f_trns%i_SGS_inertia)
!
!   filtered Lorentz force flag
      call add_vector_trans_flag(ipol%i_SGS_Lorentz,                    &
     &    nvector_rtp_2_rj, f_trns%i_SGS_Lorentz)
!
!   filtered induction flag
      call add_vector_trans_flag(ipol%i_SGS_vp_induct,                  &
     &    nvector_rtp_2_rj, f_trns%i_SGS_vp_induct)
!
!   filtered heat flux flag
      call add_vector_trans_flag(ipol%i_SGS_h_flux,                     &
     &    nvector_rtp_2_rj, f_trns%i_SGS_h_flux)
!
!   filtered composition flux flag
      call add_vector_trans_flag(ipol%i_SGS_c_flux,                     &
     &    nvector_rtp_2_rj, f_trns%i_SGS_c_flux)
!
      end subroutine f_trans_address_vector_MHD
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_MHD(nvector_rtp_2_rj,           &
     &          nscalar_rtp_2_rj, f_trns)
!
      use m_control_parameter
!
      integer(kind = kint), intent(in) :: nvector_rtp_2_rj
      integer(kind = kint), intent(inout) :: nscalar_rtp_2_rj
      type(phys_address), intent(inout) :: f_trns
!
!
      nscalar_rtp_2_rj = 0
!   divergence of Coriolis flux flag
      call add_scalar_trans_flag(iflag_4_coriolis,                      &
     &    nvector_rtp_2_rj, nscalar_rtp_2_rj, f_trns%i_div_Coriolis)
!
      end subroutine f_trans_address_scalar_MHD
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_MHD
