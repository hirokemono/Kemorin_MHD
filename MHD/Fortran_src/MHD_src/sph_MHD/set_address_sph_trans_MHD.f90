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
!!      subroutine set_addresses_trans_sph_MHD(MHD_prop, ipol, trns_MHD,&
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
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
      use t_control_parameter
      use t_physical_property
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
      subroutine set_addresses_trans_sph_MHD(MHD_prop, ipol, trns_MHD,  &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      call b_trans_address_vector_MHD                                   &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    ipol, trns_MHD%nvector_rj_2_rtp, trns_MHD%b_trns)
      call b_trans_address_scalar_MHD                                   &
     &   (MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    ipol, trns_MHD%nvector_rj_2_rtp, trns_MHD%nscalar_rj_2_rtp,   &
     &    trns_MHD%b_trns)
      trns_MHD%ntensor_rj_2_rtp = 0
!
      call f_trans_address_vector_MHD                                   &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, ipol,                     &
     &    trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns)
      call f_trans_address_scalar_MHD                                   &
     &   (MHD_prop%fl_prop, trns_MHD%nvector_rtp_2_rj,                  &
     &    trns_MHD%nscalar_rtp_2_rj, trns_MHD%f_trns)
      trns_MHD%ntensor_rtp_2_rj = 0
!
      ncomp_sph_trans =   0
      nvector_sph_trans = 0
      nscalar_sph_trans = 0
      call count_num_fields_4_sph_trans(trns_MHD, ncomp_sph_trans,      &
     &   nvector_sph_trans, nscalar_sph_trans)
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
     &         (fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          ipol, nvector_rj_2_rtp, b_trns)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(inout) :: nvector_rj_2_rtp
      type(phys_address), intent(inout) :: b_trns
!
      nvector_rj_2_rtp = 0
!   velocity flag
      if(       fl_prop%iflag_scheme .gt. id_no_evolution               &
     &     .or. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution          &
     &     .or. ht_prop%iflag_scheme .gt. id_no_evolution               &
     &     .or. cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_vector_trans_flag                                      &
     &     (ipol%i_velo, nvector_rj_2_rtp, b_trns%i_velo)
      end if
!   vorticity flag
      if(       fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_vector_trans_flag                                      &
     &     (ipol%i_vort, nvector_rj_2_rtp, b_trns%i_vort)
      end if
!   magnetic field flag
      if(       cd_prop%iflag_Bevo_scheme .gt. id_no_evolution          &
     &     .or. fl_prop%iflag_4_lorentz .gt.     id_turn_OFF) then
        call add_vector_trans_flag                                      &
     &     (ipol%i_magne, nvector_rj_2_rtp, b_trns%i_magne)
      end if
!   current density flag
      if(fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
        call add_vector_trans_flag                                      &
     &     (ipol%i_current, nvector_rj_2_rtp, b_trns%i_current)
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
      subroutine b_trans_address_scalar_MHD(ht_prop, cp_prop,           &
     &          ipol, nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns)
!
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nvector_rj_2_rtp
      integer(kind = kint), intent(inout) :: nscalar_rj_2_rtp
      type(phys_address), intent(inout) :: b_trns
!
!
      nscalar_rj_2_rtp = 0
!   temperature flag
      if(ht_prop%iflag_scheme .gt. id_no_evolution) then
        call add_scalar_trans_flag(ipol%i_temp,                         &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns%i_temp)
      end if
!   composition flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_scalar_trans_flag(ipol%i_light,                        &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp, b_trns%i_light)
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
     &         (fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          ipol, nvector_rtp_2_rj, f_trns)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(inout) :: f_trns
      integer(kind = kint), intent(inout) :: nvector_rtp_2_rj
!
!
      nvector_rtp_2_rj = 0
!   advection flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_vector_trans_flag(ipol%i_m_advect,                     &
     &      nvector_rtp_2_rj, f_trns%i_m_advect)
!   Coriolis flag
        if(fl_prop%iflag_4_coriolis .gt. id_turn_OFF) then
          call add_vector_trans_flag(ipol%i_coriolis,                   &
     &        nvector_rtp_2_rj, f_trns%i_coriolis)
        end if
        if(fl_prop%iflag_4_coriolis .gt. id_turn_OFF) then
          call add_vector_trans_flag(ipol%i_rot_Coriolis,               &
     &        nvector_rtp_2_rj, f_trns%i_rot_Coriolis)
        end if
!   Lorentz flag
        if(fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
          call add_vector_trans_flag(ipol%i_lorentz,                    &
     &        nvector_rtp_2_rj, f_trns%i_lorentz)
        end if
      end if
!
!   induction flag
      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call add_vector_trans_flag(ipol%i_vp_induct,                    &
     &      nvector_rtp_2_rj, f_trns%i_vp_induct)
      end if
!
!   heat flux flag
      if(ht_prop%iflag_scheme .gt. id_no_evolution) then
        call add_vector_trans_flag(ipol%i_h_flux,                       &
     &      nvector_rtp_2_rj, f_trns%i_h_flux)
      end if
!
!   composition flux flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_vector_trans_flag(ipol%i_c_flux,                       &
     &      nvector_rtp_2_rj, f_trns%i_c_flux)
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
      subroutine f_trans_address_scalar_MHD(fl_prop,                    &
     &          nvector_rtp_2_rj, nscalar_rtp_2_rj, f_trns)
!
      type(fluid_property), intent(in) :: fl_prop
      integer(kind = kint), intent(in) :: nvector_rtp_2_rj
      integer(kind = kint), intent(inout) :: nscalar_rtp_2_rj
      type(phys_address), intent(inout) :: f_trns
!
!
      nscalar_rtp_2_rj = 0
!   divergence of Coriolis flux flag
      call add_scalar_trans_flag(fl_prop%iflag_4_coriolis,              &
     &    nvector_rtp_2_rj, nscalar_rtp_2_rj, f_trns%i_div_Coriolis)
!
      end subroutine f_trans_address_scalar_MHD
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_MHD
