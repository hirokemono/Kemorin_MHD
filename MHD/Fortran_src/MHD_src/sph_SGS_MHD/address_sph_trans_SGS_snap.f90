!>@file   address_sph_trans_SGS_snap.f90
!!@brief  module address_sph_trans_SGS_snap
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_SGS_snap_trans                         &
!!     &         (ipol, ipol_LES, iphys, iphys_LES, trns_snap,          &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!@endverbatim
!
      module address_sph_trans_SGS_snap
!
      use m_precision
      use m_machine_parameter
!
      use t_phys_address
      use t_SGS_model_addresses
      use t_addresses_sph_transform
      use t_mesh_data
      use t_spheric_parameter
!
!
      implicit none
!
      private :: bwd_trans_address_SGS_snap, fwd_trans_address_SGS_snap
      private :: add_SGS_vector_bwd_trns_snap
      private :: add_SGS_scalar_bwd_trns_snap
      private :: add_SGS_scalar_fwd_trns_snap
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_SGS_snap_trans                           &
     &         (ipol, ipol_LES, iphys, iphys_LES, trns_snap,            &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(address_4_sph_trans), intent(inout) :: trns_snap
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*)                                                      &
     &       'Spherical transform field table for snapshot (trns_snap)'
      end if
!
      call bwd_trans_address_SGS_snap                                   &
     &   (ipol, ipol_LES, iphys, iphys_LES,                             &
     &    trns_snap%b_trns, trns_snap%b_trns_LES, trns_snap%backward)
!
      call fwd_trans_address_SGS_snap                                   &
     &   (ipol, ipol_LES, iphys, iphys_LES,                             &
     &    trns_snap%f_trns, trns_snap%f_trns_LES, trns_snap%forward)
!
      call count_num_fields_each_trans(trns_snap%backward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_snap%forward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_snap%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_snap%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_snap%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_snap%forward%num_scalar
      end if
!
      end subroutine set_addresses_SGS_snap_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine bwd_trans_address_SGS_snap                             &
     &         (ipol, ipol_LES, iphys, iphys_LES,                       &
     &          b_trns, b_trns_LES, trns_back)
!
      use address_sph_trans_snap
!
      type(phys_address), intent(in) :: ipol, iphys
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(phys_address), intent(inout) :: b_trns
      type(SGS_model_addresses), intent(inout) :: b_trns_LES
      type(address_each_sph_trans), intent(inout) :: trns_back
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
      call add_vector_4_bwd_trns_snap(ipol, iphys, b_trns, trns_back)
      call add_SGS_vector_bwd_trns_snap                                 &
     &   (ipol, ipol_LES, iphys, iphys_LES,                             &
     &    b_trns, b_trns_LES, trns_back)
      trns_back%num_vector = trns_back%nfield
!
      call add_scalar_4_bwd_trns_snap(ipol, iphys, b_trns, trns_back)
      call add_SGS_scalar_bwd_trns_snap(ipol, iphys, b_trns, trns_back)
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
      trns_back%num_tensor = 0
!
      end subroutine bwd_trans_address_SGS_snap
!
!-----------------------------------------------------------------------
!
      subroutine fwd_trans_address_SGS_snap                             &
     &         (ipol, ipol_LES, iphys, iphys_LES, f_trns, f_trns_LES,   &
     &          trns_fwd)
!
      use address_sph_trans_snap
!
      type(phys_address), intent(in) :: ipol, iphys
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(phys_address), intent(inout) :: f_trns
      type(SGS_model_addresses), intent(inout) :: f_trns_LES
      type(address_each_sph_trans), intent(inout) :: trns_fwd
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
      call add_vector_4_fwd_trns_snap(ipol, iphys, f_trns, trns_fwd)
      trns_fwd%num_vector = trns_fwd%nfield
!
      call add_scalar_4_fwd_trns_snap(ipol, iphys, f_trns, trns_fwd)
      call add_SGS_scalar_fwd_trns_snap                                 &
     &   (ipol, ipol_LES, iphys, iphys_LES, f_trns, f_trns_LES,         &
     &    trns_fwd)
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_trans_address_SGS_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_SGS_vector_bwd_trns_snap                           &
     &         (ipol, ipol_LES, iphys, iphys_LES,                       &
     &          b_trns, b_trns_LES, trns_back)
!
      use add_SGS_term_to_sph_trans
!
      type(phys_address), intent(in) :: ipol, iphys
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(phys_address), intent(inout) :: b_trns
      type(SGS_model_addresses), intent(inout) :: b_trns_LES
      type(address_each_sph_trans), intent(inout) :: trns_back
!
!
      call add_force_w_SGS_sph_trns_snap                                &
     &   (ipol_LES%frc_w_SGS, iphys_LES%frc_w_SGS,                      &
     &    b_trns_LES%frc_w_SGS, trns_back)
!
      call add_rot_SGS_4_sph_trns_snap                                  &
     &   (ipol_LES%rot_SGS, iphys_LES%rot_SGS,                          &
     &    b_trns_LES%rot_SGS, trns_back)
!
      call add_SGS_induction_sph_trns_pol                               &
     &   (ipol%SGS_term, iphys%SGS_term, b_trns%SGS_term, trns_back)
!
      end subroutine add_SGS_vector_bwd_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_SGS_scalar_bwd_trns_snap                           &
     &         (ipol, iphys, b_trns, trns_back)
!
      use add_filter_fld_to_sph_trans
      use add_SGS_term_to_sph_trans
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      call add_fil_scalar_sph_trns_snap                                 &
     &   (ipol%filter_fld, iphys%filter_fld, b_trns%filter_fld,         &
     &    trns_back)
!
      call add_div_SGS_4_sph_trns_snap                                  &
     &   (ipol%div_SGS, iphys%div_SGS, b_trns%div_SGS, trns_back)
!
      end subroutine add_SGS_scalar_bwd_trns_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_SGS_scalar_fwd_trns_snap                           &
     &         (ipol, ipol_LES, iphys, iphys_LES,                       &
     &          f_trns, f_trns_LES, trns_fwd)
!
      use add_energy_flux_4_sph_trns
      use add_SGS_eflux_to_sph_trans
      use add_Csim_4_sph_trns
!
      type(phys_address), intent(in) :: ipol, iphys
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(phys_address), intent(inout) :: f_trns
      type(SGS_model_addresses), intent(inout) :: f_trns_LES
      type(address_each_sph_trans), intent(inout) :: trns_fwd
!
!
      call add_fil_e_flux_4_sph_trns_snap                               &
     &   (ipol_LES%eflux_by_filter, iphys_LES%eflux_by_filter,          &
     &    f_trns_LES%eflux_by_filter, trns_fwd)
!
      call add_SGS_eflux_sph_trns_snap                                  &
     &   (ipol%SGS_ene_flux, iphys%SGS_ene_flux, f_trns%SGS_ene_flux,   &
     &    trns_fwd)
      call add_Csim_4_sph_trns_snap                                     &
     &   (ipol%Csim, iphys%Csim, f_trns%Csim, trns_fwd)
!
      end subroutine add_SGS_scalar_fwd_trns_snap
!
!-----------------------------------------------------------------------
!
      end module address_sph_trans_SGS_snap
