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
!!     &         (d_rj, ipol_LES, iphys_LES, trns_SGS_snap,             &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_address_sph_trans), intent(inout) :: trns_SGS_snap
!!@endverbatim
!
      module address_sph_trans_SGS_snap
!
      use m_precision
      use m_machine_parameter
!
      use t_phys_data
      use t_SGS_model_addresses
      use t_sph_trans_arrays_SGS_MHD
      use t_mesh_data
      use t_spheric_parameter
!
!
      implicit none
!
      private :: bwd_trans_address_SGS_snap, fwd_trans_address_SGS_snap
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_SGS_snap_trans                           &
     &         (d_rj, ipol_LES, iphys_LES, trns_SGS_snap,               &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS_snap
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*)                                                      &
     &       'Spherical transform field table ',                        &
     &       'for SGS terms in snapshot (trns_SGS_snap)'
      end if
!
      call bwd_trans_address_SGS_snap                                   &
     &   (d_rj, ipol_LES, iphys_LES, trns_SGS_snap%b_trns_LES,          &
     &    trns_SGS_snap%backward)
!
      call fwd_trans_address_SGS_snap                                   &
     &   (d_rj, ipol_LES, iphys_LES, trns_SGS_snap%f_trns_LES,          &
     &    trns_SGS_snap%forward)
!
      call count_num_fields_each_trans(trns_SGS_snap%backward,          &
     &    ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_SGS_snap%forward,           &
     &    ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ',                                 &
     &            trns_SGS_snap%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ',                                 &
     &            trns_SGS_snap%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ',                                 &
     &            trns_SGS_snap%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ',                                 &
     &            trns_SGS_snap%forward%num_scalar
      end if
!
      end subroutine set_addresses_SGS_snap_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine bwd_trans_address_SGS_snap                             &
     &         (d_rj, ipol_LES, iphys_LES, b_trns_LES, trns_back)
!
      use address_sph_trans_snap
      use add_base_force_4_sph_trns
      use add_SGS_term_to_sph_trans
      use add_filter_fld_to_sph_trans
      use add_filter_force_4_sph_trns
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_model_addresses), intent(inout) :: b_trns_LES
      type(spherical_transform_data), intent(inout) :: trns_back
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, grid data'
      end if
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
!   Vectors
      call add_filter_force_4_bwd_trns                                  &
     &   (d_rj, ipol_LES%force_by_filter, iphys_LES%force_by_filter,    &
     &    b_trns_LES%force_by_filter, trns_back)
      call add_rot_force_4_sph_trns_snap(d_rj,                          &
     &    ipol_LES%rot_frc_by_filter, iphys_LES%rot_frc_by_filter,      &
     &    b_trns_LES%rot_frc_by_filter, trns_back)
!
      call add_force_w_SGS_sph_trns_snap                                &
     &   (d_rj, ipol_LES%frc_w_SGS, iphys_LES%frc_w_SGS,                &
     &    b_trns_LES%frc_w_SGS, trns_back)
      call add_rot_SGS_4_sph_trns_snap                                  &
     &   (d_rj, ipol_LES%rot_SGS, iphys_LES%rot_SGS,                    &
     &    b_trns_LES%rot_SGS, trns_back)
      call add_SGS_induction_sph_trns_pol                               &
     &   (d_rj, ipol_LES%SGS_term, iphys_LES%SGS_term,                  &
     &    b_trns_LES%SGS_term, trns_back)
      trns_back%num_vector = trns_back%nfield
!
!   Scalars
      call add_fil_scalar_sph_trns_snap                                 &
     &   (d_rj, ipol_LES%filter_fld, iphys_LES%filter_fld,              &
     &    b_trns_LES%filter_fld, trns_back)
      call add_div_SGS_4_sph_trns_snap                                  &
     &   (d_rj, ipol_LES%div_SGS, iphys_LES%div_SGS,                    &
     &    b_trns_LES%div_SGS, trns_back)
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
      trns_back%num_tensor = 0
!
      end subroutine bwd_trans_address_SGS_snap
!
!-----------------------------------------------------------------------
!
      subroutine fwd_trans_address_SGS_snap                             &
     &         (d_rj, ipol_LES, iphys_LES, f_trns_LES, trns_fwd)
!
      use address_sph_trans_snap
      use add_Csim_4_sph_trns
      use add_SGS_eflux_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_model_addresses), intent(inout) :: f_trns_LES
      type(spherical_transform_data), intent(inout) :: trns_fwd
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, grid data'
      end if
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
      trns_fwd%num_vector = trns_fwd%nfield
!
!   Scalars
      call add_fil_e_flux_4_sph_trns_snap                               &
     &   (d_rj, ipol_LES%eflux_by_filter, iphys_LES%eflux_by_filter,    &
     &    f_trns_LES%eflux_by_filter, trns_fwd)
      call add_SGS_eflux_sph_trns_snap                                  &
     &   (d_rj, ipol_LES%SGS_ene_flux, iphys_LES%SGS_ene_flux,          &
     &    f_trns_LES%SGS_ene_flux, trns_fwd)
      call add_Csim_4_sph_trns_snap                                     &
     &   (d_rj, ipol_LES%Csim, iphys_LES%Csim, f_trns_LES%Csim,         &
     &    trns_fwd)
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_trans_address_SGS_snap
!
!-----------------------------------------------------------------------
!
      end module address_sph_trans_SGS_snap
