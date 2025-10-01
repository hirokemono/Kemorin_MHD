!>@file   address_sph_trans_dyn_simi.f90
!!@brief  module address_sph_trans_dyn_simi
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine empty_trans_address(trns_empty)
!!        type(spherical_transform_data), intent(inout) :: trns_empty
!!
!!      subroutine bwd_sph_trns_fld_similarity                          &
!!     &         (d_rj, ipol_LES, iphys_LES, b_trns_LES, trns_bwd)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_model_addresses), intent(inout) :: b_trns_LES
!!        type(spherical_transform_data), intent(inout) :: trns_bwd
!!      subroutine fwd_sph_trns_fld_similarity                          &
!!     &         (d_rj, ipol_LES, iphys_LES, f_trns_LES, trns_fwd)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_model_addresses), intent(inout) :: f_trns_LES
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!
!!      subroutine bwd_trans_address_dyn_simi                           &
!!     &         (d_rj, ipol_LES, iphys_LES, b_trns_LES, trns_bwd)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(inout) :: b_trns_LES
!!        type(spherical_transform_data), intent(inout) :: trns_bwd
!!
!!      subroutine set_sph_trns_address_fld_simi                        &
!!     &         (d_rj, ipol_LES, iphys_LES, trns_SIMI,                 &
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_model_addresses), intent(inout) :: f_trns_LES
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!@endverbatim
!
      module address_sph_trans_dyn_simi
!
      use m_precision
      use m_machine_parameter
!
      use t_phys_data
      use t_SGS_model_addresses
      use t_sph_trans_arrays_SGS_MHD
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine empty_trans_address(trns_empty)
!
      type(spherical_transform_data), intent(inout) :: trns_empty
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for Empty transform: '
      end if
!
      trns_empty%nfield = 0
      call alloc_sph_trns_field_name(trns_empty)
!
      trns_empty%num_vector = trns_empty%nfield
      trns_empty%num_scalar = trns_empty%nfield - trns_empty%num_vector
      trns_empty%num_tensor = 0
!
      end subroutine empty_trans_address
!
!-----------------------------------------------------------------------
!
      subroutine bwd_sph_trns_fld_similarity                            &
     &         (d_rj, ipol_LES, iphys_LES, b_trns_LES, trns_bwd)
!
      use add_SGS_term_to_sph_trans
      use add_filter_fld_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_model_addresses), intent(inout) :: b_trns_LES
      type(spherical_transform_data), intent(inout) :: trns_bwd
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &             'transform, poloidal, grid data'
      end if
!
      trns_bwd%nfield = 0
      call alloc_sph_trns_field_name(trns_bwd)
!
!   filtered vector
      call add_fil_vector_sph_trns_by_pol                               &
     &   (d_rj, ipol_LES%filter_fld, iphys_LES%filter_fld,              &
     &    b_trns_LES%filter_fld, trns_bwd)
!   filtered nonlinear terms (in SGS term address)
      call add_SGS_term_4_sph_trns_by_pol                               &
     &   (d_rj, ipol_LES%SGS_term, iphys_LES%SGS_term,                  &
     &    b_trns_LES%SGS_term, trns_bwd)
      trns_bwd%num_vector = trns_bwd%nfield
!
!   filtered scalar
      call add_fil_scalar_sph_trns_by_pol                               &
     &   (d_rj, ipol_LES%filter_fld, iphys_LES%filter_fld,              &
     &    b_trns_LES%filter_fld, trns_bwd)
      trns_bwd%num_scalar = trns_bwd%nfield - trns_bwd%num_vector
      trns_bwd%num_tensor = 0
!
      end subroutine bwd_sph_trns_fld_similarity
!
!-----------------------------------------------------------------------
!
      subroutine fwd_sph_trns_fld_similarity                            &
     &         (d_rj, ipol_LES, iphys_LES, f_trns_LES, trns_fwd)
!
      use add_SGS_term_to_sph_trans
      use add_filter_fld_to_sph_trans
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
      call add_SGS_term_4_sph_trns_by_pol                               &
     &   (d_rj, ipol_LES%SGS_term, iphys_LES%SGS_term,                  &
     &    f_trns_LES%SGS_term, trns_fwd)
      trns_fwd%num_vector = trns_fwd%nfield
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_sph_trns_fld_similarity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine bwd_trans_address_dyn_simi                             &
     &         (d_rj, ipol_LES, iphys_LES, b_trns_LES, trns_bwd)
!
      use add_SGS_term_to_sph_trans
      use add_wide_f_fld_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_model_addresses), intent(inout) :: b_trns_LES
      type(spherical_transform_data), intent(inout) :: trns_bwd
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, grid data'
      end if
!
      trns_bwd%nfield = 0
      call alloc_sph_trns_field_name(trns_bwd)
!
      call add_wide_fil_vector_sph_trns                                 &
     &   (d_rj, ipol_LES%wide_filter_fld, iphys_LES%wide_filter_fld,    &
     &    b_trns_LES%wide_filter_fld, trns_bwd)
      call add_wide_SGS_term_4_sph_trns                                 &
     &   (d_rj, ipol_LES%wide_SGS, iphys_LES%wide_SGS,                  &
     &    b_trns_LES%wide_SGS, trns_bwd)
      call add_double_SGS_term_4_sph_trns                               &
     &   (d_rj, ipol_LES%dble_SGS, iphys_LES%dble_SGS,                  &
     &    b_trns_LES%dble_SGS, trns_bwd)
      trns_bwd%num_vector = trns_bwd%nfield
!
      call add_wide_fil_scalar_sph_trns                                 &
     &   (d_rj, ipol_LES%wide_filter_fld, iphys_LES%wide_filter_fld,    &
     &    b_trns_LES%wide_filter_fld, trns_bwd)
      trns_bwd%num_scalar = trns_bwd%nfield - trns_bwd%num_vector
      trns_bwd%num_tensor = 0
!
      end subroutine bwd_trans_address_dyn_simi
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fwd_trans_address_Csim(SGS_param, d_rj,                &
     &          ipol_LES, iphys_LES, f_trns_LES, trns_fwd)
!
      use t_SGS_control_parameter
      use add_Csim_4_sph_trns
      use add_SGS_term_to_sph_trans
      use add_SGS_eflux_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_control_params), intent(in) :: SGS_param
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
      trns_fwd%num_vector = 0
      call add_Csim_4_sph_trns_by_pol                                   &
     &   (d_rj, ipol_LES%Csim, iphys_LES%Csim,                          &
     &    f_trns_LES%Csim, trns_fwd)
      if(SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
        call add_SGS_eflux_sph_trns_by_pol                              &
     &     (d_rj, ipol_LES%SGS_ene_flux, iphys_LES%SGS_ene_flux,        &
     &      f_trns_LES%SGS_ene_flux, trns_fwd)
      end if
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_trans_address_Csim
!
!-----------------------------------------------------------------------
!
      end module address_sph_trans_dyn_simi
