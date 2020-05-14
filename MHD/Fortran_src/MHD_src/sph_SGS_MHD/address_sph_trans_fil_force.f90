!>@file   address_sph_trans_fil_force.f90
!!@brief  module address_sph_trans_fil_force
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine init_sph_trns_filter_MHD                             &
!!     &         (MHD_prop, ipol_LES, iphys_LES, trns_fil_MHD,          &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(phys_address), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_address_sph_trans), intent(inout) :: trns_fil_MHD
!!@endverbatim
!
      module address_sph_trans_fil_force
!
      use m_precision
      use m_machine_parameter
!
      use t_SGS_model_addresses
      use m_phys_constants
      use t_phys_address
      use t_sph_trans_arrays_SGS_MHD
      use t_addresses_sph_transform
      use t_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_trns_filter_MHD                               &
     &         (MHD_prop, ipol_LES, iphys_LES, trns_fil_MHD,            &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_address_sph_trans), intent(inout) :: trns_fil_MHD
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform table for filtered nonlinear'
      end if
!
      call bwd_trans_address_filter_MHD(MHD_prop, ipol_LES, iphys_LES,  &
     &    trns_fil_MHD%b_trns_LES, trns_fil_MHD%backward)
      call fwd_trans_address_filter_MHD(MHD_prop, ipol_LES, iphys_LES,  &
     &    trns_fil_MHD%f_trns_LES, trns_fil_MHD%forward)
!
      ncomp_sph_trans =   0
      nvector_sph_trans = 0
      nscalar_sph_trans = 0
      call count_num_fields_each_trans(trns_fil_MHD%backward,           &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_fil_MHD%forward,            &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ',                                 &
     &            trns_fil_MHD%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ',                                 &
     &            trns_fil_MHD%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ',                                 &
     &            trns_fil_MHD%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ',                                 &
     &            trns_fil_MHD%forward%num_scalar
      end if
!
      end subroutine init_sph_trns_filter_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine bwd_trans_address_filter_MHD                           &
     &         (MHD_prop, ipol_LES, iphys_LES, b_trns_LES, trns_back)
!
      use add_filter_field_4_sph_trns
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_model_addresses), intent(inout) :: b_trns_LES
      type(spherical_transform_data), intent(inout) :: trns_back
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
!      Vectors
      call add_filter_MHD_vec_sph_trns(MHD_prop%fl_prop,                &
     &    MHD_prop%cd_prop, MHD_prop%ht_prop, MHD_prop%cp_prop,         &
     &    ipol_LES%filter_fld, iphys_LES%filter_fld,                    &
     &    b_trns_LES%filter_fld, trns_back)
      trns_back%num_vector = trns_back%nfield
!
!      Scalars
      call add_filter_MHD_scl_sph_trns                                  &
     &   (MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    ipol_LES%filter_fld, iphys_LES%filter_fld,                    &
     &    b_trns_LES%filter_fld, trns_back)
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
      trns_back%num_tensor = 0
!
      end subroutine bwd_trans_address_filter_MHD
!
!-----------------------------------------------------------------------
!
      subroutine fwd_trans_address_filter_MHD                           &
     &         (MHD_prop, ipol_LES, iphys_LES, f_trns_LES, trns_fwd)
!
      use add_filter_force_4_sph_trns
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!
      type(SGS_model_addresses), intent(inout) :: f_trns_LES
      type(spherical_transform_data), intent(inout) :: trns_fwd
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
!   forces
      call add_filter_force_MHD_sph_trns(MHD_prop%fl_prop,              &
     &    MHD_prop%cd_prop, MHD_prop%ht_prop, MHD_prop%cp_prop,         &
     &    ipol_LES%force_by_filter, iphys_LES%force_by_filter,          &
     &    f_trns_LES%force_by_filter, trns_fwd)
      trns_fwd%num_vector = trns_fwd%nfield
!
      trns_fwd%num_scalar = 0
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_trans_address_filter_MHD
!
!-----------------------------------------------------------------------
!
      end module address_sph_trans_fil_force
