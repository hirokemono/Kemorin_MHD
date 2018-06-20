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
!!      subroutine init_sph_trns_fld_similarity                         &
!!     &         (SPH_MHD, iphys, trns_SIMI,                            &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_SIMI
!!      subroutine init_sph_trns_fld_dyn_simi                           &
!!     &         (SPH_MHD, iphys, trns_DYNS,                            &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_DYNS
!!      subroutine set_addresses_trans_sph_Csim                         &
!!     &         (SPH_MHD, iphys, trns_Csim,                            &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(address_4_sph_trans), intent(inout) :: trns_Csim
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
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_trns_fld_similarity                           &
     &         (SPH_MHD, iphys, trns_SIMI,                              &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_SGS
      use address_fwd_sph_trans_SGS
!
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_SIMI
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_SIMI%backward%nfield = 0
      call alloc_sph_trns_field_name(trns_SIMI%backward)
!
      call b_trans_vector_similarity(SPH_MHD%ipol, SPH_MHD%itor,        &
     &    iphys, trns_SIMI%b_trns, trns_SIMI%backward)
      trns_SIMI%backward%num_vector = trns_SIMI%backward%nfield
!
      call b_trans_scalar_similarity(SPH_MHD%ipol, SPH_MHD%itor,        &
     &    iphys, trns_SIMI%b_trns, trns_SIMI%backward)
      trns_SIMI%backward%num_scalar = trns_SIMI%backward%nfield         &
     &                              - trns_SIMI%backward%num_vector
      trns_SIMI%backward%num_tensor = 0
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_SIMI%forward%nfield = 0
      call alloc_sph_trns_field_name(trns_SIMI%forward)
!
      call f_trans_vector_SGS_terms(SPH_MHD%ipol, SPH_MHD%itor,         &
     &    iphys, trns_SIMI%f_trns, trns_SIMI%forward)
      trns_SIMI%forward%num_vector = trns_SIMI%forward%nfield
      trns_SIMI%forward%num_scalar = trns_SIMI%forward%nfield           &
     &                              - trns_SIMI%forward%num_vector
      trns_SIMI%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_SIMI%backward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_SIMI%forward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_SIMI%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_SIMI%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_SIMI%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_SIMI%forward%num_scalar
      end if
!
      end subroutine init_sph_trns_fld_similarity
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_trns_fld_dyn_simi                             &
     &         (SPH_MHD, iphys, trns_DYNS,                              &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_SGS
      use address_fwd_sph_trans_SGS
!
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_DYNS
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table for dynamnic SGS'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_DYNS%backward%nfield = 0
      call alloc_sph_trns_field_name(trns_DYNS%backward)
!
      call b_trans_vector_wide_filter_fld(SPH_MHD%ipol, SPH_MHD%itor,   &
     &    iphys, trns_DYNS%b_trns, trns_DYNS%backward)
      call b_trans_vector_wide_similarity(SPH_MHD%ipol, SPH_MHD%itor,   &
     &    iphys, trns_DYNS%b_trns, trns_DYNS%backward)
      call b_trans_vector_filtered_SGS(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_DYNS%b_trns, trns_DYNS%backward)
      trns_DYNS%backward%num_vector = trns_DYNS%backward%nfield
!
      call b_trans_scalar_wide_filter_fld(SPH_MHD%ipol, SPH_MHD%itor,   &
     &    iphys, trns_DYNS%b_trns, trns_DYNS%backward)
      trns_DYNS%backward%num_scalar = trns_DYNS%backward%nfield         &
     &                               - trns_DYNS%backward%num_vector
      trns_DYNS%backward%num_tensor = 0
!
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_DYNS%forward%nfield = 0
      call alloc_sph_trns_field_name(trns_DYNS%forward)
!
      trns_DYNS%forward%num_vector = 0
      call f_trans_address_SGS_works(SPH_MHD%ipol, SPH_MHD%itor,        &
     &    iphys, trns_DYNS%f_trns, trns_DYNS%forward)
      trns_DYNS%forward%num_scalar = trns_DYNS%forward%nfield           &
     &                              - trns_DYNS%forward%num_vector
      trns_DYNS%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_DYNS%backward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_DYNS%forward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_DYNS%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_DYNS%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_DYNS%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_DYNS%forward%num_scalar
      end if
!
      end subroutine init_sph_trns_fld_dyn_simi
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_Csim                           &
     &         (SPH_MHD, iphys, trns_Csim,                              &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_fwd_sph_trans_Csim
!
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_Csim
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &             'for model coefs (trns_Csim)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_Csim%backward%nfield = 0
      call alloc_sph_trns_field_name(trns_Csim%backward)
!
      trns_Csim%backward%num_vector = trns_Csim%backward%nfield
      trns_Csim%backward%num_scalar = trns_Csim%backward%nfield         &
     &                               - trns_Csim%backward%num_vector
      trns_Csim%backward%num_tensor = 0
!
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_Csim%forward%nfield = 0
      call alloc_sph_trns_field_name(trns_Csim%forward)
!
      trns_Csim%forward%num_vector = 0
      call f_trans_address_scalar_Csim(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_Csim%f_trns, trns_Csim%forward)
      trns_Csim%forward%num_scalar = trns_Csim%forward%nfield           &
     &                              - trns_Csim%forward%num_vector
      trns_Csim%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_Csim%backward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_Csim%forward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_Csim%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_Csim%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_Csim%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_Csim%forward%num_scalar
      end if
!
      end subroutine set_addresses_trans_sph_Csim
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_SGS
