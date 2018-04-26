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
!!      subroutine set_addresses_trans_sph_SGS(SPH_MHD, iphys, trns_SGS,&
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!      subroutine set_addresses_trans_sph_DYNS                         &
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
      subroutine set_addresses_trans_sph_SGS(SPH_MHD, iphys, trns_SGS,  &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_SGS
      use address_fwd_sph_trans_SGS
!
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table for similarity SGS'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call b_trans_address_vector_SGS(SPH_MHD%ipol, SPH_MHD%itor,       &
     &    iphys, trns_SGS%b_trns, trns_SGS%backward)
      call b_trans_address_scalar_SGS(SPH_MHD%ipol, SPH_MHD%itor,       &
     &    iphys, trns_SGS%b_trns, trns_SGS%backward)
      trns_SGS%backward%num_tensor = 0
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call f_trans_address_vector_SGS(SPH_MHD%ipol, SPH_MHD%itor,       &
     &    iphys, trns_SGS%f_trns, trns_SGS%forward)
      call f_trans_address_scalar_SGS(trns_SGS%forward)
      trns_SGS%forward%num_tensor = 0
!
      call count_num_fields_each_trans2(trns_SGS%backward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans2(trns_SGS%forward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_SGS%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_SGS%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_SGS%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_SGS%forward%num_scalar
        write(*,*) 'Address for forward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
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
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table for dynamnic SGS'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call b_trans_address_vector_DYNS(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_DYNS%b_trns, trns_DYNS%backward)
      call b_trans_address_scalar_DYNS(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_DYNS%b_trns, trns_DYNS%backward)
      trns_DYNS%backward%num_tensor = 0
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call f_trans_address_vector_DYNS(trns_DYNS%forward)
      call f_trans_address_scalar_DYNS(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_DYNS%f_trns, trns_DYNS%forward)
      trns_DYNS%forward%num_tensor = 0
!
      call count_num_fields_each_trans2(trns_DYNS%backward,             &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans2(trns_DYNS%forward,              &
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
      end subroutine set_addresses_trans_sph_DYNS
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_Csim                           &
     &         (SPH_MHD, iphys, trns_Csim,                              &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_Csim
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
        write(*,*) 'Spherical transform field table for model coefs'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call b_trans_address_vector_Csim(trns_Csim%backward)
      call b_trans_address_scalar_Csim(trns_Csim%backward)
      trns_Csim%backward%num_tensor = 0
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call f_trans_address_vector_Csim(trns_Csim%forward)
      call f_trans_address_scalar_Csim(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_Csim%f_trns, trns_Csim%forward)
      trns_Csim%forward%num_tensor = 0
!
      call count_num_fields_each_trans2(trns_Csim%backward,             &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans2(trns_Csim%forward,              &
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
