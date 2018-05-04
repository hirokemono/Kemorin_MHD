!>@file   set_address_sph_trans_ngSGS.f90
!!@brief  module set_address_sph_trans_ngSGS
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_trans_sph_ngTMP                        &
!!     &         (SPH_MHD, iphys, trns_ngTMP,                           &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_ngTMP
!!      subroutine set_addresses_trans_sph_ngSGS                        &
!!     &         (SPH_MHD, iphys, trns_SGS,                             &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!
!!      subroutine set_addresses_trans_sph_ngDYNS                       &
!!     &         (SPH_MHD, iphys, trns_DYNS,                            &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_DYNS
!!@endverbatim
!
      module set_address_sph_trans_ngSGS
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
      subroutine set_addresses_trans_sph_ngTMP                          &
     &         (SPH_MHD, iphys, trns_ngTMP,                             &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_ngSGS
      use address_fwd_sph_trans_ngSGS
!
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_ngTMP
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &              'for nonlinear Gradient SGS (trns_ngTMP)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      trns_ngTMP%backward%nfield = 0
      call alloc_sph_trns_field_name(trns_ngTMP%backward)
!
      trns_ngTMP%backward%num_vector = trns_ngTMP%backward%nfield
      trns_ngTMP%backward%num_scalar = trns_ngTMP%backward%nfield       &
     &                              - trns_ngTMP%backward%num_vector
      trns_ngTMP%backward%num_tensor = 0
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call f_trans_address_vector_ngSGS(trns_ngTMP%forward)
      call f_trans_address_scalar_ngSGS(SPH_MHD%ipol, SPH_MHD%itor,     &
     &    iphys, trns_ngTMP%f_trns, trns_ngTMP%forward)
      trns_ngTMP%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_ngTMP%backward,             &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_ngTMP%forward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_ngTMP%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_ngTMP%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_ngTMP%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_ngTMP%forward%num_scalar
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      end subroutine set_addresses_trans_sph_ngTMP
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_ngDTMP                         &
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
        write(*,*) 'Spherical transform field table ',                  &
     &     'for temporal Dynamic nonlinear Gradient SGS (trns_SIMI)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call b_trans_address_vector_SGS(SPH_MHD%ipol, SPH_MHD%itor,       &
     &    iphys, trns_SIMI%b_trns, trns_SIMI%backward)
      call b_trans_address_scalar_SGS(SPH_MHD%ipol, SPH_MHD%itor,       &
     &    iphys, trns_SIMI%b_trns, trns_SIMI%backward)
      trns_SIMI%backward%num_tensor = 0
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call f_trans_address_vector_SGS(SPH_MHD%ipol, SPH_MHD%itor,       &
     &    iphys, trns_SIMI%f_trns, trns_SIMI%forward)
      call f_trans_address_scalar_SGS(trns_SIMI%forward)
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
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      end subroutine set_addresses_trans_sph_ngDTMP
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_ngSGS                          &
     &         (SPH_MHD, iphys, trns_SGS,                               &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_ngSGS
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
        write(*,*) 'Spherical transform field table ',                  &
     &              'for nonlinear Gradient SGS (trns_SGS)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call b_trans_address_vector_ngSGS(SPH_MHD%ipol, SPH_MHD%itor,     &
     &    iphys, trns_SGS%b_trns, trns_SGS%backward)
      call b_trans_address_scalar_ngSGS(trns_SGS%backward)
      trns_SGS%backward%num_tensor = 0
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call f_trans_address_vector_SGS(SPH_MHD%ipol, SPH_MHD%itor,       &
     &    iphys, trns_SGS%f_trns, trns_SGS%forward)
      call f_trans_address_scalar_SGS(trns_SGS%forward)
      trns_SGS%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_SGS%backward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_SGS%forward,                &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_SGS%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_SGS%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_SGS%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_SGS%forward%num_scalar
      end if
!
      end subroutine set_addresses_trans_sph_ngSGS
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_ngDYNS                         &
     &         (SPH_MHD, iphys, trns_DYNS,                              &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_dnlg
      use address_fwd_sph_trans_dnlg
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
        write(*,*) 'Spherical transform field table ',                  &
     &             'for dynamnic nonlinear gradient model (trns_DYNS)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call b_trans_address_vector_dnlg(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_DYNS%b_trns, trns_DYNS%backward)
      call b_trans_address_scalar_dnlg(trns_DYNS%backward)
      trns_DYNS%backward%num_tensor = 0
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call f_trans_address_vector_dnlg(trns_DYNS%forward)
      call f_trans_address_scalar_dnlg(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_DYNS%f_trns, trns_DYNS%forward)
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
      end subroutine set_addresses_trans_sph_ngDYNS
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_ngCsim                         &
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
        write(*,*) 'Spherical transform field table for model coefs ',  &
     &             'by nonlinear gradient model (SGS_Csim)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call b_trans_address_vector_ngCsim(SPH_MHD%ipol, SPH_MHD%itor,    &
     &    iphys, trns_Csim%b_trns, trns_Csim%backward)
      call b_trans_address_scalar_Csim(trns_Csim%backward)
      trns_Csim%backward%num_tensor = 0
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      call f_trans_address_vector_Csim(trns_Csim%forward)
      call f_trans_address_scalar_Csim(SPH_MHD%ipol, SPH_MHD%itor,      &
     &    iphys, trns_Csim%f_trns, trns_Csim%forward)
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
      end subroutine set_addresses_trans_sph_ngCsim
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_ngSGS
