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
      integer(kind = kint) :: icou
!
!
      call b_trans_address_vector_SGS(SPH_MHD%ipol, trns_SGS)
      call b_trans_address_scalar_SGS(SPH_MHD%ipol, trns_SGS)
      trns_SGS%ntensor_rj_2_rtp = 0
!
      call f_trans_address_vector_SGS(SPH_MHD%ipol, trns_SGS)
      call f_trans_address_scalar_SGS(SPH_MHD%ipol, trns_SGS)
      trns_SGS%ntensor_rtp_2_rj = 0
!
      call count_num_fields_4_sph_trans(trns_SGS, ncomp_sph_trans,      &
     &   nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table for similarity SGS'
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_SGS%nvector_rj_2_rtp
        write(*,*) 'nscalar_rj_2_rtp ', trns_SGS%nscalar_rj_2_rtp
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      icou = 0
      call set_b_trans_vector_field_SGS                                 &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_SGS)
      call set_b_trans_scalar_field_SGS                                 &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_SGS)
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'nvector_rtp_2_rj ', trns_SGS%nvector_rtp_2_rj
        write(*,*) 'nscalar_rtp_2_rj ', trns_SGS%nscalar_rtp_2_rj
        write(*,*) 'Address for forward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      icou = 0
      call set_f_trans_vector_field_SGS                                 &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_SGS)
      call set_f_trans_scalar_field_SGS                                 &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_SGS)
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
        write(*,*) 'Address for forward transform: ',                   &
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
      integer(kind = kint):: icou
!
!
      call b_trans_address_vector_Csim(SPH_MHD%ipol, trns_Csim)
      call b_trans_address_scalar_Csim(SPH_MHD%ipol, trns_Csim)
      trns_Csim%ntensor_rj_2_rtp = 0
!
      call f_trans_address_vector_Csim(trns_Csim)
      call f_trans_address_scalar_Csim(SPH_MHD%ipol, trns_Csim)
      trns_Csim%ntensor_rtp_2_rj = 0
!
      call count_num_fields_4_sph_trans(trns_Csim, ncomp_sph_trans,     &
     &    nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table for model coefs'
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_Csim%nvector_rj_2_rtp
        write(*,*) 'nscalar_rj_2_rtp ', trns_Csim%nscalar_rj_2_rtp
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      icou = 0
      call set_b_trans_vector_field_Csim                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_Csim)
      call set_b_trans_scalar_field_Csim                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_Csim)
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'nvector_rtp_2_rj ', trns_Csim%nvector_rtp_2_rj
        write(*,*) 'nscalar_rtp_2_rj ', trns_Csim%nscalar_rtp_2_rj
        write(*,*) 'Address for forward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      icou = 0
      call set_f_trans_vector_field_Csim                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_Csim)
      call set_f_trans_scalar_field_Csim                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_Csim)
!
      end subroutine set_addresses_trans_sph_Csim
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_SGS
