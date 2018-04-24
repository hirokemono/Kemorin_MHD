!>@file   set_address_sph_trans_snap.f90
!!@brief  module set_address_sph_trans_snap
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_snapshot_trans                         &
!!     &         (SPH_MHD, iphys, trns_snap,                            &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!      subroutine set_addresses_temporal_trans                         &
!!     &         (SPH_MHD, iphys, trns_tmp,                             &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_tmp
!!@endverbatim
!
      module set_address_sph_trans_snap
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
      subroutine set_addresses_snapshot_trans                           &
     &         (SPH_MHD, iphys, trns_snap,                              &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_snap
      use address_fwd_sph_trans_snap
!
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint):: icou
!
!
      call b_trans_address_vector_snap(SPH_MHD%ipol, iphys, trns_snap)
      call b_trans_address_scalar_snap(SPH_MHD%ipol, iphys, trns_snap)
      trns_snap%ntensor_rj_2_rtp = 0
!
      call f_trans_address_vector_snap(SPH_MHD%ipol, iphys, trns_snap)
      call f_trans_address_scalar_snap(SPH_MHD%ipol, iphys, trns_snap)
       trns_snap%ntensor_rtp_2_rj = 0
!
      call count_num_fields_4_sph_trans(trns_snap, ncomp_sph_trans,     &
     &   nvector_sph_trans, nscalar_sph_trans)
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table for snapshot'
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_snap%nvector_rj_2_rtp
        write(*,*) 'nscalar_rj_2_rtp ', trns_snap%nscalar_rj_2_rtp
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      icou = 0
      call set_b_trans_vector_field_snap                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_snap)
      call set_b_trans_scalar_field_snap                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_snap)
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'nvector_rtp_2_rj ', trns_snap%nvector_rtp_2_rj
        write(*,*) 'nscalar_rtp_2_rj ', trns_snap%nscalar_rtp_2_rj
        write(*,*) 'Address for forward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      icou = 0
      call set_f_trans_vector_field_snap                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_snap)
      call set_f_trans_scalar_field_snap                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_snap)
!
      end subroutine set_addresses_snapshot_trans
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_temporal_trans                           &
     &         (SPH_MHD, iphys, trns_tmp,                               &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_stmp
      use address_fwd_sph_trans_stmp
!
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_tmp
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint):: icou
!
!
      call b_trans_address_vector_stmp(trns_tmp)
      call b_trans_address_scalar_stmp(trns_tmp)
      trns_tmp%ntensor_rj_2_rtp = 0
!
      call f_trans_address_vector_stmp(trns_tmp)
      call f_trans_address_scalar_stmp(SPH_MHD%ipol, iphys, trns_tmp)
      trns_tmp%ntensor_rtp_2_rj = 0
!
      call count_num_fields_4_sph_trans(trns_tmp, ncomp_sph_trans,      &
     &   nvector_sph_trans, nscalar_sph_trans)
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &             'for intermediate of snapshot'
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_tmp%nvector_rj_2_rtp
        write(*,*) 'nscalar_rj_2_rtp ', trns_tmp%nscalar_rj_2_rtp
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      icou = 0
      call set_b_trans_vector_field_stmp                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_tmp)
      call set_b_trans_scalar_field_stmp                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_tmp)
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'nvector_rtp_2_rj ', trns_tmp%nvector_rtp_2_rj
        write(*,*) 'nscalar_rtp_2_rj ', trns_tmp%nscalar_rtp_2_rj
        write(*,*) 'Address for forward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      icou = 0
      call set_f_trans_vector_field_stmp                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_tmp)
      call set_f_trans_scalar_field_stmp                                &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_tmp)
!
      end subroutine set_addresses_temporal_trans
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_snap
