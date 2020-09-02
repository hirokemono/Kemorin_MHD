!>@file   copy_all_fields_4_sph_trans.f90
!!@brief  module copy_all_fields_4_sph_trans
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Copy spectrum data and field data to spherical transform buffer
!!       for dynamo simulation
!!
!!@verbatim
!!      subroutine copy_all_field_from_trans(m_folding, sph_rtp,       &
!!     &          backward, mesh, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(spherical_transform_data), intent(in) :: backward
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(inout) :: nod_fld
!!
!!      subroutine copy_all_field_to_trans                              &
!!     &         (mesh, sph_rtp, nod_fld, forward)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(spherical_transform_data), intent(inout) :: forward
!!@endverbatim
!
      module copy_all_fields_4_sph_trans
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
      use copy_nodal_fld_4_sph_trans
!
      use t_sph_trans_comm_tbl
      use t_spheric_rtp_data
      use t_mesh_data
      use t_phys_address
      use t_phys_data
      use t_addresses_sph_transform
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_all_field_from_trans(m_folding, sph_rtp,       &
     &          backward, mesh, nod_fld)
!
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(spherical_transform_data), intent(in) :: backward
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: j, j_fld, jcomp, i_rtp
!
!
!      write(*,*) 'fld_rtp', backward%fld_rtp(1:sph_rtp%nnod_rtp,1:3)
!
      do j_fld = 1, backward%num_vector
        jcomp = backward%ifld_trns(j_fld)
        i_rtp = backward%ifld_rtp(j_fld)
!
        if(iflag_debug .gt. 0) write(*,*) 'copy vector from ',          &
     &                                   jcomp, 'to ', i_rtp
        call copy_nod_vec_from_trans_wpole(sph_rtp, m_folding,          &
     &      backward%ncomp, jcomp, backward%fld_rtp, backward%fld_pole, &
     &      i_rtp, mesh%node, nod_fld)
      end do
!
      do j = 1, backward%num_scalar
        j_fld = j + backward%num_vector
        jcomp = backward%ifld_trns(j_fld)
        i_rtp = backward%ifld_rtp(j_fld)
!
        if(iflag_debug .gt. 0) write(*,*) 'copy scalar from ',          &
     &                                   jcomp, 'to ', i_rtp
        call copy_nod_scl_from_trans_wpole(sph_rtp, m_folding,          &
     &      backward%ncomp, jcomp, backward%fld_rtp, backward%fld_pole, &
     &      i_rtp, mesh%node, nod_fld)
      end do
!
      do j = 1, backward%num_tensor
        j_fld = j + backward%num_vector + backward%num_scalar
        jcomp = backward%ifld_trns(j_fld)
        i_rtp = backward%ifld_rtp(j_fld)
!
        if(iflag_debug .gt. 0) write(*,*) 'copy tensor from  ',         &
     &      jcomp, 'to ', i_rtp
        call copy_nod_tsr_from_trans_wpole(sph_rtp, m_folding,          &
     &      backward%ncomp, jcomp, backward%fld_rtp, backward%fld_pole, &
     &      i_rtp, mesh%node, nod_fld)
      end do
!
      end subroutine copy_all_field_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_all_field_to_trans                                &
     &         (mesh, sph_rtp, nod_fld, forward)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      type(spherical_transform_data), intent(inout) :: forward
!
      integer(kind = kint) :: j, j_fld, jcomp, i_rtp
!
!
      do j_fld = 1, forward%num_vector
        jcomp = forward%ifld_trns(j_fld)
        i_rtp = forward%ifld_rtp(j_fld)
!
        if(iflag_debug .gt. 0) write(*,*) 'copy vector from  ',         &
     &                                   jcomp, 'to ', i_rtp
        call copy_nod_vec_to_sph_trans(mesh%node, sph_rtp,              &
     &      nod_fld, i_rtp, forward%fld_rtp(1,jcomp))
      end do
!
      do j = 1, forward%num_scalar
        j_fld = j + forward%num_vector
        jcomp = forward%ifld_trns(j_fld)
        i_rtp = forward%ifld_rtp(j_fld)
        if(iflag_debug .gt. 0) write(*,*) 'copy scalar from  ',         &
     &                        jcomp, 'to ', i_rtp
        call copy_nod_scl_to_sph_trans(mesh%node, sph_rtp,              &
     &      nod_fld, i_rtp, forward%fld_rtp(1,jcomp))
      end do
!
      do j = 1, forward%num_tensor
        j_fld = j + forward%num_vector + forward%num_scalar
        jcomp = forward%ifld_trns(j_fld)
        i_rtp = forward%ifld_rtp(j_fld)
        if(iflag_debug .gt. 0) write(*,*) 'copy tensor from  ',         &
     &                        jcomp, 'to ', i_rtp
        call copy_nod_tsr_to_sph_trans(mesh%node, sph_rtp,              &
     &      nod_fld, i_rtp, forward%fld_rtp(1,jcomp))
      end do
!
      end subroutine copy_all_field_to_trans
!
!-----------------------------------------------------------------------
!
       end module copy_all_fields_4_sph_trans
