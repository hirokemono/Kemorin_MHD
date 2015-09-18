!>@file   copy_nodal_fields.f90
!!@brief  module copy_nodal_fields
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief  Copy field data (Need OMP PARALLEL)
!!
!!@verbatim
!!      subroutine copy_scalar_component(i_org, i_target)
!!      subroutine copy_vector_component(i_org, i_target)
!!      subroutine copy_tensor_components(i_org, i_target)
!!
!!       subroutine add_2_nod_scalars(i_v1, i_v2, i_r)
!!       subroutine add_2_nod_vectors(i_v1, i_v2, i_r)
!!       subroutine add_2_nod_tensors(i_v1, i_v2, i_r)
!!
!!         d_nod(inod,i_r) =  d_nod(inod,i_v1) + d_nod(inod,i_v2)
!!        i_r: result field ID
!!        i_v1, i_v2: source field IDs
!!
!!       subroutine subtract_2_nod_scalars(i_v1, i_v2, i_r)
!!       subroutine subtract_2_nod_vectors(i_v1, i_v2, i_r)
!!       subroutine subtract_2_nod_tensors(i_v1, i_v2, i_r)
!!
!!         d_nod(inod,i_r) =  d_nod(inod,i_v1) - d_nod(inod,i_v2)
!!        i_r: result field ID
!!        i_v1, i_v2: source field IDs
!!
!!       subroutine multi_by_const_nod_scalar(const, i_v1, i_r)
!!       subroutine multi_by_const_nod_vector(const, i_v1, i_r)
!!       subroutine multi_by_const_nod_tensor(const, i_v1, i_r)
!!
!!         d_nod(inod,i_r) =  const * d_nod(inod,i_v1)
!!        i_r: result field ID
!!        i_v1: source field IDs
!!@endverbatim
!
      module copy_nodal_fields
!
      use m_precision
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_scalar_component(i_org, i_target)
!
      use copy_nodal_field_type
!
      integer (kind = kint), intent(in) :: i_target, i_org
!
!
      call copy_scalar_fld                                              &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, i_org, i_target, d_nod)
!
      end subroutine copy_scalar_component
!
! ----------------------------------------------------------------------
!
      subroutine copy_vector_component(i_org, i_target)
!
      use copy_nodal_field_type
!
      integer (kind = kint), intent(in) :: i_target, i_org
!
!
      call copy_vector_fld                                              &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, i_org, i_target, d_nod)
!
      end subroutine copy_vector_component
!
! ----------------------------------------------------------------------
!
      subroutine copy_tensor_components(i_org, i_target)
!
      use copy_nodal_field_type
!
      integer (kind = kint), intent(in) :: i_target, i_org
!
!
      call copy_tensor_fld                                              &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, i_org, i_target, d_nod)
!
      end subroutine copy_tensor_components
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine add_2_nod_scalars(i_v1, i_v2, i_r)
!
      use add_nodal_field_type
!
       integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call add_scalar_array_smp                                         &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, d_nod, i_v1, i_v2, i_r)
!
       end subroutine add_2_nod_scalars
!
!-----------------------------------------------------------------------
!
       subroutine add_2_nod_vectors(i_v1, i_v2, i_r)
!
      use add_nodal_field_type
!
       integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call add_vector_array_smp                                         &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, d_nod, i_v1, i_v2, i_r)
!
       end subroutine add_2_nod_vectors
!
!-----------------------------------------------------------------------
!
      subroutine add_2_nod_tensors(i_v1, i_v2, i_r)
!
      use add_nodal_field_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call add_tensor_array_smp                                         &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, d_nod, i_v1, i_v2, i_r)
!
      end subroutine add_2_nod_tensors
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine subtract_2_nod_scalars(i_v1, i_v2, i_r)
!
      use subtract_nodal_field_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call subtract_scalar_array_smp                                    &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, d_nod, i_v1, i_v2, i_r)
!
      end subroutine subtract_2_nod_scalars
!
!-----------------------------------------------------------------------
!
      subroutine subtract_2_nod_vectors(i_v1, i_v2, i_r)
!
      use subtract_nodal_field_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call subtract_vector_array_smp                                    &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, d_nod, i_v1, i_v2, i_r)
!
      end subroutine subtract_2_nod_vectors
!
!-----------------------------------------------------------------------
!
      subroutine subtract_2_nod_tensors(i_v1, i_v2, i_r)
!
      use subtract_nodal_field_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
!
!
      call subtract_tensor_array_smp                                    &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, d_nod, i_v1, i_v2, i_r)
!
      end subroutine subtract_2_nod_tensors
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_scalar(const, i_v1, i_r)
!
      use multi_by_const_fields_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
!
!
      call multi_by_const_nod_phys1                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, d_nod, const, i_v1, i_r)
!
      end subroutine multi_by_const_nod_scalar
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_vector(const, i_v1, i_r)
!
      use multi_by_const_fields_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
!
!
      call multi_by_const_nod_phys3                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, d_nod, const, i_v1, i_r)
!
      end subroutine multi_by_const_nod_vector
!
!-----------------------------------------------------------------------
!
      subroutine multi_by_const_nod_tensor(const, i_v1, i_r)
!
      use multi_by_const_fields_type
!
      integer(kind = kint), intent(in) :: i_r, i_v1
      real(kind = kreal), intent(in) :: const
!
!
      call multi_by_const_nod_phys6                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, d_nod, const, i_v1, i_r)
!
      end subroutine multi_by_const_nod_tensor
!
!-----------------------------------------------------------------------
!
      end module copy_nodal_fields
