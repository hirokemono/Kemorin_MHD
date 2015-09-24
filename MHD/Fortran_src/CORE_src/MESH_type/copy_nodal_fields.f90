!>@file   copy_nodal_fields.f90
!!@brief  module copy_nodal_fields
!!
!!@author H. Matsui
!!@date Programmed in ??
!
!>@brief Copy fields in structure
!!
!!@verbatim
!!      subroutine clear_nodal_data(numdir,     i_target)
!!      subroutine clear_elemental_data(numdir, i_target)
!!
!!      subroutine copy_scalar_component(node, fld, i_org, i_target)
!!      subroutine copy_vector_component(node, fld, i_org, i_target)
!!      subroutine copy_tensor_component(node, fld, i_org, i_target)
!!        integer (kind = kint), intent(in) :: i_target, i_org
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(inout) :: fld
!!
!!      subroutine add_2_nod_scalars(node, fld, i_v1, i_v2, i_r)
!!      subroutine add_2_nod_vectors(node, fld, i_v1, i_v2, i_r)
!!      subroutine add_2_nod_tensors(node, fld, i_v1, i_v2, i_r)
!!         d_nod(inod,i_r) =  d_nod(inod,i_v1) + d_nod(inod,i_v2)
!!        i_r: result field ID
!!        i_v1, i_v2: source field IDs
!!
!!      subroutine subtract_2_nod_scalars(node, fld, i_v1, i_v2, i_r)
!!      subroutine subtract_2_nod_vectors(node, fld, i_v1, i_v2, i_r)
!!      subroutine subtract_2_nod_tensors(node, fld, i_v1, i_v2, i_r)
!!         d_nod(inod,i_r) =  d_nod(inod,i_v1) - d_nod(inod,i_v2)
!!        i_r: result field ID
!!        i_v1, i_v2: source field IDs
!!@endverbatim
!
      module copy_nodal_fields
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine clear_nodal_data(node, fld, numdir, i_target)
!
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: i_target, numdir
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call delete_phys_data_smp                                         &
     &   (np_smp, node%numnod, node%istack_nod_smp,                     &
     &    fld%ntot_phys, numdir, i_target, fld%d_fld)
!$omp end parallel
!
      end subroutine clear_nodal_data
!
! ----------------------------------------------------------------------
!
      subroutine clear_elemental_data(ele, fld_ele, numdir, i_target)
!
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: i_target, numdir
      type(element_data), intent(in) :: ele
      type(phys_data), intent(inout) :: fld_ele
!
!
!$omp parallel
      call delete_phys_data_smp                                         &
     &   (np_smp, ele%numele, ele%istack_ele_smp,                       &
     &    fld_ele%ntot_phys, numdir, i_target, fld_ele%d_fld)
!$omp end parallel
!
      end subroutine clear_elemental_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_scalar_component(node, fld, i_org, i_target)
!
      use copy_of_fields_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call copy_scalar_fld(np_smp, node%numnod, node%istack_nod_smp,    &
     &    fld%ntot_phys, i_org, i_target, fld%d_fld )
!$omp end parallel
!
      end subroutine copy_scalar_component
!
! ----------------------------------------------------------------------
!
      subroutine copy_vector_component(node, fld, i_org, i_target)
!
      use copy_of_fields_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call copy_vector_fld(np_smp, node%numnod, node%istack_nod_smp,    &
     &    fld%ntot_phys, i_org, i_target, fld%d_fld )
!$omp end parallel
!
      end subroutine copy_vector_component
!
! ----------------------------------------------------------------------
!
      subroutine copy_tensor_component(node, fld, i_org, i_target)
!
      use copy_of_fields_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call copy_tensor_fld(np_smp, node%numnod, node%istack_nod_smp,    &
     &    fld%ntot_phys, i_org, i_target, fld%d_fld )
!$omp end parallel
!
      end subroutine copy_tensor_component
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_2_nod_scalars(node, fld, i_v1, i_v2, i_r)
!
      use copy_of_fields_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call add_scalar_array_smp(np_smp,                                 &
     &    node%numnod, node%istack_nod_smp, fld%ntot_phys,              &
     &    fld%d_fld, i_v1, i_v2, i_r)
!$omp end parallel
!
       end subroutine add_2_nod_scalars
!
!-----------------------------------------------------------------------
!
      subroutine add_2_nod_vectors(node, fld, i_v1, i_v2, i_r)
!
      use copy_of_fields_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call add_vector_array_smp(np_smp,                                 &
     &    node%numnod, node%istack_nod_smp, fld%ntot_phys, fld%d_fld,   &
     &    i_v1, i_v2, i_r)
!$omp end parallel
!
       end subroutine add_2_nod_vectors
!
!-----------------------------------------------------------------------
!
      subroutine add_2_nod_tensors(node, fld, i_v1, i_v2, i_r)
!
      use copy_of_fields_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call add_tensor_array_smp(np_smp,                                 &
     &    node%numnod, node%istack_nod_smp, fld%ntot_phys, fld%d_fld,   &
     &    i_v1, i_v2, i_r)
!$omp end parallel
!
      end subroutine add_2_nod_tensors
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine subtract_2_nod_scalars(node, fld, i_v1, i_v2, i_r)
!
      use copy_of_fields_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call subtract_scalar_array_smp(np_smp,                            &
     &    node%numnod, node%istack_nod_smp, fld%ntot_phys,              &
     &    fld%d_fld, i_v1, i_v2, i_r)
!$omp end parallel
!
       end subroutine subtract_2_nod_scalars
!
!-----------------------------------------------------------------------
!
      subroutine subtract_2_nod_vectors(node, fld, i_v1, i_v2, i_r)
!
      use copy_of_fields_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call subtract_vector_array_smp(np_smp,                            &
     &    node%numnod, node%istack_nod_smp, fld%ntot_phys, fld%d_fld,   &
     &    i_v1, i_v2, i_r)
!$omp end parallel
!
       end subroutine subtract_2_nod_vectors
!
!-----------------------------------------------------------------------
!
      subroutine subtract_2_nod_tensors(node, fld, i_v1, i_v2, i_r)
!
      use copy_of_fields_smp
!
      integer(kind = kint), intent(in) :: i_r, i_v1, i_v2
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: fld
!
!
!$omp parallel
      call subtract_tensor_array_smp(np_smp,                            &
     &    node%numnod, node%istack_nod_smp, fld%ntot_phys, fld%d_fld,   &
     &    i_v1, i_v2, i_r)
!$omp end parallel
!
      end subroutine subtract_2_nod_tensors
!
!-----------------------------------------------------------------------
!
      end module copy_nodal_fields
