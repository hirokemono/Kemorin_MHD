!>@file   copy_nodal_fields.f90
!!@brief  module copy_nodal_fields
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief  Copy field data (Need OMP PARALLEL)
!!
!!@verbatim
!!      subroutine copy_scalar_component(i_target, i_org)
!!      subroutine copy_vector_component(i_target, i_org)
!!      subroutine copy_tensor_components(i_target, i_org)
!!@endverbatim
!
      module copy_nodal_fields
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_scalar_component(i_target, i_org)
!
      use m_geometry_parameter
      use m_node_phys_data
      use copy_field_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
!
!
!$omp parallel
      call copy_nod_scalar_smp(np_smp, numnod, inod_smp_stack,          &
     &    d_nod(1,i_org), d_nod(1,i_target) )
!$omp end parallel
!
      end subroutine copy_scalar_component
!
! ----------------------------------------------------------------------
!
      subroutine copy_vector_component(i_target, i_org)
!
      use m_geometry_parameter
      use m_node_phys_data
      use copy_field_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
!
!
!$omp parallel
      call copy_nod_vector_smp(np_smp, numnod, inod_smp_stack,          &
     &    d_nod(1,i_org), d_nod(1,i_target) )
!$omp end parallel
!
      end subroutine copy_vector_component
!
! ----------------------------------------------------------------------
!
      subroutine copy_tensor_components(i_target, i_org)
!
      use m_geometry_parameter
      use m_node_phys_data
      use copy_field_smp
!
      integer (kind = kint), intent(in) :: i_target, i_org
!
!
!$omp parallel
      call copy_nod_sym_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &    d_nod(1,i_org), d_nod(1,i_target) )
!$omp end parallel
!
      end subroutine copy_tensor_components
!
! ----------------------------------------------------------------------
!
      end module copy_nodal_fields
