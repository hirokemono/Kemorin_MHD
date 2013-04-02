!
!      module clear_phys_data_type
!
!        programmed by H.Matsui
!      Modified by H. Matsui on Aug, 2007
!
!> @brief clear field data to zero
!
!      subroutine clear_nodal_data_type(numdir, i_res)
!      subroutine clear_elemental_data_type(numdir, i_res)
!
      module clear_phys_data_type
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine clear_nodal_data_type(numdir, i_res, node, phys_nod)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: i_res, numdir
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: phys_nod
!
!
!$omp parallel
      call delete_phys_data_smp(np_smp, node%numnod,                    &
     &    node%istack_nod_smp, phys_nod%ntot_phys,                      &
     &    numdir, i_res, phys_nod%d_fld)
!$omp end parallel
!
      end subroutine clear_nodal_data_type
!
! ----------------------------------------------------------------------
!
      subroutine clear_elemental_data_type(numdir, i_res, ele, phys_ele)
!
      use m_machine_parameter
      use t_geometry_data
      use t_phys_data
!
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: i_res, numdir
      type(element_data), intent(in) :: ele
      type(phys_data), intent(inout) :: phys_ele
!
!
!$omp parallel
      call delete_phys_data_smp(np_smp, ele%numele, ele%istack_ele_smp, &
     &    phys_ele%ntot_phys, numdir, i_res, phys_ele%d_fld)
!$omp end parallel
!
      end subroutine clear_elemental_data_type
!
! ----------------------------------------------------------------------
!
      end module clear_phys_data_type
