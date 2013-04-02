!
!      module clear_phys_data
!
!        programmed by H.Matsui
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine clear_nodal_data(numdir, i_res)
!      subroutine clear_elemental_data(numdir, i_res)
!
      module clear_phys_data
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
      subroutine clear_nodal_data(numdir, i_res)
!
      use m_machine_parameter
      use m_geometry_parameter
!
      use m_node_phys_data
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: i_res, numdir
!
!
!$omp parallel
      call delete_phys_data_smp(np_smp, numnod, inod_smp_stack,         &
     &    num_tot_nod_phys, numdir, i_res, d_nod)
!$omp end parallel
!
      end subroutine clear_nodal_data
!
! ----------------------------------------------------------------------
!
      subroutine clear_elemental_data(numdir, i_res)
!
      use m_machine_parameter
      use m_geometry_parameter
!
      use m_element_phys_data
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: i_res, numdir
!
!
!$omp parallel
      call delete_phys_data_smp(np_smp, numele, iele_smp_stack,         &
     &    num_tot_ele_phys, numdir, i_res, d_ele)
!$omp end parallel
!
      end subroutine clear_elemental_data
!
! ----------------------------------------------------------------------
!
      end module clear_phys_data
