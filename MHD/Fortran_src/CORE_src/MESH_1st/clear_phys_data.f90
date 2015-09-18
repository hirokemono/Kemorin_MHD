!>@file   clear_phys_data.f90
!!@brief  module clear_phys_data
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Aug, 2007
!
!> @brief Delete nodal field
!!
!!@verbatim
!!      subroutine clear_nodal_data(numdir, i_res)
!!      subroutine clear_elemental_data(numdir, i_res)
!!@endverbatim
!!
!!@n @param numdir  number of component
!!@n @param numdir  field address to clear
!
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
      use m_geometry_data
!
      use m_node_phys_data
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: i_res, numdir
!
!
!$omp parallel
      call delete_phys_data_smp                                         &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, numdir, i_res, d_nod)
!$omp end parallel
!
      end subroutine clear_nodal_data
!
! ----------------------------------------------------------------------
!
      subroutine clear_elemental_data(numdir, i_res)
!
      use m_machine_parameter
      use m_geometry_data
!
      use m_element_phys_data
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: i_res, numdir
!
!
!$omp parallel
      call delete_phys_data_smp                                         &
     &   (np_smp, ele1%numele, ele1%istack_ele_smp,                     &
     &    fld_ele1%ntot_phys, numdir, i_res, fld_ele1%d_fld)
!$omp end parallel
!
      end subroutine clear_elemental_data
!
! ----------------------------------------------------------------------
!
      end module clear_phys_data
