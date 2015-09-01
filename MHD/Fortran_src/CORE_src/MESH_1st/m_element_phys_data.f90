!m_element_phys_data.f90
!     module m_element_phys_data
!.......................................................................
!
!     Written by H. Matsui on Nov., 2011
!
!> @brief Field data in element for FEM
!
!      subroutine allocate_ele_data_arrays(numele)
!
!      subroutine deallocate_ele_data_arrays
!
!
      module m_element_phys_data
!
      use m_precision
      use t_phys_data
!
      implicit  none
!
!
!>       Structure for field data on element
      type(phys_data), save :: fld_ele1
!
      end module m_element_phys_data
