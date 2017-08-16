!m_element_phys_data.f90
!     module m_element_phys_data
!.......................................................................
!
!     Written by H. Matsui on Nov., 2011
!
!> @brief Field data in element for FEM
!
      module m_element_phys_data
!
      use m_precision
      use t_phys_data
      use t_phys_address
!
      implicit  none
!
!
!>       Structure for field data on element
      type(phys_data), save :: ele_fld1
!
!>   address for element fields
      type(phys_address), save :: iphys_ele
!
      end module m_element_phys_data
