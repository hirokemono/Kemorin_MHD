!m_element_group.f90
!     module m_element_group
!
!> @brief element group data
!
!     Written by H. Matsui
!
      module m_element_group
!
      use m_precision
      use t_group_data
!
      implicit  none
!
!
!>  Structure for element group
      type(group_data), save :: ele_grp1
!
      end module m_element_group
