!>@file   m_SPH_transforms.f90
!!@brief  module m_SPH_transforms
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for spherical transform utilities
!!
!!@verbatim
!!@endverbatim
!
      module m_SPH_transforms
!
      use m_precision
      use t_ucd_data
      use t_next_node_ele_4_node
!
      implicit none
!
!>        Instance for FEM field data IO
      type(ucd_data), save :: ucd_SPH_TRNS
!>        Instance for numbers of FEM mesh for merged IO
      type(merged_ucd_data), save :: m_ucd_SPH_TRNS
!
!>   Structure of included element list for each node
      type(element_around_node), save :: ele_4_nod_SPH_TRANS
!
      end module m_SPH_transforms
