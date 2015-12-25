!>@file   m_visualization.f90
!!@brief  module m_visualization
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for visualizers
!!
!!@verbatim
!!@endverbatim
!
      module m_visualization
!
      use m_precision
      use t_ucd_data
      use t_next_node_ele_4_node
      use t_jacobian_3d
!
      implicit none
!
!>        Instance for FEM field data IO
     type(ucd_data), save :: ucd_VIZ
!>        Instance for numbers of FEM mesh for merged IO
!      type(merged_ucd_data), save :: m_ucd_SPH_TRNS
!
!>   Structure of included element list for each node
      type(element_around_node), save :: ele_4_nod_VIZ
!
!>     Stracture for Jacobians for linear element
      type(jacobians_3d), save :: jac_VIZ_l
!>     Stracture for Jacobians for quad element
      type(jacobians_3d), save :: jac_VIZ_q
!
      end module m_visualization
