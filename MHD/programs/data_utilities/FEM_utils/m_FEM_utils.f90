!>@file   m_FEM_utils.f90
!!@brief  module m_FEM_utils
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO for FEM utilities
!!
!!@verbatim
!!@endverbatim
!
      module m_FEM_utils
!
      use m_precision
      use t_ucd_data
      use t_jacobian_3d
!
      implicit none
!
!>        Instance for FEM field data IO
      type(ucd_data), save :: ucd_FUTIL
!>        Instance for numbers of FEM mesh for merged IO
      type(merged_ucd_data), save :: m_ucd_FUTIL
!
!>     Stracture for Jacobians for linear element
      type(jacobians_3d), save :: jac_FUTIL_l
!>     Stracture for Jacobians for quad element
      type(jacobians_3d), save :: jac_FUTIL_q
!
      end module m_FEM_utils
