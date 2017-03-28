!
!     module m_jacobians
!
!> @brief Shape function and jacobians for 3D elements
!      Written by H. Matsui on Dec., 2003
!
      module m_jacobians
!
      use m_constants
      use m_precision
      use t_jacobians
!
      implicit  none
!
!
!>     Stracture for Jacobians for FEM grid
      type(jacobians_type), save :: jacobians1
!
      end module m_jacobians
