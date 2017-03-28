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
!jacobians1%jac_3d
!
!>     Stracture for linear Jacobians for quad element
      type(jacobians_3d), save :: jac1_3d_lq
!
!
!>     Stracture of linear Jacobians for surafce group
      type(jacobians_2d), save :: jac1_sf_grp_2d_l
!>     Stracture of quadrature Jacobians for surafce group
      type(jacobians_2d), save :: jac1_sf_grp_2d_q
!>     Stracture of quadrature Jacobians for linear surafce group
      type(jacobians_2d), save :: jac1_sf_grp_2d_ql
!
      end module m_jacobians
