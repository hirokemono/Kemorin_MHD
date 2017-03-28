!t_jacobians.f90
!     module t_jacobians
!
!      Written by H. Matsui on Dec., 2008
!
!>   Structure of Jacobian and difference of shape functions
!
!      subroutine alloc_3d_linear_jac_type(jacobians)
!      subroutine dealloc_3d_linear_jac_type(jacobians)
!      subroutine unlink_3d_linear_jac_type(jacobians)
!        type(jacobians_type), intent(inout) :: jacobians
!
      module t_jacobians
!
      use m_precision
!
      use t_jacobian_3d
      use t_jacobian_2d
      use t_jacobian_1d
!
      implicit  none
!
!
!>     Stracture for Jacobians for FEM grid
      type jacobians_type
!>     Stracture for Jacobians for element
        type(jacobians_3d), pointer :: jac_3d
!>     Stracture for Jacobians for surface
        type(jacobians_2d), pointer :: jac_2d
!>     Stracture for Jacobians for edge
        type(jacobians_1d), pointer :: jac_1d
!>     Stracture for Jacobians for surafce group
        type(jacobians_2d), pointer :: jac_sf_grp
!
!>     Stracture for Jacobians for linear element
        type(jacobians_3d), pointer ::  jac_3d_l
!>     Stracture for Jacobians for linear surface
        type(jacobians_2d), pointer ::  jac_2d_l
!>     Stracture for Jacobians for linear edge
        type(jacobians_1d), pointer  :: jac_1d_l
!>     Stracture for Jacobians for linear surafce group
        type(jacobians_2d), pointer :: jac_sf_grp_l
!
        type(jacobians_3d), pointer ::  jac_3d_lq
!>     Stracture for quadrature Jacobians for surface
        type(jacobians_2d), pointer ::  jac_2d_lq
!>     Stracture for quadrature Jacobians for edge
        type(jacobians_1d), pointer  :: jac_1d_lq
!>     Stracture for quadrature Jacobians for surafce group
        type(jacobians_2d), pointer :: jac_sf_grp_lq
      end type jacobians_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_3d_linear_jac_type(jacobians)
!
      type(jacobians_type), intent(inout) :: jacobians
!
      allocate(jacobians%jac_3d_l)
      allocate(jacobians%jac_2d_l)
      allocate(jacobians%jac_1d_l)
      allocate(jacobians%jac_sf_grp_l)
!
      end subroutine alloc_3d_linear_jac_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_3d_linear_jac_type(jacobians)
!
      type(jacobians_type), intent(inout) :: jacobians
!
      deallocate(jacobians%jac_3d_l)
      deallocate(jacobians%jac_2d_l)
      deallocate(jacobians%jac_1d_l)
      deallocate(jacobians%jac_sf_grp_l)
!
      end subroutine dealloc_3d_linear_jac_type
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_3d_linear_jac_type(jacobians)
!
      type(jacobians_type), intent(inout) :: jacobians
!
      nullify(jacobians%jac_3d_l)
      nullify(jacobians%jac_2d_l)
      nullify(jacobians%jac_1d_l)
      nullify(jacobians%jac_sf_grp_l)
!
      end subroutine unlink_3d_linear_jac_type
!
!  ---------------------------------------------------------------------
!
      end module t_jacobians
