!
!   module   m_jacobians_4_edge
!.......................................................................
!
!       Written by H. Matsui
!
!      subroutine allocate_jacobians_edge_linear(n_int)
!
!!      subroutine const_jacobian_edge
!!      subroutine allocate_jacobians_edge_l_quad(n_int)
!!      subroutine deallocate_jac_edge_linear
!!      subroutine deallocate_jac_edge_quad
!!      subroutine deallocate_jac_edge_l_quad
!
      module   m_jacobians_4_edge
!
      use m_precision
      use t_jacobian_1d
!
      implicit  none
!
!>     Stracture for Jacobians for edge (linear)
      type(jacobians_1d), save :: jac1_1d_l
!>     Stracture for Jacobians for edge (quad)
      type(jacobians_1d), save :: jac1_1d_q
!>     Stracture for Jacobians for edge (linear function for quad)
      type(jacobians_1d), save :: jac1_1d_ql
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for edge element
!
      subroutine const_jacobian_edge
!
      use m_machine_parameter
      use m_geometry_constants
      use m_mesh_data
!
      use const_jacobians_1d
!
!
      call cal_jacobian_edge(mesh1%node, edge1, jac1_1d_l, jac1_1d_q)
!
      end subroutine const_jacobian_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_jacobians_edge_l_quad(n_int)
!
      use m_geometry_constants
      use m_geometry_data
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call alloc_1d_jac_type(edge1%numedge, num_quad_edge, n_int,       &
     &                       jac1_1d_ql)
!
      end subroutine allocate_jacobians_edge_l_quad
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
      subroutine deallocate_jac_edge_linear
!
      call dealloc_1d_jac_type(jac1_1d_l)
!
      end subroutine deallocate_jac_edge_linear
!
!  ------------------------------------------------------------------
!
      subroutine deallocate_jac_edge_quad
!
      call dealloc_1d_jac_type(jac1_1d_q)
!
      end subroutine deallocate_jac_edge_quad
!
!  ------------------------------------------------------------------
!
      subroutine deallocate_jac_edge_l_quad
!
      call dealloc_1d_jac_type(jac1_1d_ql)
!
      end subroutine deallocate_jac_edge_l_quad
!
!  ------------------------------------------------------------------
!
      end module   m_jacobians_4_edge
