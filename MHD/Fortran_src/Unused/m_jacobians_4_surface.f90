!
!   module   m_jacobians_4_surface
!.......................................................................
!
!
!      subroutine const_jacobian_surface
!      subroutine allocate_jacobians_surf_l_quad(n_int)
!
!      subroutine deallocate_jac_surf_linear
!      subroutine deallocate_jac_surf_quad
!      subroutine deallocate_jac_surf_l_quad
!
      module   m_jacobians_4_surface
!
      use m_precision
      use t_jacobian_2d
!
      implicit  none
!
!>     Stracture of linear Jacobians for surafces
      type(jacobians_2d), save :: jac1_2d_l
!>     Stracture of quadrature Jacobians for surafces
      type(jacobians_2d), save :: jac1_2d_q
!>     Stracture of quadrature Jacobians for linear surafces
      type(jacobians_2d), save :: jac1_2d_ql
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!> Construct shape function, difference of shape function, and Jacobian
!> for surface element
!
      subroutine const_jacobian_surface
!
      use m_geometry_constants
      use m_mesh_data
!
      use const_jacobians_2d
!
!
      call cal_jacobian_surface                                         &
     &   (mesh1%node, surf1, jac1_2d_l, jac1_2d_q)
!
      end subroutine const_jacobian_surface
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_jacobians_surf_l_quad(n_int)
!
      use m_geometry_constants
      use m_geometry_data
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call alloc_2d_jac_type(surf1%numsurf, num_quad_sf,                &
     &    n_int, jac1_2d_ql)
!
       end subroutine allocate_jacobians_surf_l_quad
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
      subroutine deallocate_jac_surf_linear
!
!
      call dealloc_2d_jac_type(jac1_2d_l)
!
       end subroutine deallocate_jac_surf_linear
!
!  ------------------------------------------------------------------
!
      subroutine deallocate_jac_surf_quad
!
      call dealloc_2d_jac_type(jac1_2d_q)
!
       end subroutine deallocate_jac_surf_quad
!
!  ------------------------------------------------------------------
!
      subroutine deallocate_jac_surf_l_quad
!
      call dealloc_2d_jac_type(jac1_2d_ql)
!
       end subroutine deallocate_jac_surf_l_quad
!
!  ------------------------------------------------------------------
!
      end module   m_jacobians_4_surface
