!
!   module   m_jacobians_4_surface
!.......................................................................
!
!
!      subroutine cal_jacobian_surface
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
      subroutine cal_jacobian_surface
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
!
      use cal_jacobians_linear
      use cal_jacobians_quad
      use cal_jacobians_lag
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_surface_linear'
      call alloc_2d_jac_type                                            &
     &   (numsurf, num_linear_sf, maxtot_int_2d, jac1_2d_l)
      call cal_jacobian_surface_linear(jac1_2d_l)
!
      if (first_ele_type .eq. 332) then
        if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_surface_quad'
        call alloc_2d_jac_type                                          &
     &     (numsurf, nnod_4_surf, maxtot_int_2d, jac1_2d_q)
        call cal_jacobian_surface_quad(jac1_2d_q)
      else if (first_ele_type .eq. 333) then
        if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_surface_lag'
        call alloc_2d_jac_type                                          &
     &     (numsurf, nnod_4_surf, maxtot_int_2d, jac1_2d_q)
        call cal_jacobian_surface_lag(jac1_2d_q)
      else
        if (iflag_debug.eq.1) write(*,*) 'copy_jacobians_2d'
        call copy_jacobians_2d                                          &
     &     (numsurf, nnod_4_surf, jac1_2d_l, jac1_2d_q)
      end if
!
      end subroutine cal_jacobian_surface
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_jacobians_surf_l_quad(n_int)
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call alloc_2d_jac_type(numsurf, num_quad_sf, n_int, jac1_2d_ql)
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
