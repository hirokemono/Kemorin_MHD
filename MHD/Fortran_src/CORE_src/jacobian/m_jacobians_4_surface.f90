!
!   module   m_jacobians_4_surface
!.......................................................................
!
!
!      subroutine allocate_jacobians_surf_linear
!      subroutine allocate_jacobians_surf_quad
!      subroutine allocate_jacobians_surf_l_quad
!      subroutine copy_jacobians_surface_quad
!
!      subroutine deallocate_jac_surf_linear
!      subroutine deallocate_jac_surf_quad
!      subroutine deallocate_jac_surf_l_quad
!
      module   m_jacobians_4_surface
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint) :: ntot_int_2d
      real (kind=kreal), allocatable :: an_surf(:,:)
! 
      real (kind=kreal), allocatable :: xsf_surf(:,:,:)
!
      real (kind=kreal), allocatable :: xj_surf(:,:)
      real (kind=kreal), allocatable :: axj_surf(:,:)
!
!
!
      real (kind=kreal), allocatable :: aw_surf(:,:)
! 
      real (kind=kreal), allocatable :: xsq_surf(:,:,:)
      real (kind=kreal), allocatable :: xjq_surf(:,:)
      real (kind=kreal), allocatable :: axjq_surf(:,:)
!
!
      real (kind=kreal), allocatable :: am_surf(:,:)
! 
      real (kind=kreal), allocatable :: xslq_surf(:,:,:)
      real (kind=kreal), allocatable :: xjlq_surf(:,:)
      real (kind=kreal), allocatable :: axjlq_surf(:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_jacobians_surf_linear
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_fem_gauss_int_coefs
!
!
      allocate(an_surf(num_linear_sf,ntot_int_2d))
!
      allocate(xsf_surf(numsurf,ntot_int_2d,3))
!
      allocate(xj_surf(numsurf,ntot_int_2d))
      allocate(axj_surf(numsurf,ntot_int_2d))
!
       an_surf = 0.0d0
!
       xj_surf = 0.0d0
       axj_surf = 0.0d0
!
       end subroutine allocate_jacobians_surf_linear
!
!  ------------------------------------------------------------------
!
      subroutine allocate_jacobians_surf_quad
!
       use m_geometry_parameter
       use m_fem_gauss_int_coefs
!
      allocate(aw_surf(nnod_4_surf,ntot_int_2d))
!
      allocate(xsq_surf(numsurf,ntot_int_2d,3))
!
      allocate(xjq_surf(numsurf,ntot_int_2d))
      allocate(axjq_surf(numsurf,ntot_int_2d)) 
!
       aw_surf = 0.0d0
       xsq_surf = 0.0d0
       xjq_surf = 0.0d0
       axjq_surf = 0.0d0 
!
       end subroutine allocate_jacobians_surf_quad
!
!  ------------------------------------------------------------------
!
      subroutine allocate_jacobians_surf_l_quad
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_fem_gauss_int_coefs
!
      allocate(am_surf(num_quad_sf,ntot_int_2d))
!
      allocate(xslq_surf(numsurf,ntot_int_2d,3))
!
      allocate(xjlq_surf(numsurf,ntot_int_2d))
      allocate(axjlq_surf(numsurf,ntot_int_2d)) 
!
       am_surf = 0.0d0
       xslq_surf = 0.0d0
       xjlq_surf = 0.0d0
       axjlq_surf = 0.0d0
!
       end subroutine allocate_jacobians_surf_l_quad
!
!  ------------------------------------------------------------------
!
      subroutine copy_jacobians_surface_quad
!
       aw_surf   = an_surf
       xsq_surf  = xsf_surf
       xjq_surf  = xj_surf
       axjq_surf = axj_surf
!
       end subroutine copy_jacobians_surface_quad
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
      subroutine deallocate_jac_surf_linear
!
!
      deallocate(an_surf)
      deallocate(xsf_surf)
!
      deallocate(xj_surf)
      deallocate(axj_surf)
!
       end subroutine deallocate_jac_surf_linear
!
!  ------------------------------------------------------------------
!
      subroutine deallocate_jac_surf_quad
!
      deallocate(aw_surf)
      deallocate(xsq_surf)
!
      deallocate(xjq_surf)
      deallocate(axjq_surf)
!
       end subroutine deallocate_jac_surf_quad
!
!  ------------------------------------------------------------------
!
      subroutine deallocate_jac_surf_l_quad
!
      deallocate(am_surf)
      deallocate(xslq_surf)
!
      deallocate(xjlq_surf)
      deallocate(axjlq_surf)
!
       end subroutine deallocate_jac_surf_l_quad
!
!  ------------------------------------------------------------------
!
      end module   m_jacobians_4_surface
