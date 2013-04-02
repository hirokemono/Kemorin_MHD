!
!   module   m_jacobians_2d
!
!      subroutine allocate_jacobians_2d_linear
!      subroutine allocate_jacobians_2d_quad
!      subroutine allocate_jacobians_2d_l_quad
!
!      subroutine copy_jacobians_2d_quad
!
!      subroutine deallocate_jacobians_2d_linear
!      subroutine deallocate_jacobians_2d_quad
!      subroutine deallocate_jacobians_2d_l_quad
!
      module   m_jacobians_2d
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint) :: ntot_int_sf_grp
      real (kind=kreal), allocatable :: an_sf(:,:)
! 
      real (kind=kreal), allocatable :: xsf_sf(:,:,:)
      real (kind=kreal), allocatable :: xj_sf(:,:)
      real (kind=kreal), allocatable :: axj_sf(:,:)
!
!
      real (kind=kreal), allocatable :: aw_sf(:,:)
! 
      real (kind=kreal), allocatable :: xsq_sf(:,:,:)
      real (kind=kreal), allocatable :: xjq_sf(:,:)
      real (kind=kreal), allocatable :: axjq_sf(:,:)
!
!
      real (kind=kreal), allocatable :: am_sf(:,:)
! 
      real (kind=kreal), allocatable :: xslq_sf(:,:,:)
      real (kind=kreal), allocatable :: xjlq_sf(:,:)
      real (kind=kreal), allocatable :: axjlq_sf(:,:)
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine allocate_jacobians_2d_linear
!
       use m_geometry_constants
       use m_geometry_parameter
       use m_surface_group
       use m_fem_gauss_int_coefs
!
!
      allocate(an_sf(num_linear_sf,ntot_int_sf_grp))
!
      allocate(xsf_sf(num_surf_bc,ntot_int_sf_grp,3))
!
      allocate(xj_sf(num_surf_bc,ntot_int_sf_grp))
      allocate(axj_sf(num_surf_bc,ntot_int_sf_grp))
!
!
      an_sf = 0.0d0
!
      if(num_surf_bc .gt. 0) then
        xj_sf = 0.0d0
        axj_sf = 0.0d0
      end if
!
      end subroutine allocate_jacobians_2d_linear
!
!  ------------------------------------------------------------------
!
      subroutine allocate_jacobians_2d_quad
!
       use m_geometry_parameter
       use m_surface_group
       use m_fem_gauss_int_coefs
!
      allocate(aw_sf(nnod_4_surf,ntot_int_sf_grp))
!
      allocate(xsq_sf(num_surf_bc,ntot_int_sf_grp,3))
!
      allocate(xjq_sf(num_surf_bc,ntot_int_sf_grp))
      allocate(axjq_sf(num_surf_bc,ntot_int_sf_grp)) 
!
      aw_sf = 0.0d0
!
      if(num_surf_bc .gt. 0) then
        xsq_sf = 0.0d0
        xjq_sf = 0.0d0
        axjq_sf = 0.0d0 
      end if
!
      end subroutine allocate_jacobians_2d_quad
!
!  ------------------------------------------------------------------
!
      subroutine allocate_jacobians_2d_l_quad
!
       use m_geometry_constants
       use m_geometry_parameter
       use m_surface_group
       use m_fem_gauss_int_coefs
!
      allocate(am_sf(num_quad_sf,ntot_int_sf_grp))
!
      allocate(xslq_sf(num_surf_bc,ntot_int_sf_grp,3))
!
      allocate(xjlq_sf(num_surf_bc,ntot_int_sf_grp))
      allocate(axjlq_sf(num_surf_bc,ntot_int_sf_grp)) 
!
      am_sf = 0.0d0
!
      if(num_surf_bc .gt. 0) then
        xslq_sf = 0.0d0
        xjlq_sf = 0.0d0
        axjlq_sf = 0.0d0
      end if
!
       end subroutine allocate_jacobians_2d_l_quad
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
      subroutine copy_jacobians_2d_quad
!
       aw_sf   = an_sf
       xsq_sf  = xsf_sf
       xjq_sf  = xj_sf
       axjq_sf = axj_sf
!       dwx_sf  = dnx_sf
       end subroutine copy_jacobians_2d_quad
!
!  ------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine deallocate_jacobians_2d_linear
!
!
      deallocate(an_sf, xsf_sf)
      deallocate(xj_sf, axj_sf)
!
      end subroutine deallocate_jacobians_2d_linear
!
!  ------------------------------------------------------------------
!
      subroutine deallocate_jacobians_2d_quad
!
!
      deallocate(aw_sf, xsq_sf)
      deallocate(xjq_sf, axjq_sf)
!
      end subroutine deallocate_jacobians_2d_quad
!
!  ------------------------------------------------------------------
!
      subroutine deallocate_jacobians_2d_l_quad
!
!
      deallocate(am_sf, xslq_sf)
      deallocate(xjlq_sf, axjlq_sf)
!
      end subroutine deallocate_jacobians_2d_l_quad
!
!  ------------------------------------------------------------------
!
      end module   m_jacobians_2d
