!m_2nd_surface_geometry_data.f90
!      module m_2nd_surface_geometry_data
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine allocate_2nd_surf_geom
!      subroutine deallocate_2nd_surf_geom
!
!      subroutine allocate_2nd_norm_vects
!      subroutine allocate_2nd_norm_vect_sph
!      subroutine allocate_2nd_norm_vect_cyl
!
!      subroutine deallocate_2nd_norm_vects
!      subroutine deallocate_2nd_norm_vect_sph
!      subroutine deallocate_2nd_norm_vect_cyl
!
      module m_2nd_surface_geometry_data
!
      use m_precision
!
      implicit  none
!
      real(kind=kreal)  , allocatable  :: x_surf_2nd(:,:)
!   position of centre of surface
      real(kind=kreal)  , allocatable  :: r_surf_2nd(:)
!   distance from the centre of surface
      real(kind=kreal)  , allocatable  :: ar_surf_2nd(:)
!   1/r_ele
      real(kind=kreal)  , allocatable  :: phi_surf_2nd(:)
!   longitude of node
      real(kind=kreal)  , allocatable  :: theta_surf_2nd(:)
!   colatitude of node
      real(kind=kreal)  , allocatable  :: s_surf_2nd(:)
!   cylindorical radius of node
      real(kind=kreal)  , allocatable  :: as_surf_2nd(:)
!   1 / s_ele
!
      real (kind=kreal), allocatable :: area_surf_2nd(:)
      real (kind=kreal), allocatable :: a_area_surf_2nd(:)
!   area of each surface
!
      real (kind=kreal), allocatable :: vnorm_surf_2nd(:,:)
!
      real (kind=kreal), allocatable :: vnorm_surf_2nd_sph(:,:)
      real (kind=kreal), allocatable :: vnorm_surf_2nd_cyl(:,:)
!
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_2nd_surf_geom
!
      use m_2nd_geometry_param
!
      allocate( x_surf_2nd(nsurf_2nd,3) )
!
      allocate( r_surf_2nd(nsurf_2nd) )
      allocate( ar_surf_2nd(nsurf_2nd) )
      allocate( phi_surf_2nd(nsurf_2nd) )
      allocate( theta_surf_2nd(nsurf_2nd) )
!
      allocate( s_surf_2nd(nsurf_2nd) )
      allocate( as_surf_2nd(nsurf_2nd) )
!
      x_surf_2nd =      0.0d0
!
      r_surf_2nd =      0.0d0
      ar_surf_2nd =     0.0d0
      phi_surf_2nd =    0.0d0
      theta_surf_2nd =  0.0d0
!
      s_surf_2nd =      0.0d0
      as_surf_2nd =     0.0d0
!
      end subroutine allocate_2nd_surf_geom
!
! ------------------------------------------------------
!
      subroutine deallocate_2nd_surf_geom
!
      deallocate( x_surf_2nd )
!
      deallocate( r_surf_2nd, phi_surf_2nd, theta_surf_2nd )
      deallocate( ar_surf_2nd )
      deallocate( s_surf_2nd, as_surf_2nd )
!
      end subroutine deallocate_2nd_surf_geom
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine allocate_2nd_norm_vects
!
      use m_2nd_geometry_param
!
      allocate( area_surf_2nd(nsurf_2nd) )
      allocate( a_area_surf_2nd(nsurf_2nd) )
      allocate( vnorm_surf_2nd(nsurf_2nd,3) )
!
      area_surf_2nd =   0.0d0
      a_area_surf_2nd = 0.0d0
      vnorm_surf_2nd =  0.0d0
!
      end subroutine allocate_2nd_norm_vects
!
! ------------------------------------------------------
!
      subroutine allocate_2nd_norm_vect_sph
!
      use m_2nd_geometry_param
!
      allocate( vnorm_surf_2nd_sph(nsurf_2nd,3) )
      vnorm_surf_2nd_sph =  0.0d0
!
      end subroutine allocate_2nd_norm_vect_sph
!
! ------------------------------------------------------
!
      subroutine allocate_2nd_norm_vect_cyl
!
      use m_2nd_geometry_param
!
      allocate( vnorm_surf_2nd_cyl(nsurf_2nd,3) )
      vnorm_surf_2nd_cyl =  0.0d0
!
      end subroutine allocate_2nd_norm_vect_cyl
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_2nd_norm_vects
!
      deallocate( area_surf_2nd )
      deallocate( a_area_surf_2nd )
      deallocate( vnorm_surf_2nd )
!
      end subroutine deallocate_2nd_norm_vects
!
! ------------------------------------------------------
!
      subroutine deallocate_2nd_norm_vect_sph
!
      deallocate( vnorm_surf_2nd_sph )
!
      end subroutine deallocate_2nd_norm_vect_sph
!
! ------------------------------------------------------
!
      subroutine deallocate_2nd_norm_vect_cyl
!
      deallocate( vnorm_surf_2nd_cyl )
!
      end subroutine deallocate_2nd_norm_vect_cyl
!
! ------------------------------------------------------
!
      end module m_2nd_surface_geometry_data
