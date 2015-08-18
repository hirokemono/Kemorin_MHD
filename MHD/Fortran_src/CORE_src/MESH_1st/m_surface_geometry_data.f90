!
!      module m_surface_geometry_data
!
!> @brief surface geometry data
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine allocate_normal_vectors
!      subroutine allocate_normal_vector_sph
!      subroutine allocate_normal_vector_cyl
!
!      subroutine deallocate_normal_vectors
!      subroutine deallocate_normal_vector_sph
!      subroutine deallocate_normal_vector_cyl
!
      module m_surface_geometry_data
!
      use m_precision
!
      implicit  none
!
!      real(kind=kreal)  , allocatable  :: x_surf(:,:)
!<   position of center of surface
!      real(kind=kreal)  , allocatable  :: r_surf(:)
!<   distance from the center of surface
!      real(kind=kreal)  , allocatable  :: ar_surf(:)
!<   1/r_surf
!      real(kind=kreal)  , allocatable  :: phi_surf(:)
!<   longitude of center of surface
!      real(kind=kreal)  , allocatable  :: theta_surf(:)
!<   colatitude of center of surface
!      real(kind=kreal)  , allocatable  :: s_surf(:)
!<   cylindorical radius of center of surface
!      real(kind=kreal)  , allocatable  :: as_surf(:)
!<   1 / s_surf
!
      real (kind=kreal), allocatable :: area_surf(:)
!<       area of each surface
      real (kind=kreal), allocatable :: a_area_surf(:)
!<     1 / area_surf
!
      real (kind=kreal), allocatable :: vnorm_surf(:,:)
!<       normal vector for sach surface
!
      real (kind=kreal), allocatable :: vnorm_surf_sph(:,:)
!<       normal vector for sach surface (spherical coordinate)
      real (kind=kreal), allocatable :: vnorm_surf_cyl(:,:)
!<       normal vector for sach surface  (cylindrical coordinate)
!
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
! ------------------------------------------------------
!
      subroutine allocate_normal_vectors
!
      use m_geometry_data
!
      allocate( area_surf(surf1%numsurf) )
      allocate( a_area_surf(surf1%numsurf) )
      allocate( vnorm_surf(surf1%numsurf,3) )
!
      area_surf =   0.0d0
      a_area_surf = 0.0d0
      vnorm_surf =  0.0d0
!
      end subroutine allocate_normal_vectors
!
! ------------------------------------------------------
!
      subroutine allocate_normal_vector_sph
!
      use m_geometry_data
!
      allocate( vnorm_surf_sph(surf1%numsurf,3) )
      vnorm_surf_sph =  0.0d0
!
      end subroutine allocate_normal_vector_sph
!
! ------------------------------------------------------
!
      subroutine allocate_normal_vector_cyl
!
      use m_geometry_data
!
      allocate( vnorm_surf_cyl(surf1%numsurf,3) )
      vnorm_surf_cyl =  0.0d0
!
      end subroutine allocate_normal_vector_cyl
!
! ------------------------------------------------------
!
      subroutine deallocate_normal_vectors
!
      deallocate( area_surf )
      deallocate( a_area_surf )
      deallocate( vnorm_surf )
!
      end subroutine deallocate_normal_vectors
!
! ------------------------------------------------------
!
      subroutine deallocate_normal_vector_sph
!
      deallocate( vnorm_surf_sph )
!
      end subroutine deallocate_normal_vector_sph
!
! ------------------------------------------------------
!
      subroutine deallocate_normal_vector_cyl
!
      deallocate( vnorm_surf_cyl )
!
      end subroutine deallocate_normal_vector_cyl
!
! ------------------------------------------------------
!
      end module m_surface_geometry_data
