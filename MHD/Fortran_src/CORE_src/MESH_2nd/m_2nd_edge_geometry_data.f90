!
!      module m_2nd_edge_geometry_data
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine allocate_2nd_edge_geometry
!      subroutine allocate_2nd_edge_vects
!      subroutine allocate_2nd_edge_vect_sph
!      subroutine allocate_2nd_edge_vect_cyl
!
!      subroutine deallocate_2nd_edge_geometry
!      subroutine deallocate_2nd_edge_vects
!      subroutine deallocate_2nd_edge_vect_sph
!      subroutine deallocate_2nd_edge_vect_cyl
!
      module m_2nd_edge_geometry_data
!
      use m_precision
!
      implicit  none
!
!
      real(kind=kreal)  , allocatable  :: x_edge_2nd(:,:)
!   position of centre of surface
      real(kind=kreal)  , allocatable  :: r_edge_2nd(:)
!   distance from the centre of surface
      real(kind=kreal)  , allocatable  :: ar_edge_2nd(:)
!   1/r_ele
      real(kind=kreal)  , allocatable  :: phi_edge_2nd(:)
!   longitude of node
      real(kind=kreal)  , allocatable  :: theta_edge_2nd(:)
!   colatitude of node
      real(kind=kreal)  , allocatable  :: s_edge_2nd(:)
!   cylindorical radius of node
      real(kind=kreal)  , allocatable  :: as_edge_2nd(:)
!   1 / s_ele
!
      real (kind=kreal), allocatable :: edge_length_2nd(:)
      real (kind=kreal), allocatable :: a_edge_length_2nd(:)
!   area of each surface
!
      real (kind=kreal), allocatable :: edge_vect_2nd(:,:)
!
      real (kind=kreal), allocatable :: edge_vect_2nd_sph(:,:)
!
      real (kind=kreal), allocatable :: edge_vect_2nd_cyl(:,:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_2nd_edge_geometry
!
      use m_2nd_geometry_param
!
      allocate( x_edge_2nd(nedge_2nd,3) )
!
      allocate( r_edge_2nd(nedge_2nd) )
      allocate( ar_edge_2nd(nedge_2nd) )
      allocate( phi_edge_2nd(nedge_2nd) )
      allocate( theta_edge_2nd(nedge_2nd) )
!
      allocate( s_edge_2nd(nedge_2nd) )
      allocate( as_edge_2nd(nedge_2nd) )
!
      x_edge_2nd =      0.0d0
!
      r_edge_2nd =      0.0d0
      ar_edge_2nd =     0.0d0
      phi_edge_2nd =    0.0d0
      theta_edge_2nd =  0.0d0
!
      s_edge_2nd =      0.0d0
      as_edge_2nd =     0.0d0
!
      end subroutine allocate_2nd_edge_geometry
!
! ------------------------------------------------------
!
      subroutine allocate_2nd_edge_vects
!
      use m_2nd_geometry_param
!
      allocate( edge_length_2nd(nedge_2nd) )
      allocate( a_edge_length_2nd(nedge_2nd) )
      allocate( edge_vect_2nd(nedge_2nd,3) )
!
      edge_length_2nd =   0.0d0
      a_edge_length_2nd = 0.0d0
      edge_vect_2nd =     0.0d0
!
      end subroutine allocate_2nd_edge_vects
!
! ------------------------------------------------------
!
      subroutine allocate_2nd_edge_vect_sph
!
      use m_2nd_geometry_param
!
      allocate( edge_vect_2nd_sph(nedge_2nd,3) )
      edge_vect_2nd_sph =     0.0d0
!
      end subroutine allocate_2nd_edge_vect_sph
!
! ------------------------------------------------------
!
      subroutine allocate_2nd_edge_vect_cyl
!
      use m_2nd_geometry_param
!
      allocate( edge_vect_2nd_cyl(nedge_2nd,3) )
      edge_vect_2nd_cyl =     0.0d0
!
      end subroutine allocate_2nd_edge_vect_cyl
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_2nd_edge_geometry
!
      deallocate( x_edge_2nd )
!
      deallocate( r_edge_2nd )
      deallocate( ar_edge_2nd )
      deallocate( phi_edge_2nd )
      deallocate( theta_edge_2nd )
!
      deallocate( s_edge_2nd )
      deallocate( as_edge_2nd )
!
      end subroutine deallocate_2nd_edge_geometry
!
! ------------------------------------------------------
!
      subroutine deallocate_2nd_edge_vects
!
      deallocate( edge_length_2nd )
      deallocate( a_edge_length_2nd )
      deallocate( edge_vect_2nd )
!
      end subroutine deallocate_2nd_edge_vects
!
! ------------------------------------------------------
!
      subroutine deallocate_2nd_edge_vect_sph
!
      deallocate( edge_vect_2nd_sph )
!
      end subroutine deallocate_2nd_edge_vect_sph
!
! ------------------------------------------------------
!
      subroutine deallocate_2nd_edge_vect_cyl
!
      deallocate( edge_vect_2nd_cyl )
!
      end subroutine deallocate_2nd_edge_vect_cyl
!
! ------------------------------------------------------
!
      end module m_2nd_edge_geometry_data
