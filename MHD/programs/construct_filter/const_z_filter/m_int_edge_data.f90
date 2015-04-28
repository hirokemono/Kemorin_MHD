!
!      module m_int_edge_data
!
!     Written by H. Matsui
!
      module m_int_edge_data
!
      use m_precision
!
      implicit none
!
!
      real(kind=kreal), dimension(:), allocatable :: dz
      real(kind=kreal), dimension(:), allocatable :: mk
      real(kind=kreal), dimension(:,:), allocatable :: mk_c
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_int_edge_data
!
      use m_geometry_parameter
      use m_fem_gauss_int_coefs
!
      allocate( dz(numele) )
      allocate( mk(numnod) )
      allocate( mk_c(numnod,numnod) )
!
      dz = 0.0d0
      mk = 0.0d0
      mk_c = 0.0d0
!
      end subroutine allocate_int_edge_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_int_edge_data
!
      deallocate( dz )
!
      end subroutine deallocate_int_edge_data
!
! -----------------------------------------------------------------------
!
      end module m_int_edge_data
