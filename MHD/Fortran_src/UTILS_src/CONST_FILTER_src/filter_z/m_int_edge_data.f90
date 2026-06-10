!>@file   m_int_edge_data.f90
!!        module m_int_edge_data
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief mass matrix for vertical filter construction
!!
!!@verbatim
!!      subroutine allocate_int_edge_data(numnod, numele)
!!      subroutine deallocate_int_edge_data
!!@endverbatim
!!
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_int_edge_data(numnod, numele)
!
      integer(kind = kint), intent(in) :: numnod, numele
!
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
