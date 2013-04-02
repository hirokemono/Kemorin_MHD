!
!      module m_drawpg_fem
!
!      Written by H. Matsui on March, 2009
!
!      subroutine allocate_pg_nodes
!      subroutine allocate_pg_connect
!      subroutine allocate_pg_data
!
!      subroutine deallocate_pg_grid
!      subroutine deallocate_pg_data
!      subroutine deallocate_pg_ele_data
!
      module m_drawpg_fem
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: nnod_pg
      real(kind = kreal), allocatable :: xg(:,:)
!
      integer(kind = kint) :: nele_pg, nnod_ele_pg
      integer(kind = kint), allocatable :: ie_pg(:,:)
!
      real(kind = kreal), allocatable :: vect_pg(:,:)
      real(kind = kreal), allocatable :: cont_pg(:)
!
      real(kind = kreal) :: xmax_pg, xmin_pg, maxlen
!
!      real(kind = kreal), allocatable :: rgb(:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_pg_nodes
!
!
      allocate( xg(3,nnod_pg) )
      xg = 0.0d0
!
      end subroutine allocate_pg_nodes
!
! ----------------------------------------------------------------------
!
      subroutine allocate_pg_connect
!
!
      allocate( ie_pg(nnod_ele_pg,nele_pg) )
      ie_pg = 0
!
      end subroutine allocate_pg_connect
!
! ----------------------------------------------------------------------
!
      subroutine allocate_pg_data
!
!
!      allocate( rgb(3,nnod_pg) )
!
      allocate( vect_pg(2, nnod_pg) )
      allocate( cont_pg(nnod_pg) )
!
!      rgb =    0.0d0
!
      vect_pg = 0.0d0
      cont_pg = 0.0d0
!
      end subroutine allocate_pg_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_pg_grid
!
      deallocate( xg )
      deallocate( ie_pg )
!
      end subroutine deallocate_pg_grid
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_pg_data
!
!      deallocate( rgb )
      deallocate( vect_pg, cont_pg )
!
      end subroutine deallocate_pg_data
!
! ----------------------------------------------------------------------
!
      end module m_drawpg_fem
