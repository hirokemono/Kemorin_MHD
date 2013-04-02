!
!     module m_node_quad_2_linear_sf
!.......................................................................
!
!      written by H. Matsui on Jan., 2007
!
!       subroutine allocate_quad_2_linear_sf
!       subroutine allocate_lag_2_linear_sf
!
!
      module m_node_quad_2_linear_sf
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint), allocatable :: node_quad_2_linear_sf(:,:)
!   position at each node on a surface
!
!
      integer(kind = kint), parameter  :: nod_quad8_2_5surfs(20)        &
     &       = (/5, 6, 7, 8,    1, 5, 8, 0,    2, 6, 5, 0,              &
     &                          3, 7, 6, 0,    4, 8, 7, 0/)
!
      integer(kind = kint), parameter  :: nod_quad9_2_4quads(16)        &
     &       = (/1, 5, 9, 8,    5, 2, 6, 9,                             &
     &           9, 6, 3, 7,    8, 9, 7, 4/)
!
      private :: nod_quad8_2_5surfs, nod_quad9_2_4quads
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_quad_2_linear_sf
!
       integer (kind = kint) ::  i, j, k
!
!
       allocate ( node_quad_2_linear_sf(4,5) )
       node_quad_2_linear_sf = 0
!
       do j = 1, 5
         do i = 1, 4
           k = i + (j-1)*4
           node_quad_2_linear_sf(i,j) = nod_quad8_2_5surfs(k)
         end do
       end do
!
       end subroutine allocate_quad_2_linear_sf
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_lag_2_linear_sf
!
       integer (kind = kint) ::  i, j, k
!
!
       allocate ( node_quad_2_linear_sf(4,4) )
       node_quad_2_linear_sf = 0
!
       do j = 1, 4
         do i = 1, 4
           k = i + (j-1)*4
           node_quad_2_linear_sf(i,j) = nod_quad9_2_4quads(k)
         end do
       end do
!
       end subroutine allocate_lag_2_linear_sf
!
!  ---------------------------------------------------------------------
!
      end module m_node_quad_2_linear_sf
