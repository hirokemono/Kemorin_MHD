!
!      module m_crs_connect
!
!        programmed by H.Matsui on Oct., 2006
!
!      subroutine allocate_crs_stack(numnod)
!      subroutine allocate_crs_connect
!
!      subroutine deallocate_crs_connect
!
!       subroutine check_crs_connect(my_rank, numnod)
!
!
      module m_crs_connect
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: ntot_crs_l, ntot_crs_u
!
      integer(kind = kint), allocatable :: num_crs_l(:)
      integer(kind = kint), allocatable :: num_crs_u(:)
!
      integer(kind = kint), allocatable :: istack_crs_l(:)
      integer(kind = kint), allocatable :: istack_crs_u(:)
!
      integer(kind = kint), allocatable :: item_crs_l(:)
      integer(kind = kint), allocatable :: item_crs_u(:)
!
      integer(kind = kint) :: max_crs_l, min_crs_l
      integer(kind = kint) :: max_crs_u, min_crs_u
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_crs_stack(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate( num_crs_l(numnod) )
      allocate( num_crs_u(numnod) )
      allocate( istack_crs_l(0:numnod) )
      allocate( istack_crs_u(0:numnod) )
!
      num_crs_l = 0
      num_crs_u = 0
      istack_crs_l = 0
      istack_crs_u = 0
!
      max_crs_l = 0
      max_crs_u = 0
      min_crs_l = 0
      min_crs_u = 0
!
      end subroutine allocate_crs_stack
!
!-----------------------------------------------------------------------
!
      subroutine allocate_crs_connect
!
      allocate( item_crs_l(ntot_crs_l) )
      allocate( item_crs_u(ntot_crs_u) )
!
      item_crs_l = 0
      item_crs_u = 0
!
      end subroutine allocate_crs_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_crs_connect
!
      deallocate( item_crs_l )
      deallocate( item_crs_u )
!
      deallocate( num_crs_l )
      deallocate( num_crs_u )
      deallocate( istack_crs_l )
      deallocate( istack_crs_u )
!
      end subroutine deallocate_crs_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine check_crs_connect(my_rank, numnod)
!
       integer (kind = kint), intent(in) :: my_rank, numnod
       integer (kind = kint) :: i, j
!
!
        do i = 1, numnod
          write(50+my_rank,*) 'item_crs_l',                             &
     &                 i, istack_crs_l(i-1), istack_crs_l(i)
          write(50+my_rank,'(10i8)')                                    &
     &        (item_crs_l(j), j=istack_crs_l(i-1)+1,istack_crs_l(i))
        end do
        do i = 1, numnod
          write(50+my_rank,*) 'item_crs_u',                             &
     &                 i, istack_crs_u(i-1), istack_crs_u(i)
          write(50+my_rank,'(10i8)')                                    &
     &        (item_crs_u(j), j=istack_crs_u(i-1)+1,istack_crs_u(i))
        end do
!
       end subroutine check_crs_connect
!
!-----------------------------------------------------------------------
!
      end module m_crs_connect
