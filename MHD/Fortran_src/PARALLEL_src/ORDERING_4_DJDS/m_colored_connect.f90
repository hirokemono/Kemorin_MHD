!
!      module m_colored_connect
!
!        programmed by H.Matsui on Oct., 2006
!
!      subroutine allocate_mc_stack(numnod)
!      subroutine allocate_mc_connect
!
!      subroutine deallocate_mc_connect
!
!       subroutine check_mc_connect(my_rank, numnod)
!
      module m_colored_connect
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: ntot_mc_l, ntot_mc_u
!
      integer(kind = kint), allocatable :: num_mc_l(:)
      integer(kind = kint), allocatable :: num_mc_u(:)
!
      integer(kind = kint), allocatable :: istack_mc_l(:)
      integer(kind = kint), allocatable :: istack_mc_u(:)
!
      integer(kind = kint), allocatable :: item_mc_l(:)
      integer(kind = kint), allocatable :: item_mc_u(:)
!
      integer(kind = kint) :: max_mc_l, min_mc_l
      integer(kind = kint) :: max_mc_u, min_mc_u
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_mc_stack(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate( num_mc_l(numnod) )
      allocate( num_mc_u(numnod) )
      allocate( istack_mc_l(0:numnod) )
      allocate( istack_mc_u(0:numnod) )
!
      num_mc_l = 0
      num_mc_u = 0
      istack_mc_l = 0
      istack_mc_u = 0
!
      max_mc_l = 0
      max_mc_u = 0
      min_mc_l = 0
      min_mc_u = 0
!
      end subroutine allocate_mc_stack
!
!-----------------------------------------------------------------------
!
      subroutine allocate_mc_connect
!
      allocate( item_mc_l(ntot_mc_l) )
      allocate( item_mc_u(ntot_mc_u) )
!
      item_mc_l = 0
      item_mc_u = 0
!
      end subroutine allocate_mc_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_mc_connect
!
      deallocate( item_mc_l )
      deallocate( item_mc_u )
!
      deallocate( num_mc_l )
      deallocate( num_mc_u )
      deallocate( istack_mc_l )
      deallocate( istack_mc_u )
!
      end subroutine deallocate_mc_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine check_mc_connect(my_rank, numnod)
!
       integer (kind = kint) :: my_rank, numnod
       integer (kind = kint) :: i, j
!
!
        do i = 1, numnod
          write(50+my_rank,*) 'item_mc_l',                              &
     &                 i, istack_mc_l(i-1), istack_mc_l(i)
          write(50+my_rank,'(10i16)')                                   &
     &        (item_mc_l(j), j=istack_mc_l(i-1)+1,istack_mc_l(i))
        end do
        do i = 1, numnod
          write(50+my_rank,*) 'item_mc_u',                              &
     &                 i, istack_mc_u(i-1), istack_mc_u(i)
          write(50+my_rank,'(10i16)')                                   &
     &        (item_mc_u(j), j=istack_mc_u(i-1)+1,istack_mc_u(i))
        end do
!
       end subroutine check_mc_connect
!
!-----------------------------------------------------------------------
!
      end module m_colored_connect
