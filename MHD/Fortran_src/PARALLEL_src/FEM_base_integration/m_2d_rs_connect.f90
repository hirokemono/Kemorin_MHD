!
!      module m_2d_rs_connect
!
!        programmed by H.Matsui on Mar., 2008
!
!      subroutine allocate_2d_rs_stack(numnod)
!      subroutine allocate_2d_rs_connect(numnod)
!      subroutine deallocate_2d_rs_connect
!
!       subroutine check_2d_rs_connect(my_rank, numnod)
!
      module m_2d_rs_connect
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: max_2d_rs_l, max_2d_rs_u
!
      integer(kind = kint), allocatable :: num_2d_rs_l(:)
      integer(kind = kint), allocatable :: num_2d_rs_u(:)
!
      integer(kind = kint), allocatable :: item_2d_rs_l(:,:)
      integer(kind = kint), allocatable :: item_2d_rs_u(:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_2d_rs_stack(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate( num_2d_rs_l(numnod) )
      allocate( num_2d_rs_u(numnod) )
      num_2d_rs_l = 0
      num_2d_rs_u = 0
!
      max_2d_rs_l = 0
      max_2d_rs_u = 0
!
      end subroutine allocate_2d_rs_stack
!
!-----------------------------------------------------------------------
!
      subroutine allocate_2d_rs_connect(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate( item_2d_rs_l(max_2d_rs_l,numnod) )
      allocate( item_2d_rs_u(max_2d_rs_u,numnod) )
!
      item_2d_rs_l = 0
      item_2d_rs_u = 0
!
      end subroutine allocate_2d_rs_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_2d_rs_connect
!
      deallocate( item_2d_rs_l )
      deallocate( item_2d_rs_u )
!
      deallocate( num_2d_rs_l )
      deallocate( num_2d_rs_u )
!
      end subroutine deallocate_2d_rs_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine check_2d_rs_connect(my_rank, numnod)
!
       integer (kind = kint), intent(in) :: my_rank, numnod
       integer (kind = kint) :: i, j
!
!
        do i = 1, numnod
          write(50+my_rank,*) 'item_2d_rs_l',                           &
     &                 i, num_2d_rs_l(i)
          write(50+my_rank,'(10i16)')                                   &
     &        (item_2d_rs_l(j,i), j=1,num_2d_rs_l(i))
        end do
        do i = 1, numnod
          write(50+my_rank,*) 'item_2d_rs_u',                           &
     &                 i, num_2d_rs_u(i)
          write(50+my_rank,'(10i16)')                                   &
     &        (item_2d_rs_u(j,i), j=1,num_2d_rs_u(i))
        end do
!
       end subroutine check_2d_rs_connect
!
!-----------------------------------------------------------------------
!
      end module m_2d_rs_connect
