!> @file  t_colored_connect.f90
!!      module t_colored_connect
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2006
!! @n    Modified in Nov., 2016
!
!> @brief Work area for multicoloring
!!
!!@verbatim
!!      subroutine alloc_mc_stack(numnod, WK_MC)
!!      subroutine alloc_mc_connect(WK_MC)
!!
!!      subroutine dealloc_mc_connect(WK_MC)
!!
!!      subroutine check_mc_connect(my_rank, WK_MC)
!!
!!      subroutine copy_ntot_from_crs_mat(tbl_crs, WK_MC)
!!@endverbatim
!
      module t_colored_connect
!
      use m_precision
!
      implicit none
!
!>      Work structure for multicoloring
      type work_4_RCM
        integer(kind = kint) :: NP
!
        integer(kind = kint) :: ntot_mc_l
        integer(kind = kint) :: ntot_mc_u
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
        integer(kind = kint) :: min_mc_l
        integer(kind = kint) :: max_mc_l
!
        integer(kind = kint) :: min_mc_u
        integer(kind = kint) :: max_mc_u
      end type work_4_RCM
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_mc_stack(numnod, WK_MC)
!
      integer(kind = kint), intent(in) :: numnod
      type(work_4_RCM), intent(inout) :: WK_MC
!
!
      WK_MC%NP = numnod
!
      allocate( WK_MC%num_mc_l(WK_MC%NP) )
      allocate( WK_MC%num_mc_u(WK_MC%NP) )
      allocate( WK_MC%istack_mc_l(0:WK_MC%NP) )
      allocate( WK_MC%istack_mc_u(0:WK_MC%NP) )
!
      if(WK_MC%NP .gt. 0) WK_MC%num_mc_l = 0
      if(WK_MC%NP .gt. 0) WK_MC%num_mc_u = 0
      WK_MC%istack_mc_l = 0
      WK_MC%istack_mc_u = 0
!
      WK_MC%max_mc_l = 0
      WK_MC%max_mc_u = 0
      WK_MC%min_mc_l = 0
      WK_MC%min_mc_u = 0
!
      end subroutine alloc_mc_stack
!
!-----------------------------------------------------------------------
!
      subroutine alloc_mc_connect(WK_MC)
!
      type(work_4_RCM), intent(inout) :: WK_MC
!
!
      allocate( WK_MC%item_mc_l(WK_MC%ntot_mc_l) )
      allocate( WK_MC%item_mc_u(WK_MC%ntot_mc_u) )
!
      if(WK_MC%ntot_mc_l .gt. 0) WK_MC%item_mc_l = 0
      if(WK_MC%ntot_mc_u .gt. 0) WK_MC%item_mc_u = 0
!
      end subroutine alloc_mc_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_mc_connect(WK_MC)
!
      type(work_4_RCM), intent(inout) :: WK_MC
!
!
      deallocate( WK_MC%item_mc_l )
      deallocate( WK_MC%item_mc_u )
!
      deallocate( WK_MC%num_mc_l )
      deallocate( WK_MC%num_mc_u )
      deallocate( WK_MC%istack_mc_l )
      deallocate( WK_MC%istack_mc_u )
!
      end subroutine dealloc_mc_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine check_mc_connect(my_rank, WK_MC)
!
       integer (kind = kint), intent(in) :: my_rank
      type(work_4_RCM), intent(in) :: WK_MC
!
       integer (kind = kint) :: i
!
!
        do i = 1, WK_MC%NP
          write(50+my_rank,*) 'item_mc_l',                              &
     &                i, WK_MC%istack_mc_l(i-1), WK_MC%istack_mc_l(i)
          write(50+my_rank,'(10i16)')                                   &
     &      WK_MC%item_mc_l(WK_MC%istack_mc_l(i-1)+1                    &
     &                     :WK_MC%istack_mc_l(i))
        end do
        do i = 1, WK_MC%NP
          write(50+my_rank,*) 'item_mc_u',                              &
     &                i, WK_MC%istack_mc_u(i-1), WK_MC%istack_mc_u(i)
          write(50+my_rank,'(10i16)')                                   &
     &      WK_MC%item_mc_u(WK_MC%istack_mc_u(i-1)+1                    &
     &                     :WK_MC%istack_mc_u(i))
        end do
!
       end subroutine check_mc_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_ntot_from_crs_mat(tbl_crs, WK_MC)
!
      use t_crs_connect
!
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(work_4_RCM), intent(inout) :: WK_MC
!
!
      WK_MC%ntot_mc_l = tbl_crs%ntot_l
      WK_MC%ntot_mc_u = tbl_crs%ntot_u
      WK_MC%max_mc_l =  tbl_crs%max_l
      WK_MC%min_mc_l =  tbl_crs%min_l
      WK_MC%max_mc_u =  tbl_crs%max_u
      WK_MC%min_mc_u =  tbl_crs%min_u
!
      end subroutine copy_ntot_from_crs_mat
!
!-----------------------------------------------------------------------
!
      end module t_colored_connect
