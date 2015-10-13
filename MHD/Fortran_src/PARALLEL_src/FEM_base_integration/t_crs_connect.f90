!t_crs_connect.f90
!      module t_crs_connect
!
!        programmed by H.Matsui on Dec., 2008
!
!> @brief Structures for index table for compressed raw strage matrix
!
!>         (module m_crs_connect)
!>         (module m_2d_rs_connect)
!
!      subroutine alloc_crs_stack(numnod, tbl_crs)
!        integer(kind = kint), intent(in) :: numnod
!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!      subroutine alloc_crs_connect(tbl_crs)
!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!
!      subroutine alloc_type_2d_rs_stack(numnod, tbl_2drs)
!      subroutine alloc_type_2d_rs_connect(numnod, tbl_2drs)
!        integer(kind = kint), intent(in) :: numnod
!        type(RS2d_matrix_connect), intent(inout) :: tbl_2drs
!
!      subroutine dealloc_crs_connect(tbl_crs)
!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!      subroutine dealloc_type_2d_rs_connect(tbl_2drs)
!        type(RS2d_matrix_connect), intent(inout) :: tbl_2drs
!
!      subroutine check_crs_connect(my_rank, numnod, tbl_crs)
!         integer (kind = kint), intent(in) :: my_rank, numnod
!         type(CRS_matrix_connect), intent(in) :: tbl_crs
!      subroutine check_2d_rs_connect_type(my_rank, numnod, tbl_2drs)
!         integer (kind = kint), intent(in) :: my_rank, numnod
!         type(RS2d_matrix_connect), intent(in) :: tbl_2drs
!
      module t_crs_connect
!
      use m_precision
!
      implicit none
!
!>  Structures for index table for compressed raw strage matrix
      type CRS_matrix_connect
        integer(kind = kint) :: ntot_d
        integer(kind = kint) :: ntot_l
        integer(kind = kint) :: ntot_u
!
        integer(kind = kint), pointer :: nitem_l(:)
        integer(kind = kint), pointer :: nitem_u(:)
!
        integer(kind = kint), pointer :: istack_l(:)
        integer(kind = kint), pointer :: istack_u(:)
!
        integer(kind = kint), pointer :: item_l(:)
        integer(kind = kint), pointer :: item_u(:)
!
        integer(kind = kint) :: max_l, min_l
        integer(kind = kint) :: max_u, min_u
      end type CRS_matrix_connect
!
!
!>  Structures for index table for two demensinal raw strage matrix
      type RS2d_matrix_connect
        integer(kind = kint) :: max_2d_rs_l, max_2d_rs_u
!
        integer(kind = kint), pointer :: num_2d_rs_l(:)
        integer(kind = kint), pointer :: num_2d_rs_u(:)
!
        integer(kind = kint), pointer :: item_2d_rs_l(:,:)
        integer(kind = kint), pointer :: item_2d_rs_u(:,:)
      end type RS2d_matrix_connect
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_crs_stack(numnod, tbl_crs)
!
      integer(kind = kint), intent(in) :: numnod
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
!
!
      tbl_crs%ntot_d = numnod
      allocate( tbl_crs%nitem_l(numnod) )
      allocate( tbl_crs%nitem_u(numnod) )
      allocate( tbl_crs%istack_l(0:numnod) )
      allocate( tbl_crs%istack_u(0:numnod) )
!
      if (numnod .gt. 0) then
        tbl_crs%nitem_l =  0
        tbl_crs%nitem_u =  0
        tbl_crs%istack_l = 0
        tbl_crs%istack_u = 0
      end if
!
      tbl_crs%max_l = 0
      tbl_crs%max_u = 0
      tbl_crs%min_l = 0
      tbl_crs%min_u = 0
!
      end subroutine alloc_crs_stack
!
!-----------------------------------------------------------------------
!
      subroutine alloc_crs_connect(tbl_crs)
!
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
!
!
      allocate( tbl_crs%item_l(tbl_crs%ntot_l) )
      allocate( tbl_crs%item_u(tbl_crs%ntot_u) )
!
      if(tbl_crs%ntot_l .gt. 0) tbl_crs%item_l = 0
      if(tbl_crs%ntot_u .gt. 0) tbl_crs%item_u = 0
!
      end subroutine alloc_crs_connect
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_2d_rs_stack(numnod, tbl_2drs)
!
      integer(kind = kint), intent(in) :: numnod
      type(RS2d_matrix_connect), intent(inout) :: tbl_2drs
!
!
      allocate( tbl_2drs%num_2d_rs_l(numnod) )
      allocate( tbl_2drs%num_2d_rs_u(numnod) )
!
      if (numnod .gt. 0) then
        tbl_2drs%num_2d_rs_l = 0
        tbl_2drs%num_2d_rs_u = 0
      end if
!
      tbl_2drs%max_2d_rs_l = 0
      tbl_2drs%max_2d_rs_u = 0
!
      end subroutine alloc_type_2d_rs_stack
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_2d_rs_connect(numnod, tbl_2drs)
!
      integer(kind = kint), intent(in) :: numnod
      type(RS2d_matrix_connect), intent(inout) :: tbl_2drs
!
!
      allocate( tbl_2drs%item_2d_rs_l(tbl_2drs%max_2d_rs_l,numnod) )
      allocate( tbl_2drs%item_2d_rs_u(tbl_2drs%max_2d_rs_u,numnod) )
!
      if(tbl_2drs%max_2d_rs_l .gt. 0) tbl_2drs%item_2d_rs_l = 0
      if(tbl_2drs%max_2d_rs_u .gt. 0) tbl_2drs%item_2d_rs_u = 0
!
      end subroutine alloc_type_2d_rs_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_crs_connect(tbl_crs)
!
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
!
!
      deallocate( tbl_crs%item_l,   tbl_crs%item_u)
!
      deallocate( tbl_crs%nitem_l,  tbl_crs%nitem_u)
      deallocate( tbl_crs%istack_l, tbl_crs%istack_u)
!
      end subroutine dealloc_crs_connect
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_type_2d_rs_connect(tbl_2drs)
!
      type(RS2d_matrix_connect), intent(inout) :: tbl_2drs
!
!
      deallocate( tbl_2drs%item_2d_rs_l, tbl_2drs%item_2d_rs_u )
      deallocate( tbl_2drs%num_2d_rs_l,  tbl_2drs%num_2d_rs_u )
!
      end subroutine dealloc_type_2d_rs_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine check_crs_connect(my_rank, numnod, tbl_crs)
!
       integer (kind = kint), intent(in) :: my_rank, numnod
       type(CRS_matrix_connect), intent(in) :: tbl_crs
!
       integer (kind = kint) :: i, ist, ied
!
!
        do i = 1, numnod
          ist = tbl_crs%istack_l(i-1)+1
          ied = tbl_crs%istack_l(i)
          write(50+my_rank,*) 'item_l', i, ist, ied
          write(50+my_rank,'(10i16)') tbl_crs%item_l(ist:ied)
        end do
        do i = 1, numnod
          ist = tbl_crs%istack_u(i-1)+1
          ied = tbl_crs%istack_u(i)
          write(50+my_rank,*) 'item_u', i, ist, ied
          write(50+my_rank,'(10i16)') tbl_crs%item_u(ist:ied)
        end do
!
       end subroutine check_crs_connect
!
!-----------------------------------------------------------------------
!
       subroutine check_2d_rs_connect_type(my_rank, numnod, tbl_2drs)
!
       integer (kind = kint), intent(in) :: my_rank, numnod
       type(RS2d_matrix_connect), intent(in) :: tbl_2drs
!
       integer (kind = kint) :: i
!
!
        do i = 1, numnod
          write(50+my_rank,*) 'item_2d_rs_l',                           &
     &            i, tbl_2drs%num_2d_rs_l(i)
          write(50+my_rank,'(10i16)')                                   &
     &            tbl_2drs%item_2d_rs_l(1:tbl_2drs%num_2d_rs_l(i),i)
        end do
        do i = 1, numnod
          write(50+my_rank,*) 'item_2d_rs_u',                           &
     &            i, tbl_2drs%num_2d_rs_u(i)
          write(50+my_rank,'(10i16)')                                   &
     &            tbl_2drs%item_2d_rs_u(1:tbl_2drs%num_2d_rs_u(i),i)
        end do
!
       end subroutine check_2d_rs_connect_type
!
!-----------------------------------------------------------------------
!
      end module t_crs_connect
