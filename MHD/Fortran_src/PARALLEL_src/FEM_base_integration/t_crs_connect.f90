!t_crs_connect.f90
!      module t_crs_connect
!
!        programmed by H.Matsui on Dec., 2008
!
!> @brief Structures for index table for compressed raw strage matrix
!
!>         (module m_2d_rs_connect)
!
!      subroutine s_set_crs_connection(node, neib_nod, tbl_crs)
!        type(node_data), intent(in) :: node
!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!
!      subroutine alloc_crs_stack(numnod, tbl_crs)
!        integer(kind = kint), intent(in) :: numnod
!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!      subroutine alloc_crs_connect(tbl_crs)
!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!
!      subroutine alloc_type_2d_rs_stack(numnod, tbl_2drs)
!      subroutine alloc_type_2d_rs_connect(tbl_2drs)
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
        integer(kind = kint), allocatable :: nitem_l(:)
        integer(kind = kint), allocatable :: nitem_u(:)
!
        integer(kind = kint), allocatable :: istack_l(:)
        integer(kind = kint), allocatable :: istack_u(:)
!
        integer(kind = kint), allocatable :: item_l(:)
        integer(kind = kint), allocatable :: item_u(:)
!
        integer(kind = kint) :: max_l, min_l
        integer(kind = kint) :: max_u, min_u
      end type CRS_matrix_connect
!
!
!>  Structures for index table for two demensinal raw strage matrix
      type RS2d_matrix_connect
        integer(kind = kint) :: ntot_2d
        integer(kind = kint) :: max_2l
        integer(kind = kint) :: max_2u
!
        integer(kind = kint), allocatable :: nitem_2l(:)
        integer(kind = kint), allocatable :: nitem_2u(:)
!
        integer(kind = kint), allocatable :: item_2l(:,:)
        integer(kind = kint), allocatable :: item_2u(:,:)
      end type RS2d_matrix_connect
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_crs_connection(node, neib_nod, tbl_crs)
!
      use m_machine_parameter
      use t_geometry_data
      use t_next_node_ele_4_node
!
      use set_crs_connection
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
!
!
      call alloc_crs_stack(node%numnod, tbl_crs)
!
      call count_item_crs(node%numnod, np_smp, node%istack_nod_smp,     &
     &    neib_nod%ntot, neib_nod%istack_next, neib_nod%inod_next,      &
     &    tbl_crs%nitem_l, tbl_crs%nitem_u)
!
      call s_cal_minmax_and_stacks(node%numnod, tbl_crs%nitem_l,        &
     &    izero, tbl_crs%istack_l, tbl_crs%ntot_l,                      &
     &    tbl_crs%max_l, tbl_crs%min_l)
      call s_cal_minmax_and_stacks(node%numnod, tbl_crs%nitem_u,        &
     &    izero, tbl_crs%istack_u, tbl_crs%ntot_u,                      &
     &    tbl_crs%max_u, tbl_crs%min_u)
!
      call alloc_crs_connect(tbl_crs)
!
      call set_item_crs(node%numnod, np_smp, node%istack_nod_smp,       &
     &    neib_nod%ntot, neib_nod%istack_next,                          &
     &    neib_nod%inod_next, tbl_crs%ntot_l, tbl_crs%ntot_u,           &
     &    tbl_crs%istack_l, tbl_crs%istack_u,                           &
     &    tbl_crs%item_l, tbl_crs%item_u)
!
      end subroutine s_set_crs_connection
!
!-----------------------------------------------------------------------
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
      tbl_2drs%ntot_2d = numnod
      allocate( tbl_2drs%nitem_2l(tbl_2drs%ntot_2d) )
      allocate( tbl_2drs%nitem_2u(tbl_2drs%ntot_2d) )
!
      if (tbl_2drs%ntot_2d .gt. 0) then
        tbl_2drs%nitem_2l = 0
        tbl_2drs%nitem_2u = 0
      end if
!
      tbl_2drs%max_2l = 0
      tbl_2drs%max_2u = 0
!
      end subroutine alloc_type_2d_rs_stack
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_2d_rs_connect(tbl_2drs)
!
      type(RS2d_matrix_connect), intent(inout) :: tbl_2drs
!
!
      allocate(tbl_2drs%item_2l(tbl_2drs%max_2l,tbl_2drs%ntot_2d))
      allocate(tbl_2drs%item_2u(tbl_2drs%max_2u,tbl_2drs%ntot_2d))
!
      if(tbl_2drs%max_2l .gt. 0) tbl_2drs%item_2l = 0
      if(tbl_2drs%max_2u .gt. 0) tbl_2drs%item_2u = 0
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
      deallocate( tbl_2drs%item_2l, tbl_2drs%item_2u )
      deallocate( tbl_2drs%nitem_2l,  tbl_2drs%nitem_2u )
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
          write(50+my_rank,*) 'item_2l',                                &
     &            i, tbl_2drs%nitem_2l(i)
          write(50+my_rank,'(10i16)')                                   &
     &            tbl_2drs%item_2l(1:tbl_2drs%nitem_2l(i),i)
        end do
        do i = 1, numnod
          write(50+my_rank,*) 'item_2u',                                &
     &            i, tbl_2drs%nitem_2u(i)
          write(50+my_rank,'(10i16)')                                   &
     &            tbl_2drs%item_2u(1:tbl_2drs%nitem_2u(i),i)
        end do
!
       end subroutine check_2d_rs_connect_type
!
!-----------------------------------------------------------------------
!
      end module t_crs_connect
