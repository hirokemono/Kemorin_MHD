!
!      module gz_filter_coef_IO
!
!     Written by H. Matsui on Nov., 2006
!     Modified by H. Matsui on Apr., 2008
!     Modified by H. Matsui on Nov., 2008
!
!!      subroutine read_3d_filter_stack_gz(IO_filters)
!!      subroutine read_3d_filter_weights_coef_gz(IO_filters)
!!        type(filter_coefficients_type), intent(inout) :: IO_filters
!!
!!      subroutine write_3d_filter_stack_gz(IO_filters)
!!      subroutine write_3d_filter_weights_coef_gz(IO_filters)
!!        type(filter_coefficients_type), intent(in) :: IO_filters
!
      module gz_filter_coef_IO
!
      use m_precision
!
      use t_filter_coefficients
      use skip_gz_comment
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_3d_filter_stack_gz(IO_filters)
!
      use cal_minmax_and_stacks
!
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
      integer(kind = kint) :: i, j, ist, ied
!
!
      call skip_gz_comment_int(IO_filters%ngrp_node, zbuf1)
!
      call alloc_num_filtering_comb(ione, IO_filters)
!
      call read_gz_multi_int(IO_filters%ngrp_node,                      &
     &    IO_filters%istack_node(1:IO_filters%ngrp_node), zbuf1)
!
      call s_cal_numbers_from_stack(IO_filters%ngrp_node,               &
     &    IO_filters%num_node, IO_filters%istack_node)
      IO_filters%ntot_nod                                               &
     &     = IO_filters%istack_node(IO_filters%ngrp_node)
!
      call alloc_inod_filter_comb(IO_filters)
!
      do i = 1, IO_filters%ngrp_node
        ist = IO_filters%istack_node(i-1)+1
        ied = IO_filters%istack_node(i)
!
        call get_one_line_from_gz_f(zbuf1)
        read(zbuf1%fixbuf(1),*) IO_filters%group_name(i)
!
        do j = ist, ied
          call get_one_line_from_gz_f(zbuf1)
          read(zbuf1%fixbuf(1),*) IO_filters%inod_filter(j),            &
     &                           IO_filters%istack_near_nod(j)
        end do
      end do
!
      call s_cal_numbers_from_stack(IO_filters%ntot_nod,                &
     &    IO_filters%nnod_near, IO_filters%istack_near_nod)
      IO_filters%ntot_near_nod                                          &
     &       = IO_filters%istack_near_nod(IO_filters%ntot_nod)
!
      end subroutine read_3d_filter_stack_gz
!
!  ---------------------------------------------------------------------
!
      subroutine read_3d_filter_weights_coef_gz(IO_filters)
!
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
      integer(kind = kint) :: j, itmp
!
!
      call alloc_3d_filter_comb(IO_filters)
      call alloc_3d_filter_func(IO_filters)
!
      call skip_gz_comment_int(itmp, zbuf1)
!
      do j = 1, IO_filters%ntot_near_nod
        call get_one_line_from_gz_f(zbuf1)
        read(zbuf1%fixbuf(1),*) itmp, IO_filters%inod_near(j),          &
     &                         IO_filters%func(j), IO_filters%weight(j)
      end do
!
      end subroutine read_3d_filter_weights_coef_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_stack_gz(IO_filters)
!
      type(filter_coefficients_type), intent(in) :: IO_filters
!
      integer(kind = kint) :: i, j, ist, ied
!
!
      write(zbuf1%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(zbuf1%fixbuf(1),'(a,2a1)') '! nodes for filtering',         &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(zbuf1%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      write(zbuf1%fixbuf(1),'(i12,2a1)') IO_filters%ngrp_node,          &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      call write_gz_multi_int_10i12(IO_filters%ngrp_node,               &
     &    IO_filters%istack_node(1:IO_filters%ngrp_node), zbuf1)
!
      do i = 1, IO_filters%ngrp_node
        ist = IO_filters%istack_node(i-1)+1
        ied = IO_filters%istack_node(i)
        write(zbuf1%fixbuf(1),'(a,2a1)')                                &
     &        trim(IO_filters%group_name(i)), char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf1)
        do j = ist, ied
          write(zbuf1%fixbuf(1),'(3i12,2a1)')                           &
     &        IO_filters%inod_filter(j), IO_filters%istack_near_nod(j), &
     &        char(10), char(0)
          call gz_write_textbuf_no_lf(zbuf1)
        end do
      end do
!
      end subroutine write_3d_filter_stack_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_weights_coef_gz(IO_filters)
!
      type(filter_coefficients_type), intent(in) :: IO_filters
!
      integer(kind = kint) :: j
!
!
      write(zbuf1%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(zbuf1%fixbuf(1),'(a,2a1)') '!   filter coefficients',       &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(zbuf1%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      write(zbuf1%fixbuf(1),'(i12,2a1)') IO_filters%ntot_near_nod,      &
     &                          char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      do j = 1, IO_filters%ntot_near_nod
        write(zbuf1%fixbuf(1),'(2i12,1p2E25.15e3,2a1)')                 &
     &     j, IO_filters%inod_near(j),                                  &
     &     IO_filters%func(j), IO_filters%weight(j), char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf1)
      end do
!
      end subroutine write_3d_filter_weights_coef_gz
!
!  ---------------------------------------------------------------------
!
      end module gz_filter_coef_IO
