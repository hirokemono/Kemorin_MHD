!>@file   filter_coef_IO.f90
!!@brief  module filter_coef_IO
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2006
!!@date Modified in Apr., 2008
!!@date Modified in Nov., 2008
!
!> @brief Data IO routine for ASCII filter data
!!
!!@verbatim
!!      subroutine read_3d_filter_stack(id_file, IO_filters)
!!      subroutine read_3d_filter_weights_coef(id_file, IO_filters)
!!      subroutine read_3d_filter_stack_b(id_file, IO_filters)
!!      subroutine read_3d_filter_weights_coef_b(id_file, IO_filters)
!!        type(filter_coefficients_type), intent(inout) :: IO_filters
!!
!!      subroutine write_3d_filter_stack(id_file, IO_filters)
!!      subroutine write_3d_filter_weights_coef(id_file, IO_filters)
!!      subroutine write_3d_filter_stack_b(id_file, IO_filters)
!!      subroutine write_3d_filter_weights_coef_b(id_file, IO_filters)
!!        type(filter_coefficients_type), intent(in) :: IO_filters
!!@endverbatim
!
      module filter_coef_IO
!
      use m_precision
!
      use t_filter_coefficients
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_3d_filter_stack(id_file, IO_filters)
!
      use skip_comment_f
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
      integer(kind = kint) :: i, j, ist, ied
      character(len=255) :: character_4_read
!
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) IO_filters%ngrp_node
!
      call alloc_num_filtering_comb(ione, IO_filters)
!
      read(id_file,*) IO_filters%istack_node(1:IO_filters%ngrp_node)
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
        read(id_file,*) IO_filters%group_name(i)
        do j = ist, ied
          read(id_file,*) IO_filters%inod_filter(j),                    &
     &                    IO_filters%istack_near_nod(j)
        end do
      end do
!
      call s_cal_numbers_from_stack(IO_filters%ntot_nod,                &
     &    IO_filters%nnod_near, IO_filters%istack_near_nod)
      IO_filters%ntot_near_nod                                          &
     &       = IO_filters%istack_near_nod(IO_filters%ntot_nod)
!
      end subroutine read_3d_filter_stack
!
!  ---------------------------------------------------------------------
!
      subroutine read_3d_filter_weights_coef(id_file, IO_filters)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
      integer(kind = kint) :: j, itmp
      character(len=255) :: character_4_read
!
!
      call alloc_3d_filter_comb(IO_filters)
      call alloc_3d_filter_func(IO_filters)
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) itmp
!
      do j = 1, IO_filters%ntot_near_nod
        read(id_file,*) itmp, IO_filters%inod_near(j),                  &
     &                  IO_filters%func(j), IO_filters%weight(j)
      end do
!
      end subroutine read_3d_filter_weights_coef
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_stack(id_file, IO_filters)
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(in) :: IO_filters
!
      integer(kind = kint) :: i, j, ist, ied
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! nodes for filtering'
      write(id_file,'(a)') '!'
!
      write(id_file,'(i12)') IO_filters%ngrp_node
      write(id_file,'(10i12)')                                          &
     &              IO_filters%istack_node(1:IO_filters%ngrp_node)
!
      do i = 1, IO_filters%ngrp_node
        ist = IO_filters%istack_node(i-1)+1
        ied = IO_filters%istack_node(i)
        write(id_file,'(a)') trim(IO_filters%group_name(i))
        do j = ist, ied
          write(id_file,'(3i12)') IO_filters%inod_filter(j),            &
     &                            IO_filters%istack_near_nod(j)
        end do
      end do
!
      end subroutine write_3d_filter_stack
!
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_weights_coef(id_file, IO_filters)
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(in) :: IO_filters
!
      integer(kind = kint) :: j
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!   filter coefficients'
      write(id_file,'(a)') '!'
      write(id_file,'(i12)') IO_filters%ntot_near_nod
      do j = 1, IO_filters%ntot_near_nod
        write(id_file,'(2i12,1p2E25.15e3)') j, IO_filters%inod_near(j), &
     &     IO_filters%func(j), IO_filters%weight(j)
      end do
!
      end subroutine write_3d_filter_weights_coef
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_3d_filter_stack_b(id_file, IO_filters)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
!
      read(id_file) IO_filters%ngrp_node
!
      call alloc_num_filtering_comb(ione, IO_filters)
!
      read(id_file) IO_filters%istack_node(1:IO_filters%ngrp_node)
      read(id_file) IO_filters%group_name(1:IO_filters%ngrp_node)
!
      call s_cal_numbers_from_stack(IO_filters%ngrp_node,               &
     &    IO_filters%num_node, IO_filters%istack_node)
      IO_filters%ntot_nod                                               &
     &     = IO_filters%istack_node(IO_filters%ngrp_node)
!
      call alloc_inod_filter_comb(IO_filters)
!
      read(id_file) IO_filters%inod_filter(1:IO_filters%ntot_nod)
      read(id_file) IO_filters%istack_near_nod(1:IO_filters%ntot_nod)
!
      call s_cal_numbers_from_stack(IO_filters%ntot_nod,                &
     &    IO_filters%nnod_near, IO_filters%istack_near_nod)
      IO_filters%ntot_near_nod                                          &
     &       = IO_filters%istack_near_nod(IO_filters%ntot_nod)
!
      end subroutine read_3d_filter_stack_b
!
!  ---------------------------------------------------------------------
!
      subroutine read_3d_filter_weights_coef_b(id_file, IO_filters)
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
!
      call alloc_3d_filter_comb(IO_filters)
      call alloc_3d_filter_func(IO_filters)
!
      read(id_file) IO_filters%inod_near(1:IO_filters%ntot_near_nod)
      read(id_file) IO_filters%func(1:IO_filters%ntot_near_nod)
      read(id_file) IO_filters%weight(1:IO_filters%ntot_near_nod)
!
      end subroutine read_3d_filter_weights_coef_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_stack_b(id_file, IO_filters)
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(in) :: IO_filters
!
!
      write(id_file) IO_filters%ngrp_node
      write(id_file) IO_filters%istack_node(1:IO_filters%ngrp_node)
      write(id_file) IO_filters%group_name(1:IO_filters%ngrp_node)
!
      write(id_file) IO_filters%inod_filter(1:IO_filters%ntot_nod)
      write(id_file) IO_filters%istack_near_nod(1:IO_filters%ntot_nod)
!
      end subroutine write_3d_filter_stack_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_weights_coef_b(id_file, IO_filters)
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(in) :: IO_filters
!
!
      write(id_file) IO_filters%inod_near(1:IO_filters%ntot_near_nod)
      write(id_file) IO_filters%func(1:IO_filters%ntot_near_nod)
      write(id_file) IO_filters%weight(1:IO_filters%ntot_near_nod)
!
      end subroutine write_3d_filter_weights_coef_b
!
!  ---------------------------------------------------------------------
!
      end module filter_coef_IO
