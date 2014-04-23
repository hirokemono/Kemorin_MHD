!
!      module gz_filter_coef_IO
!
!     Written by H. Matsui on Nov., 2006
!     Modified by H. Matsui on Apr., 2008
!     Modified by H. Matsui on Nov., 2008
!
!      subroutine read_3d_filter_stack_gz
!      subroutine read_3d_filter_weights_coef_gz
!
!      subroutine write_3d_filter_stack_gz
!      subroutine write_3d_filter_weights_coef_gz
!
      module gz_filter_coef_IO
!
      use m_precision
!
      use m_combained_filter_IO
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
      subroutine read_3d_filter_stack_gz
!
      use cal_minmax_and_stacks
!
      integer(kind = kint) :: i, j, ist, ied, nchara
!
!
      call skip_gz_comment_int(ngrp_nod_filter_IO)
!
      call allocate_num_filtering_IO
!
      call read_gz_multi_int(ngrp_nod_filter_IO,                        &
     &    istack_nod_filter_IO(1))
!
      call s_cal_numbers_from_stack(ngrp_nod_filter_IO,                 &
     &    num_nod_filter_IO, istack_nod_filter_IO)
      ntot_nod_filter_IO = istack_nod_filter_IO(ngrp_nod_filter_IO)
!
      call allocate_inod_filter_comb_IO
!
      do i = 1, ngrp_nod_filter_IO
        ist = istack_nod_filter_IO(i-1)+1
        ied = istack_nod_filter_IO(i)
!
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) grp_name_filter_IO(i)
!
        do j = ist, ied
          call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
          read(textbuf,*) inod_filter_IO(j),                            &
     &                    istack_near_nod_filter_IO(j)
        end do
      end do
!
      call s_cal_numbers_from_stack(ntot_nod_filter_IO,                 &
     &    num_near_nod_filter_IO, istack_near_nod_filter_IO)
      ntot_near_nod_filter_IO                                           &
     &       = istack_near_nod_filter_IO(ntot_nod_filter_IO)
!
      end subroutine read_3d_filter_stack_gz
!
!  ---------------------------------------------------------------------
!
      subroutine read_3d_filter_weights_coef_gz
!
      integer(kind = kint) :: j, itmp, nchara
!
!
      call allocate_3d_filter_data_IO
!
      call skip_gz_comment_int(itmp)
!
      do j = 1, ntot_near_nod_filter_IO
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) itmp, inod_near_nod_IO(j), filter_func_IO(j),   &
     &                  filter_weight_IO(j)
      end do
!
      end subroutine read_3d_filter_weights_coef_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_stack_gz
!
      integer(kind = kint) :: i, j, ist, ied
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! nodes for filtering', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(i12,a1)') ngrp_nod_filter_IO, char(0)
      call write_compress_txt(nbuf, textbuf)
      call write_gz_multi_int_10i12(ngrp_nod_filter_IO,                 &
     &     istack_nod_filter_IO(1) )
!
      do i = 1, ngrp_nod_filter_IO
        ist = istack_nod_filter_IO(i-1)+1
        ied = istack_nod_filter_IO(i)
        write(textbuf,'(a,a1)') trim(grp_name_filter_IO(i)), char(0)
        call write_compress_txt(nbuf, textbuf)
        do j = ist, ied
          write(textbuf,'(3i12,a1)') inod_filter_IO(j),                 &
     &                         istack_near_nod_filter_IO(j), char(0)
          call write_compress_txt(nbuf, textbuf)
        end do
      end do
!
      end subroutine write_3d_filter_stack_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_weights_coef_gz
!
      integer(kind = kint) :: j
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!   filter coefficients', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(i12)') ntot_near_nod_filter_IO
      call write_compress_txt(nbuf, textbuf)
!
      do j = 1, ntot_near_nod_filter_IO
        write(textbuf,'(2i12,1p2E25.15e3,a1)') j, inod_near_nod_IO(j),  &
     &     filter_func_IO(j), filter_weight_IO(j), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine write_3d_filter_weights_coef_gz
!
!  ---------------------------------------------------------------------
!
      end module gz_filter_coef_IO
