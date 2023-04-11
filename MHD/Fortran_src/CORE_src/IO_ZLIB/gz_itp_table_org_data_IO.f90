!>@file  gz_itp_table_org_data_IO.f90
!!       module gz_itp_table_org_data_IO
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped data IO for interpolation
!!
!!@verbatim
!!      subroutine write_gz_itp_table_org                               &
!!     &         (FPz_f, id_rank, IO_itp_org, zbuf)
!!      subroutine write_gz_itp_coefs_org(FPz_f, IO_itp_org, zbuf)
!!      subroutine write_gz_itp_idx_org(FPz_f, IO_itp_org, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(interpolate_table_org), intent(in) :: IO_itp_org
!!
!!      subroutine read_gz_itp_domain_org                               &
!!     &         (FPz_f, n_rank, IO_itp_org, zbuf)
!!      subroutine read_gz_itp_table_org(FPz_f, IO_itp_org, zbuf)
!!      subroutine read_gz_itp_coefs_org(FPz_f, IO_itp_org, zbuf)
!!      subroutine read_gz_itp_idx_org(FPz_f, IO_itp_org, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!@endverbatim
!
      module gz_itp_table_org_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_interpolation_data_labels
      use t_buffer_4_gzip
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_table_org                                 &
     &         (FPz_f, id_rank, IO_itp_org, zbuf)
!
      use t_interpolate_tbl_org
      use gzip_file_access
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) = hd_itp_export_pe() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      write(zbuf%fixbuf(1),'(i16,2a1)') id_rank, char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      write(zbuf%fixbuf(1),'(i16,2a1)') IO_itp_org%num_dest_domain,     &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call write_gz_multi_int_10i8(FPz_f, IO_itp_org%num_dest_domain, &
     &      IO_itp_org%id_dest_domain, zbuf)
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end if
!
!
      zbuf%fixbuf(1) = hd_itp_export_item() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call write_gz_multi_int_8i16(FPz_f, IO_itp_org%num_dest_domain, &
     &     IO_itp_org%istack_nod_tbl_org(1:IO_itp_org%num_dest_domain), &
     &     zbuf)
        call write_gz_multi_int_8i16(FPz_f, IO_itp_org%ntot_table_org,  &
     &                               IO_itp_org%inod_itp_send, zbuf)
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end if
!
      end subroutine write_gz_itp_table_org
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_coefs_org(FPz_f, IO_itp_org, zbuf)
!
      use t_interpolate_tbl_org
      use gzip_file_access
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      type(interpolate_table_org), intent(in) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: inod
!
!
      zbuf%fixbuf(1) = hd_itp_export_coef() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call write_gz_multi_int_8i16(FPz_f, ifour,                      &
     &      IO_itp_org%istack_itp_type_org(1:ifour), zbuf)
!
        do inod = 1, IO_itp_org%ntot_table_org
          write(zbuf%fixbuf(1),'(3i16,1p3E25.15e3,2a1)')                &
     &        IO_itp_org%inod_gl_dest_4_org(inod),                      &
     &        IO_itp_org%iele_org_4_org(inod),                          &
     &        IO_itp_org%itype_inter_org(inod),                         &
     &        IO_itp_org%coef_inter_org(inod,1:3), char(10), char(0)
          call gz_write_textbuf_no_lf(FPz_f, zbuf)
        end do
!
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end if
!
      end subroutine write_gz_itp_coefs_org
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_idx_org(FPz_f, IO_itp_org, zbuf)
!
      use t_interpolate_tbl_org
      use gzip_file_access
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      type(interpolate_table_org), intent(in) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: inod
!
!
      zbuf%fixbuf(1) = hd_itp_export_coef() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call write_gz_multi_int_8i16(FPz_f, ifour,                      &
     &      IO_itp_org%istack_itp_type_org(1:ifour), zbuf)
!
        do inod = 1, IO_itp_org%ntot_table_org
          write(zbuf%fixbuf(1),'(2i16,2a1)')                            &
     &        IO_itp_org%inod_gl_dest_4_org(inod),                      &
     &        IO_itp_org%iele_org_4_org(inod), char(10), char(0)
          call gz_write_textbuf_no_lf(FPz_f, zbuf)
        end do
!
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end if
!
      end subroutine write_gz_itp_idx_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_domain_org                                 &
     &         (FPz_f, n_rank, IO_itp_org, zbuf)
!
      use t_interpolate_tbl_org
      use gz_data_IO
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call skip_gz_comment_int(FPz_f, n_rank, zbuf)
      call skip_gz_comment_int(FPz_f, IO_itp_org%num_dest_domain, zbuf)
!
      call alloc_itp_num_org(np_smp, IO_itp_org)
      if (IO_itp_org%num_dest_domain .gt. 0) then
!
        call read_gz_multi_int(FPz_f, IO_itp_org%num_dest_domain,       &
     &      IO_itp_org%id_dest_domain, zbuf)
      end if
!
      end subroutine read_gz_itp_domain_org
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_table_org(FPz_f, IO_itp_org, zbuf)
!
      use t_interpolate_tbl_org
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
        IO_itp_org%istack_nod_tbl_org(0) = 0
        call read_gz_multi_int(FPz_f, IO_itp_org%num_dest_domain,       &
     &     IO_itp_org%istack_nod_tbl_org(1:IO_itp_org%num_dest_domain), &
     &     zbuf)
        IO_itp_org%ntot_table_org                                       &
     &      = IO_itp_org%istack_nod_tbl_org(IO_itp_org%num_dest_domain)
!
        call alloc_itp_table_org(IO_itp_org)
        call read_gz_multi_int(FPz_f, IO_itp_org%ntot_table_org,        &
     &                         IO_itp_org%inod_itp_send, zbuf)
!
      end subroutine read_gz_itp_table_org
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_coefs_org(FPz_f, IO_itp_org, zbuf)
!
      use t_interpolate_tbl_org
      use gzip_file_access
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: inod
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
!
        IO_itp_org%istack_itp_type_org(0) = 0
        call read_gz_multi_int(FPz_f, ifour,                            &
     &      IO_itp_org%istack_itp_type_org(1:ifour), zbuf)
!
        do inod = 1, IO_itp_org%ntot_table_org
          call get_one_line_text_from_gz(FPz_f, zbuf)
          read(zbuf%fixbuf(1),*) IO_itp_org%inod_gl_dest_4_org(inod),   &
     &        IO_itp_org%iele_org_4_org(inod),                          &
     &        IO_itp_org%itype_inter_org(inod),                         &
     &        IO_itp_org%coef_inter_org(inod,1:3)
        end do
!
      end subroutine read_gz_itp_coefs_org
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_idx_org(FPz_f, IO_itp_org, zbuf)
!
      use t_interpolate_tbl_org
      use gzip_file_access
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: inod
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
!
        IO_itp_org%istack_itp_type_org(0) = 0
        call read_gz_multi_int(FPz_f, ifour,                            &
     &      IO_itp_org%istack_itp_type_org(1:ifour), zbuf)
!
        do inod = 1, IO_itp_org%ntot_table_org
          call get_one_line_text_from_gz(FPz_f, zbuf)
          read(zbuf%fixbuf(1),*) IO_itp_org%inod_gl_dest_4_org(inod),   &
     &        IO_itp_org%iele_org_4_org(inod)
        end do
!
      end subroutine read_gz_itp_idx_org
!
!-----------------------------------------------------------------------
!
      end module gz_itp_table_org_data_IO
