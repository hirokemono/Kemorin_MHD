!gz_itp_table_data_IO.f90
!      module gz_itp_table_data_IO
!
!        programmed by H.Matsui on Sep. 2012
!
!!      subroutine write_gz_itp_table_org(id_rank, IO_itp_org, zbuf)
!!      subroutine write_gz_itp_coefs_org(IO_itp_org, zbuf)
!!        type(interpolate_table_org), intent(in) :: IO_itp_org
!!
!!      subroutine read_gz_itp_domain_org(n_rank, IO_itp_org, zbuf)
!!      subroutine read_gz_itp_table_org(IO_itp_org, zbuf)
!!      subroutine read_gz_itp_coefs_org(IO_itp_org, zbuf)
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!
!!      subroutine write_gz_itp_table_dest(id_rank, IO_itp_dest, zbuf)
!!      subroutine write_gz_itp_coefs_dest                              &
!!     &         (IO_itp_dest, IO_itp_c_dest, zbuf)
!!        type(interpolate_table_dest), intent(in) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine read_gz_itp_domain_dest(n_rank, IO_itp_dest, zbuf)
!!      subroutine read_gz_itp_table_dest(IO_itp_dest, zbuf)
!!      subroutine read_gz_itp_coefs_dest                               &
!!     &         (IO_itp_dest, IO_itp_c_dest, zbuf)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!
      module gz_itp_table_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
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
      subroutine write_gz_itp_table_org(id_rank, IO_itp_org, zbuf)
!
      use t_interpolate_tbl_org
      use gzip_file_access
      use gz_data_IO
!
      integer, intent(in) :: id_rank
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      write(zbuf%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  domain ID ',                  &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  number of domain to export',  &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  domain IDs to export',        &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      write(zbuf%fixbuf(1),'(i16,2a1)') id_rank, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(i16,2a1)') IO_itp_org%num_dest_domain,     &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call write_gz_multi_int_10i8(IO_itp_org%num_dest_domain,        &
     &      IO_itp_org%id_dest_domain, zbuf)
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end if
!
!
!
      write(zbuf%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  stack of node to export',     &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  exported node ID',            &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call write_gz_multi_int_8i16(IO_itp_org%num_dest_domain,        &
     &     IO_itp_org%istack_nod_tbl_org(1:IO_itp_org%num_dest_domain), &
     &     zbuf)
        call write_gz_multi_int_8i16                                    &
     &     (IO_itp_org%ntot_table_org, IO_itp_org%inod_itp_send, zbuf)
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end if
!
      end subroutine write_gz_itp_table_org
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_coefs_org(IO_itp_org, zbuf)
!
      use t_interpolate_tbl_org
      use gzip_file_access
      use gz_data_IO
!
      type(interpolate_table_org), intent(in) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: inod
!
!
      write(zbuf%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  stack by interpolation type', &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  destinate global node ID, ',  &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &       '!  local element ID for interpolation', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  interpolation type ID ',      &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  generalized position ',       &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call write_gz_multi_int_8i16                                    &
     &     (ifour, IO_itp_org%istack_itp_type_org(1:ifour), zbuf)
!
        do inod = 1, IO_itp_org%ntot_table_org
          write(zbuf%fixbuf(1),'(3i16,1p3E25.15e3,2a1)')                &
     &        IO_itp_org%inod_gl_dest_4_org(inod),                      &
     &        IO_itp_org%iele_org_4_org(inod),                          &
     &        IO_itp_org%itype_inter_org(inod),                         &
     &        IO_itp_org%coef_inter_org(inod,1:3), char(10), char(0)
          call gz_write_textbuf_no_lf(zbuf)
        end do
!
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end if
!
      end subroutine write_gz_itp_coefs_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_domain_org(n_rank, IO_itp_org, zbuf)
!
      use t_interpolate_tbl_org
      use gz_data_IO
      use skip_gz_comment
!
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call skip_gz_comment_int(n_rank, zbuf)
      call skip_gz_comment_int(IO_itp_org%num_dest_domain, zbuf)
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call alloc_itp_num_org(np_smp, IO_itp_org)
        call read_gz_multi_int(IO_itp_org%num_dest_domain,              &
     &      IO_itp_org%id_dest_domain, zbuf)
      end if
!
      end subroutine read_gz_itp_domain_org
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_table_org(IO_itp_org, zbuf)
!
      use t_interpolate_tbl_org
      use gz_data_IO
!
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
        IO_itp_org%istack_nod_tbl_org(0) = 0
        call read_gz_multi_int(IO_itp_org%num_dest_domain,              &
     &     IO_itp_org%istack_nod_tbl_org(1:IO_itp_org%num_dest_domain), &
     &     zbuf)
        IO_itp_org%ntot_table_org                                       &
     &      = IO_itp_org%istack_nod_tbl_org(IO_itp_org%num_dest_domain)
!
        call alloc_itp_table_org(IO_itp_org)
        call read_gz_multi_int                                          &
     &     (IO_itp_org%ntot_table_org, IO_itp_org%inod_itp_send, zbuf)
!
      end subroutine read_gz_itp_table_org
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_coefs_org(IO_itp_org, zbuf)
!
      use t_interpolate_tbl_org
      use gzip_file_access
      use gz_data_IO
!
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: inod
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
!
        IO_itp_org%istack_itp_type_org(0) = 0
        call read_gz_multi_int                                          &
     &     (ifour, IO_itp_org%istack_itp_type_org(1:ifour), zbuf)
!
        do inod = 1, IO_itp_org%ntot_table_org
          call get_one_line_text_from_gz(zbuf)
          read(zbuf%fixbuf(1),*) IO_itp_org%inod_gl_dest_4_org(inod),   &
     &        IO_itp_org%iele_org_4_org(inod),                          &
     &        IO_itp_org%itype_inter_org(inod),                         &
     &        IO_itp_org%coef_inter_org(inod,1:3)
        end do
!
      end subroutine read_gz_itp_coefs_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_table_dest(id_rank, IO_itp_dest, zbuf)
!
      use t_interpolate_tbl_dest
      use gzip_file_access
      use gz_data_IO
!
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      integer, intent(in) :: id_rank
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      write(zbuf%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  domain ID ',                  &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '! number of domain to import',   &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  domain IDs to import',        &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      write(zbuf%fixbuf(1),'(i16,2a1)') id_rank, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(i16,2a1)') IO_itp_dest%num_org_domain,     &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call write_gz_multi_int_10i8(IO_itp_dest%num_org_domain,        &
     &     IO_itp_dest%id_org_domain, zbuf)
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end if
!
!
      write(zbuf%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  stack of node to import',     &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  imported node ID',            &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call write_gz_multi_int_8i16(IO_itp_dest%num_org_domain,        &
     &   IO_itp_dest%istack_nod_tbl_dest(1:IO_itp_dest%num_org_domain), &
     &   zbuf)
        call write_gz_multi_int_8i16                                    &
     &     (IO_itp_dest%ntot_table_dest, IO_itp_dest%inod_dest_4_dest,  &
     &      zbuf)
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end if
!
      end subroutine write_gz_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_coefs_dest                                &
     &         (IO_itp_dest, IO_itp_c_dest, zbuf)
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use gzip_file_access
      use gz_data_IO
!
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, inod
!
!
      write(zbuf%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &              '!  stack by interpolation type', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  target global node ID, ',     &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  belonged local element ID ',  &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  interpolation type ID ',      &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!  generalized position ',       &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '!', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        do i = 1, IO_itp_dest%num_org_domain
          call write_gz_multi_int_8i16(ifour,                           &
     &       IO_itp_c_dest%istack_nod_tbl_wtype_dest(4*i-3:4*i), zbuf)
        end do
!
        do inod = 1, IO_itp_dest%ntot_table_dest
          write(zbuf%fixbuf(1),'(3i16,1p3E25.15e3,2a1)')                &
     &        IO_itp_c_dest%inod_gl_dest(inod),                         &
     &        IO_itp_c_dest%iele_org_4_dest(inod),                      &
     &        IO_itp_c_dest%itype_inter_dest(inod),                     &
     &        IO_itp_c_dest%coef_inter_dest(inod,1:3),                  &
     &        char(10), char(0)
          call gz_write_textbuf_no_lf(zbuf)
        end do
!
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end if
!
      end subroutine write_gz_itp_coefs_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_domain_dest(n_rank, IO_itp_dest, zbuf)
!
      use t_interpolate_tbl_dest
      use gz_data_IO
      use skip_gz_comment
!
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call skip_gz_comment_int(n_rank, zbuf)
      call skip_gz_comment_int(IO_itp_dest%num_org_domain, zbuf)
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call alloc_itp_num_dest(IO_itp_dest)
        call read_gz_multi_int(IO_itp_dest%num_org_domain,              &
     &      IO_itp_dest%id_org_domain, zbuf)
      end if
!
      end subroutine read_gz_itp_domain_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_table_dest(IO_itp_dest, zbuf)
!
      use t_interpolate_tbl_dest
      use gz_data_IO
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
      IO_itp_dest%istack_nod_tbl_dest(0) = 0
      call read_gz_multi_int(IO_itp_dest%num_org_domain,                &
     &   IO_itp_dest%istack_nod_tbl_dest(1:IO_itp_dest%num_org_domain), &
     &   zbuf)
      IO_itp_dest%ntot_table_dest                                       &
     &   = IO_itp_dest%istack_nod_tbl_dest(IO_itp_dest%num_org_domain)
!
      call alloc_itp_table_dest(IO_itp_dest)
      call read_gz_multi_int                                            &
     &   (IO_itp_dest%ntot_table_dest, IO_itp_dest%inod_dest_4_dest,    &
     &    zbuf)
!
      end subroutine read_gz_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_coefs_dest                                 &
     &         (IO_itp_dest, IO_itp_c_dest, zbuf)
!
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use gzip_file_access
      use gz_data_IO
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, inod, num
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
        call alloc_itp_coef_stack                                       &
     &     (IO_itp_dest%num_org_domain, IO_itp_c_dest)
        IO_itp_c_dest%istack_nod_tbl_wtype_dest(0) = 0
!
        do i = 1, IO_itp_dest%num_org_domain
          call read_gz_multi_int(ifour,                                 &
     &       IO_itp_c_dest%istack_nod_tbl_wtype_dest(4*i-3:4*i), zbuf)
        end do
        num = 4*IO_itp_dest%num_org_domain
        IO_itp_dest%ntot_table_dest                                     &
     &     = IO_itp_c_dest%istack_nod_tbl_wtype_dest(num)
!
        call alloc_itp_coef_dest(IO_itp_dest, IO_itp_c_dest)
!
        do inod = 1, IO_itp_dest%ntot_table_dest
          call get_one_line_text_from_gz(zbuf)
          read(zbuf%fixbuf(1),*) IO_itp_c_dest%inod_gl_dest(inod),      &
     &        IO_itp_c_dest%iele_org_4_dest(inod),                      &
     &        IO_itp_c_dest%itype_inter_dest(inod),                     &
     &        IO_itp_c_dest%coef_inter_dest(inod,1:3)
        end do
!
      end subroutine read_gz_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      end module gz_itp_table_data_IO
