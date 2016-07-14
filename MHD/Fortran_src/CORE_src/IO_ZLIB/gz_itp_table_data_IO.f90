!gz_itp_table_data_IO.f90
!      module gz_itp_table_data_IO
!
!        programmed by H.Matsui on Sep. 2012
!
!      subroutine write_gz_itp_table_org(my_rank, IO_itp_org)
!      subroutine write_gz_itp_coefs_org(IO_itp_org)
!!        type(interpolate_table_org), intent(in) :: IO_itp_org
!
!      subroutine read_gz_itp_domain_org(n_rank, IO_itp_org)
!      subroutine read_gz_itp_table_org(IO_itp_org)
!      subroutine read_gz_itp_coefs_org(IO_itp_org)
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!      subroutine write_gz_itp_table_dest(my_rank)
!      subroutine write_gz_itp_coefs_dest
!
!      subroutine read_gz_itp_domain_dest(n_rank)
!      subroutine read_gz_itp_table_dest
!      subroutine read_gz_itp_coefs_dest
!
      module gz_itp_table_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use skip_gz_comment
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_table_org(my_rank, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table_org), intent(in) :: IO_itp_org
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  domain ID ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  number of domain to export', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  domain IDs to export', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      write(textbuf,'(i16,a1)') my_rank, char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(i16,a1)') IO_itp_org%num_dest_domain, char(0)
      call gz_write_textbuf_w_lf
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call write_gz_multi_int_10i8(IO_itp_org%num_dest_domain,        &
     &      IO_itp_org%id_dest_domain)
      else
        write(textbuf,'(a1)') char(0)
        call gz_write_textbuf_w_lf
      end if
!
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  stack of node to export', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  exported node ID', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call write_gz_multi_int_8i10(IO_itp_org%num_dest_domain,        &
     &     IO_itp_org%istack_nod_tbl_org(1:IO_itp_org%num_dest_domain))
        call write_gz_multi_int_8i10(IO_itp_org%ntot_table_org,         &
     &     IO_itp_org%inod_itp_send)
      else
        write(textbuf,'(a1)') char(0)
        call gz_write_textbuf_w_lf
      end if
!
      end subroutine write_gz_itp_table_org
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_coefs_org(IO_itp_org)
!
      use t_interpolate_tbl_org
!
      type(interpolate_table_org), intent(in) :: IO_itp_org
      integer(kind = kint) :: inod
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  stack by interpolation type', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  destinate global node ID, ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  local element ID for interpolation',  &
     &                          char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  interpolation type ID ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  generalized position ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call write_gz_multi_int_8i10                                    &
     &     (ifour, IO_itp_org%istack_itp_type_org(1:ifour) )
!
        do inod = 1, IO_itp_org%ntot_table_org
          write(textbuf,'(3i16,1p3E25.15e3,a1)')                        &
     &        IO_itp_org%inod_gl_dest_4_org(inod),                      &
     &        IO_itp_org%iele_org_4_org(inod),                          &
     &        IO_itp_org%itype_inter_org(inod),                         &
     &        IO_itp_org%coef_inter_org(inod,1:3), char(0)
          call gz_write_textbuf_w_lf
        end do
!
      else
        write(textbuf,'(a1)') char(0)
        call gz_write_textbuf_w_lf
      end if
!
      end subroutine write_gz_itp_coefs_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_domain_org(n_rank, IO_itp_org)
!
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(inout) :: n_rank
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      call skip_gz_comment_int(n_rank)
      call skip_gz_comment_int(IO_itp_org%num_dest_domain)
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call alloc_itp_num_org(np_smp, IO_itp_org)
        call read_gz_multi_int                                          &
     &     (IO_itp_org%num_dest_domain, IO_itp_org%id_dest_domain)
      end if
!
      end subroutine read_gz_itp_domain_org
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_table_org(IO_itp_org)
!
      use t_interpolate_tbl_org
!
      type(interpolate_table_org), intent(inout) :: IO_itp_org
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
        IO_itp_org%istack_nod_tbl_org(0) = 0
        call read_gz_multi_int(IO_itp_org%num_dest_domain,              &
     &     IO_itp_org%istack_nod_tbl_org(1:IO_itp_org%num_dest_domain))
        IO_itp_org%ntot_table_org                                       &
     &      = IO_itp_org%istack_nod_tbl_org(IO_itp_org%num_dest_domain)
!
        call alloc_itp_table_org(IO_itp_org)
        call read_gz_multi_int                                          &
     &     (IO_itp_org%ntot_table_org, IO_itp_org%inod_itp_send)
!
      end subroutine read_gz_itp_table_org
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_coefs_org(IO_itp_org)
!
      use t_interpolate_tbl_org
!
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      integer(kind = kint) :: inod
!
!
      if (IO_itp_org%num_dest_domain .eq. 0) return
!
        IO_itp_org%istack_itp_type_org(0) = 0
        call read_gz_multi_int                                          &
     &     (ifour, IO_itp_org%istack_itp_type_org(1:ifour) )
!
        do inod = 1, IO_itp_org%ntot_table_org
          call get_one_line_from_gz_f
          read(textbuf,*) IO_itp_org%inod_gl_dest_4_org(inod),          &
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
      subroutine write_gz_itp_table_dest(my_rank)
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  domain ID ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! number of domain to import', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  domain IDs to import', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      write(textbuf,'(i16,a1)') my_rank, char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(i16,a1)') IO_itp_dest%num_org_domain, char(0)
      call gz_write_textbuf_w_lf
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call write_gz_multi_int_10i8(IO_itp_dest%num_org_domain,        &
     &     IO_itp_dest%id_org_domain)
      else
        write(textbuf,'(a1)') char(0)
        call gz_write_textbuf_w_lf
      end if
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  stack of node to import', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  imported node ID', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call write_gz_multi_int_8i10(IO_itp_dest%num_org_domain,        &
     &   IO_itp_dest%istack_nod_tbl_dest(1:IO_itp_dest%num_org_domain))
        call write_gz_multi_int_8i10(IO_itp_dest%ntot_table_dest,       &
     &      IO_itp_dest%inod_dest_4_dest)
      else
        write(textbuf,'(a1)') char(0)
        call gz_write_textbuf_w_lf
      end if
!
      end subroutine write_gz_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_coefs_dest
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint) :: i, inod
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  stack by interpolation type', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  target global node ID, ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  belonged local element ID ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  interpolation type ID ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  generalized position ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        do i = 1, IO_itp_dest%num_org_domain
          call write_gz_multi_int_8i10(ifour,                           &
     &        istack_table_wtype_dest_IO(4*i-3))
        end do
!
        do inod = 1, IO_itp_dest%ntot_table_dest
          write(textbuf,'(3i16,1p3E25.15e3,a1)')                        &
     &        inod_global_dest_IO(inod), iele_orgin_IO(inod),           &
     &        itype_inter_dest_IO(inod), coef_inter_dest_IO(inod,1:3),  &
     &        char(0)
          call gz_write_textbuf_w_lf
        end do
!
      else
        write(textbuf,'(a1)') char(0)
        call gz_write_textbuf_w_lf
      end if
!
      end subroutine write_gz_itp_coefs_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_domain_dest(n_rank)
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint), intent(inout) :: n_rank
!
!
      call skip_gz_comment_int(n_rank)
      call skip_gz_comment_int(IO_itp_dest%num_org_domain)
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call allocate_itp_num_dst_IO
        call read_gz_multi_int                                          &
     &     (IO_itp_dest%num_org_domain, IO_itp_dest%id_org_domain)
      end if
!
      end subroutine read_gz_itp_domain_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_table_dest
!
      use m_interpolate_table_dest_IO
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
      IO_itp_dest%istack_nod_tbl_dest(0) = 0
      call read_gz_multi_int(IO_itp_dest%num_org_domain,                &
     &   IO_itp_dest%istack_nod_tbl_dest(1:IO_itp_dest%num_org_domain))
      IO_itp_dest%ntot_table_dest                                       &
     &   = IO_itp_dest%istack_nod_tbl_dest(IO_itp_dest%num_org_domain)
!
      call allocate_itp_nod_dst_IO
      call read_gz_multi_int                                            &
     &   (IO_itp_dest%ntot_table_dest, IO_itp_dest%inod_dest_4_dest)
!
      end subroutine read_gz_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_coefs_dest
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint) :: i, inod
!
!
      if (IO_itp_dest%num_org_domain .eq. 0) return
        istack_table_wtype_dest_IO(0) = 0
        do i = 1, IO_itp_dest%num_org_domain
          call read_gz_multi_int(ifour,                                 &
     &        istack_table_wtype_dest_IO(4*i-3) )
        end do
        IO_itp_dest%ntot_table_dest                                     &
     &     = istack_table_wtype_dest_IO(4*IO_itp_dest%num_org_domain)
!
        call allocate_itp_coefs_dst_IO
!
        do inod = 1, IO_itp_dest%ntot_table_dest
          call get_one_line_from_gz_f
          read(textbuf,*) inod_global_dest_IO(inod),                    &
     &        iele_orgin_IO(inod), itype_inter_dest_IO(inod),           &
     &        coef_inter_dest_IO(inod,1:3)
        end do
!
      end subroutine read_gz_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      end module gz_itp_table_data_IO
