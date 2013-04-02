!gz_itp_table_data_IO.f90
!      module gz_itp_table_data_IO
!
!        programmed by H.Matsui on Sep. 2012
!
!      subroutine write_gz_itp_domain_org(my_rank)
!      subroutine write_gz_itp_table_org
!
!      subroutine read_gz_itp_domain_org(n_rank)
!      subroutine read_gz_itp_table_org
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
      subroutine write_gz_itp_domain_org(my_rank)
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  domain ID ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  number of destination domain', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  domain IDs to send', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(i10,a1)') my_rank, char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(i10,a1)') num_dest_domain_IO, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      if (num_dest_domain_IO .gt. 0) then
        call write_gz_multi_int_10i8(num_dest_domain_IO,                &
     &     id_dest_domain_IO)
      else
        write(textbuf,'(a1)') char(0)
        call write_compress_txt(nbuf, textbuf)
      end if
!
!
      end subroutine write_gz_itp_domain_org
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_table_org
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint) :: i, inod
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  stack by interpolation type', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  destinate global node ID, ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  local element ID for interpolation',  &
     &                          char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  interpolation type ID ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  generalized position ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      if (num_dest_domain_IO .gt. 0) then
        do i = 1, num_dest_domain_IO
          call write_gz_multi_int_8i10(ifour,                           &
     &        istack_table_wtype_org_IO(4*i-3) )
        end do
!
        do inod = 1, ntot_table_org_IO
          write(textbuf,'(3i10,1p3e23.12,a1)')                          &
     &        inod_gl_dest_4_org_IO(inod), iele_org_4_org_IO(inod),     &
     &        itype_inter_org_IO(inod), coef_inter_org_IO(inod,1:3),    &
     &        char(0)
          call write_compress_txt(nbuf, textbuf)
        end do
!
      else
        write(textbuf,'(a1)') char(0)
        call write_compress_txt(nbuf, textbuf)
      end if
!
      end subroutine write_gz_itp_table_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_domain_org(n_rank)
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint), intent(inout) :: n_rank
!
!
      call skip_gz_comment_int(n_rank)
      call skip_gz_comment_int(num_dest_domain_IO)
!
      if (num_dest_domain_IO .gt. 0) then
        call allocate_itp_num_org_IO
        call read_gz_multi_int(num_dest_domain_IO, id_dest_domain_IO)
      end if
!
      end subroutine read_gz_itp_domain_org
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_table_org
!
      use m_interpolate_table_org_IO
!
      integer(kind = kint) :: i, inod, nchara
!
!
      if (num_dest_domain_IO .gt. 0) then
!
        istack_table_wtype_org_IO(0) = 0
        do i = 1, num_dest_domain_IO
          call read_gz_multi_int(ifour,                                 &
     &        istack_table_wtype_org_IO(4*i-3) )
        end do
        ntot_table_org_IO                                               &
     &        = istack_table_wtype_org_IO(4*num_dest_domain_IO)
!
        call allocate_itp_table_org_IO
!
        do inod = 1, ntot_table_org_IO
          call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
          read(textbuf,*) inod_gl_dest_4_org_IO(inod),                  &
     &        iele_org_4_org_IO(inod), itype_inter_org_IO(inod),        &
     &        coef_inter_org_IO(inod,1:3)
        end do
!
      end if
!
      end subroutine read_gz_itp_table_org
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
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  domain ID ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  number of domain of origin', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  originate domain IDs', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(i10,a1)') my_rank, char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(i10,a1)') num_org_domain_IO, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      if (num_org_domain_IO .gt. 0) then
        call write_gz_multi_int_10i8(num_org_domain_IO,                 &
     &     id_org_domain_IO)
      else
        write(textbuf,'(a1)') char(0)
        call write_compress_txt(nbuf, textbuf)
      end if
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  stack of originate domain', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  destination node ID', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      if (num_org_domain_IO .gt. 0) then
        call write_gz_multi_int_8i10(num_org_domain_IO,                 &
     &      istack_table_dest_IO(1))
        call write_gz_multi_int_8i10(ntot_table_dest_IO,                &
     &      inod_dest_IO(1))
      else
        write(textbuf,'(a1)') char(0)
        call write_compress_txt(nbuf, textbuf)
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
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  stack by interpolation type', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  target global node ID, ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  belonged local element ID ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  interpolation type ID ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!  generalized position ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      if (num_org_domain_IO .gt. 0) then
        do i = 1, num_org_domain_IO
          call write_gz_multi_int_8i10(ifour,                           &
     &        istack_table_wtype_dest_IO(4*i-3))
        end do
!
        do inod = 1, ntot_table_dest_IO
          write(textbuf,'(3i10,1p3e23.12,a1)')                          &
     &        inod_global_dest_IO(inod), iele_orgin_IO(inod),           &
     &        itype_inter_dest_IO(inod), coef_inter_dest_IO(inod,1:3),  &
     &        char(0)
          call write_compress_txt(nbuf, textbuf)
        end do
!
      else
        write(textbuf,'(a1)') char(0)
        call write_compress_txt(nbuf, textbuf)
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
      call skip_gz_comment_int(num_org_domain_IO)
!
      if (num_org_domain_IO .gt. 0) then
        call allocate_itp_num_dst_IO
        call read_gz_multi_int(num_org_domain_IO, id_org_domain_IO)
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
      if (num_org_domain_IO .gt. 0) then
        istack_table_dest_IO(0) = 0
        call read_gz_multi_int(num_org_domain_IO,                       &
     &      istack_table_dest_IO(1))
        ntot_table_dest_IO = istack_table_dest_IO(num_org_domain_IO)
!
        call allocate_itp_nod_dst_IO
        call read_gz_multi_int(ntot_table_dest_IO, inod_dest_IO)
      end if
!
      end subroutine read_gz_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_coefs_dest
!
      use m_interpolate_table_dest_IO
!
      integer(kind = kint) :: i, inod, nchara
!
!
      if (num_org_domain_IO .gt. 0) then
        istack_table_wtype_dest_IO(0) = 0
        do i = 1, num_org_domain_IO
          call read_gz_multi_int(ifour,                                 &
     &        istack_table_wtype_dest_IO(4*i-3) )
        end do
        ntot_table_dest_IO                                              &
     &        = istack_table_wtype_dest_IO(4*num_org_domain_IO)
!
        call allocate_itp_coefs_dst_IO
!
        do inod = 1, ntot_table_dest_IO
          call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
          read(textbuf,*) inod_global_dest_IO(inod),                    &
     &        iele_orgin_IO(inod), itype_inter_dest_IO(inod),           &
     &        coef_inter_dest_IO(inod,1:3)
        end do
!
      end if
!
      end subroutine read_gz_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      end module gz_itp_table_data_IO
