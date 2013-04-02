!filter_moment_IO_select.F90
!      module filter_moment_IO_select
!
!     Written by H. Matsui in 2004
!
!      subroutine sel_read_num_filter_mom_file(my_rank)
!      subroutine sel_read_filter_elen_file(my_rank, nnod, nele, ierr)
!      subroutine sel_write_filter_elen_file(my_rank)
!
!      subroutine sel_read_filter_moms_file(my_rank, nnod, nele, ierr)
!      subroutine sel_write_filter_moms_file(my_rank)
!
      module filter_moment_IO_select
!
      use m_precision
!
      use m_filter_file_names
      use m_file_format_switch
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_num_filter_mom_file(my_rank)
!
      use filter_moments_file_IO
      use gz_filter_moments_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
      character(len=kchara) :: file_name
!
!
      call add_int_suffix(my_rank, filter_file_head, file_name)
        call read_num_filter_mom_file_b(file_name, my_rank)
!
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
!
#ifdef ZLIB_IO
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call read_num_filter_mom_file_gz(file_name, my_rank)
#endif
!
      else
        call read_num_filter_mom_file(file_name, my_rank)
      end if
!
      end subroutine sel_read_num_filter_mom_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_read_filter_elen_file(my_rank, nnod, nele, ierr)
!
      use filter_moments_file_IO
      use gz_filter_moments_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      call add_int_suffix(my_rank, filter_file_head, file_name)
!
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call read_filter_elen_file_b(file_name, my_rank,                &
     &      nnod, nele, ierr)
!
#ifdef ZLIB_IO
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call read_filter_elen_file_gz(file_name, my_rank,               &
     &      nnod, nele, ierr)
#endif
!
      else
        call read_filter_elen_file(file_name, my_rank,                  &
     &      nnod, nele, ierr)
      end if
!
      end subroutine sel_read_filter_elen_file
!
!-----------------------------------------------------------------------
!
      subroutine sel_write_filter_elen_file(my_rank)
!
      use filter_moments_file_IO
      use gz_filter_moments_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
      character(len=kchara) :: file_name
!
!
      call add_int_suffix(my_rank, filter_file_head, file_name)
!
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call write_filter_elen_file_b(file_name, my_rank)
!
#ifdef ZLIB_IO
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call write_filter_elen_file_gz(file_name, my_rank)
#endif
!
      else
        call write_filter_elen_file(file_name, my_rank)
      end if
!
      end subroutine sel_write_filter_elen_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_read_filter_moms_file(my_rank, nnod, nele, ierr)
!
      use filter_moments_file_IO
      use gz_filter_moments_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      call add_int_suffix(my_rank, filter_file_head, file_name)
!
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call read_filter_moments_file_b(file_name, my_rank,             &
     &      nnod, nele, ierr)
!
#ifdef ZLIB_IO
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call read_filter_moments_file_gz(file_name, my_rank,            &
     &      nnod, nele, ierr)
#endif
!
      else
        call read_filter_moments_file(file_name, my_rank,               &
     &      nnod, nele, ierr)
      end if
!
      end subroutine sel_read_filter_moms_file
!
!-----------------------------------------------------------------------
!
      subroutine sel_write_filter_moms_file(my_rank)
!
      use filter_moments_file_IO
      use gz_filter_moments_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
      character(len=kchara) :: file_name
!
!
      call add_int_suffix(my_rank, filter_file_head, file_name)
!
      if (ifmt_filter_file .eq. id_binary_file_fmt) then
        call write_filter_moments_file_b(file_name, my_rank)
!
#ifdef ZLIB_IO
      else if(ifmt_filter_file .eq. id_gzip_txt_file_fmt) then
        call write_filter_moments_file_gz(file_name, my_rank)
#endif
!
      else
        call write_filter_moments_file(file_name, my_rank)
      end if
!
      end subroutine sel_write_filter_moms_file
!
!-----------------------------------------------------------------------
!
      end module filter_moment_IO_select
