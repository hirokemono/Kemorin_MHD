!>@file  gz_MPI_ascii_data_IO.f90
!!       module gz_MPI_ascii_data_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_mpi_write_charahead(IO_param, ilength, chara_dat)
!!      subroutine gz_mpi_write_num_of_data(IO_param, num)
!!      subroutine gz_mpi_write_stack_over_domain(IO_param, ilength)
!!      subroutine gz_mpi_write_characters(IO_param, ilength, chara_dat)
!!
!!      subroutine gz_mpi_read_num_of_data(IO_param, num)
!!      function gz_mpi_read_charahead(IO_param, ilength)
!!        character(len=ilength) :: gz_mpi_read_charahead
!!      function gz_mpi_read_characters(IO_param, ilength)
!!        character(len=ilength) :: gz_mpi_read_characters
!!
!!      subroutine gz_mpi_skip_header(IO_param, ilength)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module gz_MPI_ascii_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use t_buffer_4_gzip
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use data_IO_to_textline
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_charahead(IO_param, ilength, chara_dat)
!
      use zlib_convert_text
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(in) :: ilength
      character(len=ilength), intent(in) :: chara_dat
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        call defleate_characters(ilength, chara_dat, zbuf)
!
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_long_write_gz(IO_param%id_file, ioffset,  &
     &      zbuf%ilen_gzipped, zbuf%gzip_buf(1))
        call dealloc_zip_buffer(zbuf)
      end if
      call MPI_BCAST(zbuf%ilen_gzipped, ione, CALYPSO_GLOBAL_INT,       &
     &    izero, CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_mpi_write_charahead
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_num_of_data(IO_param, num)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
!
!
      call set_numbers_2_head_node(num, IO_param)
      call gz_mpi_write_charahead(IO_param,                             &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &                        IO_param%istack_merged))
!
      end subroutine gz_mpi_write_num_of_data
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_stack_over_domain(IO_param, ilength)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: ilength
!
!
      call istack64_4_parallell_data(ilength, IO_param)
      call gz_mpi_write_charahead(IO_param,                             &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &                        IO_param%istack_merged))
!
      end subroutine gz_mpi_write_stack_over_domain
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_characters(IO_param, ilength, chara_dat)
!
      use data_IO_to_textline
      use zlib_convert_text
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: ilength
      character(len=ilength), intent(in) :: chara_dat
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call defleate_characters(ilength, chara_dat, zbuf)
!
      call gz_mpi_write_stack_over_domain(IO_param, zbuf%ilen_gzipped)
!
      if(zbuf%ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl + IO_param%istack_merged(my_rank)
        call calypso_mpi_seek_long_write_gz(IO_param%id_file, ioffset,  &
     &      zbuf%ilen_gzipped, zbuf%gzip_buf(1))
      end if
!
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_mpi_write_characters
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_num_of_data(IO_param, num)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(inout) :: num
!
      integer(kind = kint) :: ilength
!
!
      ilength = len_multi_int_textline(IO_param%nprocs_in)
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param, ilength),                     &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      if(IO_param%id_rank .lt. IO_param%nprocs_in) then
        num = int(IO_param%istack_merged(IO_param%id_rank+1))
      else
        num = 0
      end if
!
      end subroutine gz_mpi_read_num_of_data
!
! -----------------------------------------------------------------------
!
      function gz_mpi_read_charahead(IO_param, ilength)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: ilength
      character(len=ilength) :: gz_mpi_read_charahead
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz = 0
      integer(kind = kint) :: ilen_gzipped = 0
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,        &
     &      ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once(ilen_gz, gzip_buf(1),                    &
     &      ilength, gz_mpi_read_charahead, ilen_gzipped)
        deallocate(gzip_buf)
      end if
!
      call MPI_BCAST(gz_mpi_read_charahead, ilength,                    &
     &    CALYPSO_CHARACTER, izero,  CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_gzipped
!
      end function gz_mpi_read_charahead
!
! -----------------------------------------------------------------------
!
      function gz_mpi_read_characters(IO_param, ilength)
!
      use data_IO_to_textline
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(in) :: ilength
      character(len=ilength) :: gz_mpi_read_characters
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz = 0
      integer(kind = kint) :: ilen_gzipped = 0
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      if(ilength .le. 0) return
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)          &
     &            - IO_param%istack_merged(IO_param%id_rank))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &   ilen_gz, gzip_buf(1))
!
      call gzip_infleat_once(ilen_gz, gzip_buf(1),                      &
     &    ilength, gz_mpi_read_characters, ilen_gzipped)
      deallocate(gzip_buf)
!
      end function gz_mpi_read_characters
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_skip_header(IO_param, ilength)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: ilength
!
      character(len=1), allocatable :: chara_dat(:)
      character(len=1), allocatable :: gzip_buf(:)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz = 0
      integer(kind = kint) :: ilen_gzipped = 0
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        ilen_gz = int(real(ilength) *1.1) + 24
        allocate(chara_dat(ilength))
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,        &
     &      ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once(ilen_gz, gzip_buf(1),                    &
     &      ilength, chara_dat(1), ilen_gzipped)
        deallocate(gzip_buf)
        deallocate(chara_dat)
      end if
!
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_skip_header
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_ascii_data_IO
