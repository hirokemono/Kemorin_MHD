!>@file   gz_MPI_domain_data_IO.f90
!!@brief  module gz_MPI_domain_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for gzipped binary doimain data IO
!!
!!@verbatim
!!      subroutine gz_mpi_read_domain_info(IO_param, comm_IO)
!!      subroutine gz_mpi_read_import_data(IO_param, comm_IO)
!!      subroutine gz_mpi_read_export_data(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine gz_mpi_write_domain_info(IO_param, comm_IO)
!!      subroutine gz_mpi_write_import_data(IO_param, comm_IO)
!!      subroutine gz_mpi_write_export_data(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!@endverbatim
!
      module gz_MPI_domain_data_IO
!
      use m_precision
      use m_constants
!
      use t_comm_table
      use t_calypso_mpi_IO_param
      use gz_MPI_ascii_data_IO
      use MPI_ascii_data_IO
      use data_IO_to_textline
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_domain_info(IO_param, comm_IO)
!
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: nprocs_read, ilength
!
!
      call read_integer_textline                                        &
     &   (gz_mpi_read_charahead(IO_param, len_integer_textline),        &
     &    nprocs_read)
      if(nprocs_read .ne. IO_param%nprocs_in) then
        call calypso_mpi_abort(ierr_file, '#. of subdmain is wrong')
      end if
!
      call gz_mpi_read_num_int(IO_param, comm_IO%num_neib)
      call allocate_type_neib_id(comm_IO)
!
      call gz_mpi_read_int_vector                                       &
     &   (IO_param, comm_IO%num_neib, comm_IO%id_neib)
      write(*,*) 'comm_IO%id_neib', comm_IO%id_neib
!
      end subroutine gz_mpi_read_domain_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_import_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: num_tmp
!
!
      call gz_mpi_read_num_int(IO_param, num_tmp)
      call allocate_type_import_num(comm_IO)
!
      call gz_mpi_read_int_stack(IO_param,                              &
     &    comm_IO%num_neib, comm_IO%istack_import, comm_IO%ntot_import)
!
      call gz_mpi_read_num_int(IO_param, comm_IO%ntot_import)
      call allocate_type_import_item(comm_IO)
!
      call mpi_read_comm_table                                          &
     &   (IO_param, comm_IO%ntot_import, comm_IO%item_import)
      write(*,*) 'comm_IO%item_import', comm_IO%item_import(comm_IO%ntot_import)
!
      end subroutine gz_mpi_read_import_data
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_export_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: num_tmp
!
!
      call gz_mpi_read_num_int(IO_param, num_tmp)
      call allocate_type_export_num(comm_IO)
!
      call gz_mpi_read_int_stack(IO_param,                              &
     &    comm_IO%num_neib, comm_IO%istack_export, comm_IO%ntot_export)
!
      call gz_mpi_read_num_int(IO_param, comm_IO%ntot_import)
      call allocate_type_export_item(comm_IO)
!
      call mpi_read_comm_table                                          &
     &     (IO_param, comm_IO%ntot_export, comm_IO%item_export)
!
      end subroutine gz_mpi_read_export_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_domain_info(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call gz_mpi_write_charahead(IO_param, len_integer_textline,       &
     &    integer_textline(IO_param%nprocs_in))
!
      call gz_mpi_write_int_vector                                      &
     &   (IO_param, comm_IO%num_neib, comm_IO%id_neib)
!
      call deallocate_type_neib_id(comm_IO)
!
      end subroutine gz_mpi_write_domain_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_import_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call gz_mpi_write_int_stack                                       &
     &   (IO_param, comm_IO%num_neib, comm_IO%istack_import)
!
      call gz_mpi_write_comm_table                                      &
     &   (IO_param, comm_IO%ntot_import, comm_IO%item_import)
!
      call deallocate_type_import(comm_IO)
!
      end subroutine gz_mpi_write_import_data
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_export_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call gz_mpi_write_int_stack                                       &
     &   (IO_param, comm_IO%ntot_export, comm_IO%istack_export)
!
      call gz_mpi_write_comm_table                                      &
     &   (IO_param, comm_IO%ntot_export, comm_IO%item_export)
!
      call deallocate_type_export(comm_IO)
!
      end subroutine gz_mpi_write_export_data
!
! -----------------------------------------------------------------------! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int_stack(IO_param, num, istack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(in) :: istack(0:num)
!
!
      call gz_mpi_write_int_vector(IO_param, num, istack(1))
!
      end subroutine gz_mpi_write_int_stack
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int_vector(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: ilength, i
!
!
      call set_numbers_2_head_node(num, IO_param)
      call gz_mpi_write_charahead(IO_param,                             &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &    IO_param%istack_merged))
!
      call gz_mpi_write_characters                                      &
     &   (IO_param, len_multi_int_textline(num),                        &
     &    multi_int_textline(num, int_dat))
!
      end subroutine gz_mpi_write_int_vector
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_comm_table(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: ilength, i, ist, ied
      character(len = num*len_integer_textline) :: textbuf
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      call set_numbers_2_head_node(num, IO_param)
      call gz_mpi_write_charahead(IO_param,                             &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &    IO_param%istack_merged))
!
      ilen_gz = int(real(num*len_integer_textline) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
      if(num .eq. 1) then
        call gzip_defleat_once                                          &
     &     (len_integer_textline, integer_textline(int_dat(1)),         &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
!
      else if(num .gt. 1) then
        call gzip_defleat_begin                                         &
     &     (len_integer_textline, integer_textline(int_dat(1)),         &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 2, num-1
          call gzip_defleat_cont                                        &
     &     (len_integer_textline, integer_textline(int_dat(i)),         &
     &      ilen_gz, ilen_gzipped)
        end do
        call gzip_defleat_last                                          &
     &     (len_integer_textline, integer_textline(int_dat(num)),       &
     &      ilen_gz, ilen_gzipped)
      else
        ilen_gzipped = 0
      end if
!
      call set_istack_4_parallell_data(ilen_gzipped, IO_param)
      call gz_mpi_write_charahead(IO_param,                             &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &                        IO_param%istack_merged))
!
      if(ilen_gzipped .gt. 0) then
        write(*,*) 'ilen_gzipped use', ilen_gzipped
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_comm_table
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int_stack(IO_param, num, istack, ntot)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: istack(0:num)
      integer(kind=kint), intent(inout) :: ntot
!
!
      istack(0) = 0
      call gz_mpi_read_int_vector(IO_param, num, istack(1))
      ntot = istack(num)
!
      end subroutine gz_mpi_read_int_stack
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_num_int(IO_param, num)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(inout) :: num
!
      integer(kind = kint) :: ilength, i
!
!
      ilength = len_multi_int_textline(IO_param%nprocs_in)
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param, ilength),                     &
     &    IO_param%nprocs_in, IO_param%istack_merged)
      num = int(IO_param%istack_merged(IO_param%id_rank+1))
!
      end subroutine gz_mpi_read_num_int
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int_vector(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: ilength
!
!
      ilength = len_multi_int_textline(num)
      call read_multi_int_textline                                      &
     &   (gz_mpi_read_characters(IO_param, ilength),                    &
     &    num, int_dat)
!
      end subroutine gz_mpi_read_int_vector
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_comm_table(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: i
      character(len = num*len_integer_textline) :: textbuf_d
      character(len = len_integer_textline) :: textbuf
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_multi_int_textline(IO_param%nprocs_in)),                &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)          &
     &            - IO_param%istack_merged(IO_param%id_rank))
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &   ilen_gz, gzip_buf(1))
!
      call gzip_infleat_once(ilen_gz, gzip_buf(1),                      &
     &    len(textbuf_d), textbuf_d, ilen_gzipped)
!
      if(num .eq. 1) then
        call gzip_infleat_once                                          &
     &    (ilen_gz, gzip_buf(1), len(textbuf), textbuf, ilen_gzipped)
        call read_integer_textline(textbuf, int_dat(1))
      else if(num .gt. 0) then
        call gzip_infleat_begin                                         &
     &   (ilen_gz, gzip_buf(1), len(textbuf), textbuf, ilen_gzipped)
        call read_integer_textline(textbuf, int_dat(1))
        do i = 2, num-1
          call gzip_infleat_cont                                        &
     &        (ilen_gz, len(textbuf), textbuf, ilen_gzipped)
          call read_integer_textline(textbuf, int_dat(i))
        end do
        call gzip_infleat_last                                          &
     &     (ilen_gz, len(textbuf), textbuf, ilen_gzipped)
        call read_integer_textline(textbuf, int_dat(num))
      end if
!
      deallocate(gzip_buf)
!
      end subroutine mpi_read_comm_table
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_domain_data_IO
