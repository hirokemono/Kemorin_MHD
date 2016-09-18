!>@file   MPI_domain_data_IO.f90
!!@brief  module MPI_domain_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for gzipped binary doimain data IO
!!
!!@verbatim
!!      subroutine mpi_read_domain_info(IO_param, comm_IO)
!!      subroutine mpi_read_import_data(IO_param, comm_IO)
!!      subroutine mpi_read_export_data(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine mpi_write_domain_info(IO_param, comm_IO)
!!      subroutine mpi_write_import_data(IO_param, comm_IO)
!!      subroutine mpi_write_export_data(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!@endverbatim
!
      module MPI_domain_data_IO
!
      use m_precision
      use m_constants
!
      use t_comm_table
      use t_calypso_mpi_IO_param
      use MPI_binary_head_IO
      use MPI_binary_data_IO
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
      subroutine mpi_read_domain_info(IO_param, comm_IO)
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
     &   (mpi_read_charahead(IO_param, len_integer_textline),           &
     &    nprocs_read)
      if(nprocs_read .ne. IO_param%nprocs_in) then
        call calypso_mpi_abort(ierr_file, '#. of subdmain is wrong')
      end if
!
      call mpi_read_num_int(IO_param, comm_IO%num_neib)
!
      call allocate_type_neib_id(comm_IO)
!
      call mpi_read_int_vector                                          &
     &   (IO_param, comm_IO%num_neib, comm_IO%id_neib)
!
      end subroutine mpi_read_domain_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_import_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: num_tmp
!
!
      call mpi_read_num_int(IO_param, num_tmp)
      call allocate_type_import_num(comm_IO)
!
      call mpi_read_int_stack(IO_param,                                 &
     &    comm_IO%num_neib, comm_IO%istack_import, comm_IO%ntot_import)
!
      call mpi_read_num_int(IO_param, comm_IO%ntot_import)
      call allocate_type_import_item(comm_IO)
!
      call mpi_read_comm_table                                          &
     &   (IO_param, comm_IO%ntot_import, comm_IO%item_import)
!
      end subroutine mpi_read_import_data
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_export_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: num_tmp
!
!
      call mpi_read_num_int(IO_param, num_tmp)
!
      call allocate_type_export_num(comm_IO)
!
      call mpi_read_int_stack(IO_param,                                 &
     &    comm_IO%num_neib, comm_IO%istack_export, comm_IO%ntot_export)
!
      call mpi_read_num_int(IO_param, comm_IO%ntot_export)
      call allocate_type_export_item(comm_IO)
!
      call mpi_read_comm_table                                          &
     &     (IO_param, comm_IO%ntot_export, comm_IO%item_export)
!
      end subroutine mpi_read_export_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_domain_info(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call mpi_write_charahead(IO_param, len_integer_textline,          &
     &    integer_textline(IO_param%nprocs_in))
!
      call mpi_write_int_vector                                         &
     &   (IO_param, comm_IO%num_neib, comm_IO%id_neib)
!
      call deallocate_type_neib_id(comm_IO)
!
      end subroutine mpi_write_domain_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_import_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call mpi_write_int_stack                                          &
     &   (IO_param, comm_IO%num_neib, comm_IO%istack_import)
!
      call mpi_write_comm_table                                         &
     &   (IO_param, comm_IO%ntot_import, comm_IO%item_import)
!
      call deallocate_type_import(comm_IO)
!
      end subroutine mpi_write_import_data
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_export_data(IO_param, comm_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call mpi_write_int_stack                                          &
     &   (IO_param, comm_IO%ntot_export, comm_IO%istack_export)
!
      call mpi_write_comm_table                                         &
     &   (IO_param, comm_IO%ntot_export, comm_IO%item_export)
!
      call deallocate_type_export(comm_IO)
!
      end subroutine mpi_write_export_data
!
! -----------------------------------------------------------------------! -----------------------------------------------------------------------
!
      subroutine mpi_write_int_stack(IO_param, num, istack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(in) :: istack(0:num)
!
!
      call mpi_write_int_vector(IO_param, num, istack(1))
!
      end subroutine mpi_write_int_stack
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_int_vector(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: ilength, i
!
!
      call set_numbers_2_head_node(num, IO_param)
      call mpi_write_charahead(IO_param,                                &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &    IO_param%istack_merged))
!
      do i = 1, IO_param%nprocs_in
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &         + len_multi_int_textline(int(IO_param%istack_merged(i)))
      end do
!
      call mpi_write_charahead(IO_param,                                &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &                        IO_param%istack_merged))
!
      ilength = len_multi_int_textline(num)
      call set_istack_4_parallell_data(ilength, IO_param)
      call mpi_write_characters(IO_param, ilength,                      &
     &   multi_int_textline(num, int_dat))
!
      end subroutine mpi_write_int_vector
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_comm_table(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: ilength, i, ist, ied
      character(len = num*len_integer_textline) :: textbuf
!
!
      call set_numbers_2_head_node(num, IO_param)
      call mpi_write_charahead(IO_param,                                &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &    IO_param%istack_merged))
!
      do i = 1, IO_param%nprocs_in
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &         + len_integer_textline + IO_param%istack_merged(i)
      end do
!
      call mpi_write_charahead(IO_param,                                &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &                        IO_param%istack_merged))
!
      do i = 1, num
        ist = len_integer_textline * (i-1) + 1
        ied = len_integer_textline *  i
        textbuf(ist:ied) = integer_textline(int_dat(i))
      end do
!
      call set_istack_4_parallell_data(len(textbuf), IO_param)
      call mpi_write_characters(IO_param, len(textbuf), textbuf)
!
      end subroutine mpi_write_comm_table
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int_stack(IO_param, num, istack, ntot)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: istack(0:num)
      integer(kind=kint), intent(inout) :: ntot
!
!
      istack(0) = 0
      call mpi_read_int_vector(IO_param, num, istack(1))
      ntot = istack(num)
!
      end subroutine mpi_read_int_stack
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_num_int(IO_param, num)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(inout) :: num
!
      integer(kind = kint) :: ilength, i
!
!
      ilength = len_multi_int_textline(IO_param%nprocs_in)
      call read_int8_stack_textline                                     &
         (mpi_read_charahead(IO_param, ilength),                        &
     &    IO_param%nprocs_in, IO_param%istack_merged)
      num = IO_param%istack_merged(IO_param%id_rank+1)
!
      end subroutine mpi_read_num_int
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int_vector(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: ilength, i
!
!
      call mpi_skip_read                                               &
     &   (IO_param, len_multi_int_textline(IO_param%nprocs_in))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &         + len_multi_int_textline(int(IO_param%istack_merged(i)))
      end do
!
      ilength = len_multi_int_textline(num)
      call read_multi_int_textline                                      &
     &   (mpi_read_characters(IO_param, ilength),                       &
     &    num, int_dat)
!
      end subroutine mpi_read_int_vector
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_comm_table(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: ilength, i, ist, ied
      character(len = num*len_integer_textline) :: textbuf
!
!
      call mpi_skip_read                                               &
     &   (IO_param, len_multi_int_textline(IO_param%nprocs_in))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &         + len_integer_textline + IO_param%istack_merged(i)
      end do
!
      textbuf = mpi_read_characters(IO_param, len(textbuf))
!
      do i = 1, num
        ist = len_integer_textline * (i-1) + 1
        ied = len_integer_textline *  i
        call read_integer_textline(textbuf(ist:ied), int_dat(i))
      end do
!
      end subroutine mpi_read_comm_table
!
! -----------------------------------------------------------------------
!
      end module MPI_domain_data_IO
