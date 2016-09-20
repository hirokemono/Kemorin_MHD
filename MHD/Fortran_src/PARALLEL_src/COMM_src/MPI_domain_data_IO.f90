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
!!
!!      subroutine mpi_read_int_stack(IO_param, num, istack, ntot)
!!      subroutine mpi_read_comm_table(IO_param, ncolumn, num, int_dat)
!!      subroutine mpi_write_int_stack(IO_param, num, istack)
!!      subroutine mpi_write_comm_table(IO_param, ncolumn, num, int_dat)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
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
      private :: mpi_write_int_vector, mpi_read_int_vector
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
      integer(kind = kint) :: nprocs_read
!
!
      call read_integer_textline                                        &
     &   (mpi_read_charahead(IO_param, len_int_txt), nprocs_read)
      if(nprocs_read .ne. IO_param%nprocs_in) then
        call calypso_mpi_abort(ierr_file, '#. of subdmain is wrong')
      end if
!
      call mpi_read_num_of_data(IO_param, comm_IO%num_neib)
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
      call mpi_read_num_of_data(IO_param, num_tmp)
      call allocate_type_import_num(comm_IO)
!
      call mpi_read_int_stack(IO_param,                                 &
     &    comm_IO%num_neib, comm_IO%istack_import, comm_IO%ntot_import)
!
      call mpi_read_num_of_data(IO_param, comm_IO%ntot_import)
      call allocate_type_import_item(comm_IO)
!
      call mpi_read_comm_table                                          &
     &   (IO_param, ione, comm_IO%ntot_import, comm_IO%item_import)
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
      call mpi_read_num_of_data(IO_param, num_tmp)
      call allocate_type_export_num(comm_IO)
!
      call mpi_read_int_stack(IO_param,                                 &
     &    comm_IO%num_neib, comm_IO%istack_export, comm_IO%ntot_export)
!
      call mpi_read_num_of_data(IO_param, comm_IO%ntot_export)
      call allocate_type_export_item(comm_IO)
!
      call mpi_read_comm_table                                          &
     &     (IO_param, ione, comm_IO%ntot_export, comm_IO%item_export)
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
      call mpi_write_charahead(IO_param, len_int_txt,                   &
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
     &   (IO_param, ione, comm_IO%ntot_import, comm_IO%item_import)
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
     &   (IO_param, comm_IO%num_neib, comm_IO%istack_export)
!
      call mpi_write_comm_table                                         &
     &   (IO_param, ione, comm_IO%ntot_export, comm_IO%item_export)
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
      if(num .gt. 0) call mpi_write_int_vector                          &
     &                  (IO_param, num, istack(1))
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
      call mpi_write_num_of_data(IO_param, num)
!
      ilength = len_multi_int_textline(num)
      IO_param%istack_merged(0) = 0
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
      call mpi_write_characters(IO_param, ilength,                      &
     &   multi_int_textline(num, int_dat))
!
      end subroutine mpi_write_int_vector
!
! -----------------------------------------------------------------------
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
     &   (mpi_read_characters(IO_param, ilength), num, int_dat)
!
      end subroutine mpi_read_int_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_comm_table(IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, n_item, lst, led, loop
      character(len = num*len_int_txt) :: textbuf
!
!
      call mpi_skip_read                                                &
     &   (IO_param, len_multi_int_textline(IO_param%nprocs_in))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        n_item = int(IO_param%istack_merged(i))
        if(n_item .le. 0) then
          led = ione
        else if(n_item .le. ncolumn) then
          led = len_multi_int_textline(n_item)
        else if(n_item .gt. 0) then
          nrest = mod((n_item-1),ncolumn) + 1
          loop = (n_item-1)/ncolumn
          led = len_multi_int_textline(nrest)                           &
     &         + len_multi_int_textline(ncolumn) * loop
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1) + led
      end do
      led = int(IO_param%istack_merged(IO_param%id_rank+1)              &
     &         -  IO_param%istack_merged(IO_param%id_rank))
!
      textbuf = mpi_read_characters(IO_param, led)
!
      if(num .le. 0) then
        led = ione
      else if(num .le. ncolumn) then
        led = len_multi_int_textline(num)
        call read_multi_int_textline(textbuf, num, int_dat(1))
      else if(num .gt. 0) then
        lst = 0
        led = lst + len_multi_int_textline(ncolumn)
        call read_multi_int_textline                                    &
     &    (textbuf(lst+1:led) , ncolumn, int_dat(1))
        do i = 1, (num-1)/ncolumn - 1
          lst = led
          led = lst + len_multi_int_textline(ncolumn)
          call read_multi_int_textline                                  &
     &       (textbuf(lst+1:led) , ncolumn, int_dat(ncolumn*i+1))
        end do
        nrest = mod((num-1),ncolumn) + 1
        lst = led
        led = lst + len_multi_int_textline(nrest)
        call read_multi_int_textline                                    &
     &     (textbuf(lst+1:led), nrest, int_dat(num-nrest+1))
      end if
!
      end subroutine mpi_read_comm_table
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_comm_table(IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, lst, led
      character(len = num*len_int_txt) :: textbuf
!
!
      call mpi_write_num_of_data(IO_param, num)
!
      if(num .le. 0) then
        led = ione
      else if(num .le. ncolumn) then
        led = len_multi_int_textline(num)
        textbuf(1:led) =  multi_int_textline(num, int_dat(1))
      else if(num .gt. 0) then
        lst = 0
        led = lst + len_multi_int_textline(ncolumn)
        textbuf(lst+1:led) =  multi_int_textline(ncolumn, int_dat(1))
        do i = 1, (num-1)/ncolumn - 1
          lst = led
          led = lst + len_multi_int_textline(ncolumn)
          textbuf(lst+1:led)                                            &
     &            =  multi_int_textline(ncolumn, int_dat(ncolumn*i+1))
        end do
        nrest = mod((num-1),ncolumn) + 1
        lst = led
        led = lst + len_multi_int_textline(nrest)
        textbuf(lst+1:led)                                              &
     &            =  multi_int_textline(nrest, int_dat(num-nrest+1))
      end if
!
      call mpi_write_stack_over_domain(IO_param, led)
!
      if(num .le. 0) then
        call mpi_write_characters(IO_param, ione, char(10))
      else
        call mpi_write_characters(IO_param, led, textbuf)
      end if
!
      end subroutine mpi_write_comm_table
!
! -----------------------------------------------------------------------
!
      end module MPI_domain_data_IO
