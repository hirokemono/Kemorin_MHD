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
     &   (mpi_read_charahead(IO_param, len_int_txt), nprocs_read)
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
      call mpi_read_num_int(IO_param, num_tmp)
      call allocate_type_export_num(comm_IO)
!
      call mpi_read_int_stack(IO_param,                                 &
     &    comm_IO%num_neib, comm_IO%istack_export, comm_IO%ntot_export)
!
      call mpi_read_num_int(IO_param, comm_IO%ntot_export)
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
     &   (IO_param, comm_IO%ntot_export, comm_IO%istack_export)
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
      subroutine mpi_write_comm_table(IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, ilength, nrest, lst, led
      character(len = num*len_int_txt) :: textbuf
!
!
      call set_numbers_2_head_node(num, IO_param)
      call mpi_write_charahead(IO_param,                                &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &    IO_param%istack_merged))
!
      if(num .le. 0) then
        ilength = ione
      else if(num .le. ncolumn) then
        ilength = len_multi_int_textline(num)
        textbuf(1:ilength) =  multi_int_textline(num, int_dat(1))
      else if(num .gt. 0) then
        ilength = len_multi_int_textline(ncolumn)
        lst = 0
        led = lst + ilength
        textbuf(lst+1:led) =  multi_int_textline(ncolumn, int_dat(1))
        do i = 1, (num-1)/ncolumn - 1
          ilength = len_multi_int_textline(ncolumn)
          lst = led
          led = lst + ilength
          textbuf(lst+1:led)                                            &
     &            =  multi_int_textline(ncolumn, int_dat(ncolumn*i+1))
        end do
        nrest = mod((num-1),ncolumn) + 1
        ilength = len_multi_int_textline(nrest)
        lst = led
        led = lst + ilength
        textbuf(lst+1:led)                                              &
     &            =  multi_int_textline(nrest, int_dat(num-nrest+1))
        ilength = led
      end if
!
      call set_istack_4_parallell_data(ilength, IO_param)
      call mpi_write_charahead(IO_param,                                &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &                        IO_param%istack_merged))
!
      if(num .le. 0) then
        call mpi_write_characters(IO_param, ione, char(10))
      else
        call mpi_write_characters(IO_param, ilength, textbuf)
      end if
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
      integer(kind = kint) :: ilength
!
!
      ilength = len_multi_int_textline(IO_param%nprocs_in)
      call read_int8_stack_textline                                     &
         (mpi_read_charahead(IO_param, ilength),                        &
     &    IO_param%nprocs_in, IO_param%istack_merged)
      num = int(IO_param%istack_merged(IO_param%id_rank+1))
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
      subroutine mpi_read_comm_table(IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: i, ilength, nrest, n_item, lst, led, loop
      character(len = num*len_int_txt) :: textbuf
!
!
      call mpi_skip_read                                                &
     &   (IO_param, len_multi_int_textline(IO_param%nprocs_in))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        n_item = IO_param%istack_merged(i)
        if(n_item .le. 0) then
          ilength = ione
        else if(n_item .le. ncolumn) then
          ilength = len_multi_int_textline(n_item)
        else if(n_item .gt. 0) then
          nrest = mod((n_item-1),ncolumn) + 1
          loop = (n_item-1)/ncolumn
          ilength = len_multi_int_textline(nrest)                       &
     &             + len_multi_int_textline(ncolumn) * loop
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &                             + ilength
      end do
      ilength = IO_param%istack_merged(IO_param%id_rank+1)              &
     &         -  IO_param%istack_merged(IO_param%id_rank)
!
      textbuf = mpi_read_characters(IO_param, ilength)
!
      if(num .le. 0) then
        ilength = ione
      else if(num .le. ncolumn) then
        ilength = len_multi_int_textline(num)
        call read_multi_int_textline(textbuf, num, int_dat(1))
      else if(num .gt. 0) then
        ilength = len_multi_int_textline(ncolumn)
        lst = 0
        led = lst + ilength
        call read_multi_int_textline                                    &
     &    (textbuf(lst+1:led) , ncolumn, int_dat(1))
        do i = 1, (num-1)/ncolumn - 1
          ilength = len_multi_int_textline(ncolumn)
          lst = led
          led = lst + ilength
          call read_multi_int_textline                                  &
     &       (textbuf(lst+1:led) , ncolumn, int_dat(ncolumn*i+1))
        end do
        nrest = mod((num-1),ncolumn) + 1
        ilength = len_multi_int_textline(nrest)
        lst = led
        led = lst + ilength
        call read_multi_int_textline                                    &
     &     (textbuf(lst+1:led), nrest, int_dat(num-nrest+1))
      end if
!
      end subroutine mpi_read_comm_table
!
! -----------------------------------------------------------------------
!
      end module MPI_domain_data_IO
