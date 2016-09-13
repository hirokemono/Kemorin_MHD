!>@file   MPI_domain_data_IO.f90
!!@brief  module MPI_domain_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for gzipped binary doimain data IO
!!
!!@verbatim
!!      subroutine mpi_read_domain_info                                 &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!!      subroutine mpi_read_import_data                                 &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!!      subroutine mpi_read_export_data                                 &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine mpi_write_domain_info                                &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!!      subroutine mpi_write_import_data                                &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!!      subroutine mpi_write_export_data                                &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!@endverbatim
!!
!@param id_file file ID
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
!
      implicit none
!
      type(calypso_MPI_IO_params), private :: IO_param
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_read_domain_info                                   &
     &         (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      use m_error_IDs
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(communication_table), intent(inout) :: comm_IO
!
      integer(kind = kint) :: nprocs_read
!
!
      call mpi_read_one_inthead_b(id_file, ioff_gl, nprocs_read)
      if(nprocs_read .ne. nprocs_in) then
        call calypso_mpi_abort(ierr_file, '#. of subdmain is wrong')
      end if
!
      call mpi_read_one_integer_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO%num_neib)
!
      call allocate_type_neib_id(comm_IO)
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
!
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    comm_IO%num_neib, comm_IO%id_neib, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      end subroutine mpi_read_domain_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_import_data                                   &
     &         (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(communication_table), intent(inout) :: comm_IO
!
!
      call allocate_type_import_num(comm_IO)
      if (comm_IO%num_neib .gt. 0) then
        call alloc_istack_merge(id_rank, nprocs_in, IO_param)
        call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
        call mpi_read_integer_stack_b                                   &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      comm_IO%num_neib, comm_IO%istack_import,                    &
     &      comm_IO%ntot_import, IO_param%istack_merged)
!
        call allocate_type_import_item(comm_IO)
!
        call set_istack_4_parallell_data(comm_IO%ntot_import, IO_param)
        call mpi_read_int_vector_b                                      &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      comm_IO%ntot_import, comm_IO%item_import,                   &
     &      IO_param%istack_merged)
        call dealloc_istack_merge(IO_param)
      else
        comm_IO%ntot_import = 0
        call allocate_type_import_item(comm_IO)
      end if
!
      end subroutine mpi_read_import_data
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_export_data                                   &
     &         (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(communication_table), intent(inout) :: comm_IO
!
!
      call allocate_type_export_num(comm_IO)
      if (comm_IO%num_neib .gt. 0) then
        call alloc_istack_merge(id_rank, nprocs_in, IO_param)
        call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
        call mpi_read_integer_stack_b                                   &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      comm_IO%num_neib, comm_IO%istack_export,                    &
     &      comm_IO%ntot_export, IO_param%istack_merged)
!
        call allocate_type_export_item(comm_IO)
!
        call set_istack_4_parallell_data(comm_IO%ntot_export, IO_param)
        call mpi_read_int_vector_b                                      &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      comm_IO%ntot_export, comm_IO%item_export,                   &
     &      IO_param%istack_merged)
        call dealloc_istack_merge(IO_param)
      else
        comm_IO%ntot_export = 0
        call allocate_type_export_item(comm_IO)
      end if
!
      end subroutine mpi_read_export_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_domain_info                                  &
     &         (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(communication_table), intent(inout) :: comm_IO
!
!
      call mpi_write_one_inthead_b(id_file, ioff_gl, nprocs_in)
      call mpi_write_one_integer_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO%num_neib)
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
!
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    comm_IO%num_neib, comm_IO%id_neib, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      call deallocate_type_neib_id(comm_IO)
!
      end subroutine mpi_write_domain_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_import_data                                  &
     &         (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(communication_table), intent(inout) :: comm_IO
!
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
      call mpi_write_integer_stack_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    comm_IO%num_neib, comm_IO%istack_import,                      &
     &    IO_param%istack_merged)
!
      call set_istack_4_parallell_data(comm_IO%ntot_import, IO_param)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    comm_IO%ntot_import, comm_IO%item_import,                     &
     &    IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      call deallocate_type_import(comm_IO)
!
      end subroutine mpi_write_import_data
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_export_data                                  &
     &         (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(communication_table), intent(inout) :: comm_IO
!
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
      call mpi_write_integer_stack_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    comm_IO%num_neib, comm_IO%istack_export,                      &
     &    IO_param%istack_merged)
!
      call set_istack_4_parallell_data(comm_IO%ntot_export, IO_param)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    comm_IO%ntot_export, comm_IO%item_export,                     &
     &    IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      call deallocate_type_export(comm_IO)
!
      end subroutine mpi_write_export_data
!
! -----------------------------------------------------------------------!
      end module MPI_domain_data_IO
