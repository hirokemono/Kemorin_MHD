!>@file   MPI_domain_data_IO_b.f90
!!@brief  module MPI_domain_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for gzipped binary doimain data IO
!!
!!@verbatim
!!      subroutine mpi_read_domain_info_b                               &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_read_import_data_b                               &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_read_export_data_b                               &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!
!!      subroutine mpi_write_domain_info_b                              &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_write_import_data_b                              &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_write_export_data_b                              &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!@endverbatim
!!
!@param id_file file ID
!
      module MPI_domain_data_IO_b
!
      use m_precision
      use m_constants
!
      use m_comm_data_IO
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
      subroutine mpi_read_domain_info_b                                 &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      use m_error_IDs
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
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
      call allocate_neib_domain_IO
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
!
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    comm_IO%num_neib, comm_IO%id_neib, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      end subroutine mpi_read_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_import_data_b                                 &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call allocate_import_stack_IO
      if (comm_IO%num_neib .gt. 0) then
        call alloc_istack_merge(id_rank, nprocs_in, IO_param)
        call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
        call mpi_read_integer_stack_b                                   &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      comm_IO%num_neib, istack_import_IO, comm_IO%ntot_import,    &
     &      IO_param%istack_merged)
!
        call allocate_import_item_IO
!
        call set_istack_4_parallell_data(comm_IO%ntot_import, IO_param)
        call mpi_read_int_vector_b                                      &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      comm_IO%ntot_import, item_import_IO,                        &
     &      IO_param%istack_merged)
        call dealloc_istack_merge(IO_param)
      else
        comm_IO%ntot_import = 0
        call allocate_import_item_IO
      end if
!
      end subroutine mpi_read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_export_data_b                                 &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call allocate_export_stack_IO
      if (comm_IO%num_neib .gt. 0) then
        call alloc_istack_merge(id_rank, nprocs_in, IO_param)
        call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
        call mpi_read_integer_stack_b                                   &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      comm_IO%num_neib, istack_export_IO, comm_IO%ntot_export,    &
     &      IO_param%istack_merged)
!
        call allocate_export_item_IO
!
        call set_istack_4_parallell_data(comm_IO%ntot_export, IO_param)
        call mpi_read_int_vector_b                                      &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      comm_IO%ntot_export, item_export_IO,                        &
     &      IO_param%istack_merged)
        call dealloc_istack_merge(IO_param)
      else
        comm_IO%ntot_export = 0
        call allocate_export_item_IO
      end if
!
      end subroutine mpi_read_export_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_domain_info_b                                &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
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
      call deallocate_neib_domain_IO
!
      end subroutine mpi_write_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_import_data_b                                &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
      call mpi_write_integer_stack_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    comm_IO%num_neib, istack_import_IO, IO_param%istack_merged)
!
      call set_istack_4_parallell_data(comm_IO%ntot_import, IO_param)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    comm_IO%ntot_import, item_import_IO, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      call deallocate_import_item_IO
!
      end subroutine mpi_write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_export_data_b                                &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_parallell_data(comm_IO%num_neib, IO_param)
      call mpi_write_integer_stack_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    comm_IO%num_neib, istack_export_IO, IO_param%istack_merged)
!
      call set_istack_4_parallell_data(comm_IO%ntot_export, IO_param)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    comm_IO%ntot_export, item_export_IO, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      call deallocate_export_item_IO
!
      end subroutine mpi_write_export_data_b
!
! -----------------------------------------------------------------------!
      end module MPI_domain_data_IO_b
