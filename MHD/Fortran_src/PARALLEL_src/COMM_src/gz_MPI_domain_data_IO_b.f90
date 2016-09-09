!>@file   gz_MPI_domain_data_IO_b.f90
!!@brief  module gz_MPI_domain_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2016
!
!>@brief  Routine for gzipped binary doimain data IO
!!
!!@verbatim
!!      subroutine gz_mpi_read_domain_info_b                            &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine gz_mpi_read_import_data_b                            &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine gz_mpi_read_export_data_b                            &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!
!!      subroutine gz_mpi_write_domain_info_b                           &
!!     &         (id_file, nprocs_in, ioff_gl)
!!      subroutine gz_mpi_write_import_data_b(id_file, ioff_gl)
!!      subroutine gz_mpi_write_export_data_b(id_file, ioff_gl)
!!@endverbatim
!!
!@param id_file file ID
!
      module gz_MPI_domain_data_IO_b
!
      use m_precision
      use m_constants
!
      use m_comm_data_IO
      use gz_MPI_binary_datum_IO
      use gz_MPI_binary_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_domain_info_b                              &
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
      call gz_mpi_read_one_inthead_b(id_file, ioff_gl, nprocs_read)
      if(nprocs_read .ne. nprocs_in) then
        call calypso_mpi_abort(ierr_file, '#. of subdmain is wrong')
      end if
!
      call gz_mpi_read_one_integer_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO%num_neib)
!
      call allocate_neib_domain_IO
!
      call gz_mpi_read_int_vector_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    comm_IO%num_neib, comm_IO%id_neib)
!
      end subroutine gz_mpi_read_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_import_data_b                              &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call allocate_import_stack_IO
      if (comm_IO%num_neib .gt. 0) then
        call gz_mpi_read_integer_stack_b                                &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      comm_IO%num_neib, istack_import_IO, comm_IO%ntot_import)
!
        call allocate_import_item_IO
        call gz_mpi_read_int_vector_b                                   &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      comm_IO%ntot_import, comm_IO%item_import)
      else
        comm_IO%ntot_import = 0
        call allocate_import_item_IO
      end if
!
      end subroutine gz_mpi_read_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_export_data_b                              &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call allocate_export_stack_IO
      if (comm_IO%num_neib .gt. 0) then
        call gz_mpi_read_integer_stack_b                                &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      comm_IO%num_neib, istack_export_IO, comm_IO%ntot_export)
!
        call allocate_export_item_IO
        call gz_mpi_read_int_vector_b                                   &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      comm_IO%ntot_export, comm_IO%item_export)
      else
        comm_IO%ntot_export = 0
        call allocate_export_item_IO
      end if
!
      end subroutine gz_mpi_read_export_data_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_domain_info_b                             &
     &         (id_file, nprocs_in, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call gz_mpi_write_one_inthead_b(id_file, ioff_gl, nprocs_in)
      call gz_mpi_write_one_integer_b                                   &
     &   (id_file, ioff_gl, comm_IO%num_neib)
!
      call gz_mpi_write_int_vector_b                                    &
     &   (id_file, ioff_gl, comm_IO%num_neib, comm_IO%id_neib)
!
      call deallocate_neib_domain_IO
!
      end subroutine gz_mpi_write_domain_info_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_import_data_b(id_file, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call gz_mpi_write_integer_stack_b                                 &
     &   (id_file, ioff_gl, comm_IO%num_neib, istack_import_IO)
      call gz_mpi_write_int_vector_b                                    &
     &   (id_file, ioff_gl, comm_IO%ntot_import, comm_IO%item_import)
!
      call deallocate_import_item_IO
!
      end subroutine gz_mpi_write_import_data_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_export_data_b(id_file, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call gz_mpi_write_integer_stack_b                                 &
     &   (id_file, ioff_gl, comm_IO%num_neib, istack_export_IO)
      call gz_mpi_write_int_vector_b                                    &
     &   (id_file, ioff_gl, comm_IO%ntot_export, comm_IO%item_export)
!
      call deallocate_export_item_IO
!
      end subroutine gz_mpi_write_export_data_b
!
! -----------------------------------------------------------------------!
      end module gz_MPI_domain_data_IO_b
