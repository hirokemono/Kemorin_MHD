!>@file  gz_field_file_MPI_IO_b.f90
!!       module gz_field_file_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_step_fld_file_mpi_b(file_name, fld_IO)
!!
!!      subroutine gz_read_step_field_file_mpi_b(file_name, fld_IO)
!!      subroutine gz_rd_alloc_st_fld_file_mpi_b(file_name, fld_IO)
!!
!!      subroutine gz_rd_alloc_st_fld_head_mpi_b(file_name, fld_IO)
!!@endverbatim
!
      module gz_field_file_MPI_IO_b
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use t_field_data_IO
!
      implicit none
!
      integer(kind = kint), allocatable ::    nnod_merged(:)
      integer(kind = kint_gl), allocatable :: istack_merged_nod(:)
!
      private :: nnod_merged, istack_merged_nod
      private :: gz_write_field_data_mpi_b
      private :: set_istack_merged_nod, deallocate_istack_merged_nod
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_step_fld_file_mpi_b(file_name, fld_IO)
!
      use gz_field_data_MPI_IO_b
!
      character(len=kchara), intent(in) :: file_name
!
      type(field_IO), intent(in) :: fld_IO
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      call set_istack_merged_nod(fld_IO%nnod_IO)
!
      call MPI_FILE_OPEN                                                &
     &   (CALYPSO_COMM, file_name, MPI_MODE_WRONLY+MPI_MODE_CREATE,     &
     &    MPI_INFO_NULL, id_fld, ierr_MPI)
!
      ioff_gl = 0
      call gz_write_field_data_mpi_b(id_fld, ioff_gl,                   &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO,             &
     &    istack_merged_nod)
!
      call MPI_FILE_CLOSE(id_fld, ierr_MPI)
      call deallocate_istack_merged_nod
!
      end subroutine gz_write_step_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_field_file_mpi_b(file_name, fld_IO)
!
      use gz_field_data_MPI_IO_b
!
      character(len=kchara), intent(in) :: file_name
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call set_istack_merged_nod(fld_IO%nnod_IO)
!
      call open_rd_gzfile(file_name)
!
      call read_step_data_mpi_b
      call gz_read_fld_mul_i8head_mpi_b(nprocs, istack_merged_nod(1))
      call gz_read_fld_inthead_mpi_b(fld_IO%num_field_IO)
!
      call gz_read_fld_mul_inthead_mpi_b                                &
     &   (fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call read_field_data_mpi_b                                        &
     &   (fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%fld_name, fld_IO%d_IO, istack_merged_nod)
!
      call close_gzfile()
!
      call deallocate_istack_merged_nod
!
      end subroutine gz_read_step_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_file_mpi_b(file_name, fld_IO)
!
      use gz_field_data_MPI_IO_b
!
      character(len=kchara), intent(in) :: file_name
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call set_istack_merged_nod(fld_IO%nnod_IO)
!
      call open_rd_gzfile(file_name)
!
      call read_step_data_mpi_b
      call gz_read_fld_mul_i8head_mpi_b(nprocs, istack_merged_nod(1))
      call gz_read_fld_inthead_mpi_b(fld_IO%num_field_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call gz_read_fld_mul_inthead_mpi_b                                &
     &   (fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      fld_IO%nnod_IO = int(istack_merged_nod(my_rank+1)                 &
     &                   - istack_merged_nod(my_rank)  )
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
      call read_field_data_mpi_b                                        &
     &   (fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%fld_name, fld_IO%d_IO, istack_merged_nod)
!
      call close_gzfile()
!
      call deallocate_istack_merged_nod
!
      end subroutine gz_rd_alloc_st_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_head_mpi_b(file_name, fld_IO)
!
      use gz_field_data_MPI_IO_b
!
      character(len=kchara), intent(in) :: file_name
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call set_istack_merged_nod(fld_IO%nnod_IO)
!
      call open_rd_gzfile(file_name)
!
      call read_step_data_mpi_b
      call gz_read_fld_mul_i8head_mpi_b(nprocs, istack_merged_nod(1))
      call gz_read_fld_inthead_mpi_b(fld_IO%num_field_IO)
!
      call alloc_phys_name_IO(fld_IO)
      call gz_read_fld_mul_inthead_mpi_b                                &
     &   (fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call close_gzfile()
      call cal_istack_phys_comp_IO(fld_IO)
!
      call deallocate_istack_merged_nod
!
      end subroutine gz_rd_alloc_st_fld_head_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_field_data_mpi_b(id_fld, ioff_gl,             &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use vtk_file_MPI_IO
      use gz_field_data_MPI_IO_b
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_fld
!
!
      call write_step_data_mpi_b(id_fld, ioff_gl, nprocs-1)
!
      call gz_write_fld_mul_i8head_mpi_b                                &
     &         (id_fld, ioff_gl, nprocs, istack_merged(1))
      call gz_write_fld_inthead_mpi_b(id_fld, ioff_gl, num_field)
      call gz_write_fld_mul_inthead_mpi_b                               &
     &         (id_fld, ioff_gl, num_field, ncomp_field)
!
      call gz_write_fld_mul_charhead_mpi_b                              &
     &         (id_fld, ioff_gl, num_field, field_name)
      call gz_write_fld_realarray2_mpi_b                                &
     &         (id_fld, ioff_gl, nnod, ntot_comp, d_nod)
!
      end subroutine gz_write_field_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_step_data_mpi_b(id_fld, ioff_gl, id_rank)
!
      use m_time_data_IO
      use gz_field_data_MPI_IO_b
!
      integer(kind=kint), intent(in) :: id_rank
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer, intent(in) ::  id_fld
!
!
      call gz_write_fld_inthead_mpi_b(id_fld, ioff_gl, id_rank)
      call gz_write_fld_inthead_mpi_b(id_fld, ioff_gl, i_time_step_IO)
!
      call gz_write_fld_realhead_mpi_b(id_fld, ioff_gl, time_IO)
      call gz_write_fld_realhead_mpi_b(id_fld, ioff_gl, delta_t_IO)
!
      end subroutine write_step_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_step_data_mpi_b
!
      use m_time_data_IO
      use gz_field_data_MPI_IO_b
!
      integer(kind = kint) :: id_rank
!
!
      call gz_read_fld_inthead_mpi_b(id_rank)
      call gz_read_fld_inthead_mpi_b(i_time_step_IO)
      call gz_read_fld_realhead_mpi_b(time_IO)
      call gz_read_fld_realhead_mpi_b(delta_t_IO)
!
      end subroutine read_step_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_data_mpi_b(nnod, num_field, ncomp,          &
     &          field_name, vect, istack_merged)
!
      use m_phys_constants
      use gz_field_data_MPI_IO_b
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ncomp
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: vect(nnod,ncomp)
!
!
      call gz_read_fld_mul_charhead_mpi_b(num_field, field_name)
      call gz_read_fld_realarray2_mpi_b                                 &
     &         (nnod, ncomp, vect, istack_merged)
!
      end subroutine read_field_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_istack_merged_nod(nnod)
!
      use gz_field_data_MPI_IO_b
!
      integer(kind=kint), intent(in) :: nnod
!
      integer(kind = kint) :: ip
!
!
      call allocate_mpi_status_gz_mpi_b
!
      allocate(nnod_merged(nprocs))
      allocate(istack_merged_nod(0:nprocs))
!
      call MPI_Allgather(nnod, ione, CALYPSO_INTEGER,                   &
     &    nnod_merged, ione, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
      istack_merged_nod(0) = 0
      do ip = 1, nprocs
        istack_merged_nod(ip)                                           &
     &       =  istack_merged_nod(ip-1) + nnod_merged(ip)
      end do
!
      end subroutine set_istack_merged_nod
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_istack_merged_nod
!
      use gz_field_data_MPI_IO_b
!
      call deallocate_mpi_status_gz_mpi_b
      deallocate(istack_merged_nod, nnod_merged)
!
      end subroutine deallocate_istack_merged_nod
!
!  ---------------------------------------------------------------------
!
      end module gz_field_file_MPI_IO_b
