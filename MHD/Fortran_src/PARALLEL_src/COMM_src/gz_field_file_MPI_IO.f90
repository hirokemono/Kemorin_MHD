!>@file  gz_field_file_MPI_IO.f90
!!       module gz_field_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine write_gz_step_field_file_mpi(file_name, fld_IO)
!!
!!      subroutine read_step_field_file_gz_mpi(file_name, fld_IO)
!!      subroutine read_alloc_stp_fld_file_gz_mpi(file_name, fld_IO)
!!
!!      subroutine read_alloc_stp_fld_head_gz_mpi(file_name, fld_IO)
!!@endverbatim
!
      module gz_field_file_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_time_data_IO
      use t_field_data_IO
!
      implicit none
!
      integer(kind = kint), allocatable ::    nnod_merged(:)
      integer(kind = kint_gl), allocatable :: istack_merged_nod(:)
!
      private :: nnod_merged, istack_merged_nod
      private :: write_field_data_gz_mpi
      private :: set_istack_merged_nod, deallocate_istack_merged_nod
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_gz_step_field_file_mpi(file_name, fld_IO)
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
      call write_field_data_gz_mpi(id_fld, ioff_gl,                     &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO,             &
     &    istack_merged_nod)
!
      call MPI_FILE_CLOSE(id_fld, ierr_MPI)
!
      call deallocate_istack_merged_nod
!
      end subroutine write_gz_step_field_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_step_field_file_gz_mpi(file_name, fld_IO)
!
      use gz_field_data_IO
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      call set_istack_merged_nod(fld_IO%nnod_IO)
!
      call open_rd_gzfile(file_name)
      call read_gz_step_data
!
      call read_field_header_gz_mpi                                     &
     &   (fld_IO%num_field_IO,  istack_merged_nod)
!
      call read_gz_multi_int(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call read_field_data_gz_mpi(fld_IO%nnod_IO, fld_IO%num_field_IO,  &
     &    fld_IO%ntot_comp_IO, fld_IO%num_comp_IO, fld_IO%fld_name,     &
     &    fld_IO%d_IO, istack_merged_nod)
!
      call deallocate_istack_merged_nod
!
      end subroutine read_step_field_file_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_file_gz_mpi(file_name, fld_IO)
!
      use gz_field_data_IO
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      call set_istack_merged_nod(fld_IO%nnod_IO)
!
      call open_rd_gzfile(file_name)
      call read_gz_step_data
!
      call read_field_header_gz_mpi                                     &
     &   (fld_IO%num_field_IO,  istack_merged_nod)
      call alloc_phys_name_IO(fld_IO)
!
      call read_gz_multi_int(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      fld_IO%nnod_IO = int(istack_merged_nod(my_rank+1)                 &
     &                   - istack_merged_nod(my_rank)  )
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call read_field_data_gz_mpi(fld_IO%nnod_IO, fld_IO%num_field_IO,  &
     &    fld_IO%ntot_comp_IO, fld_IO%num_comp_IO, fld_IO%fld_name,     &
     &    fld_IO%d_IO, istack_merged_nod)
!
      call deallocate_istack_merged_nod
!
      end subroutine read_alloc_stp_fld_file_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_head_gz_mpi(file_name, fld_IO)
!
      use gz_field_data_IO
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      type(field_IO), intent(inout) :: fld_IO
!
!
      call set_istack_merged_nod(fld_IO%nnod_IO)
!
      call open_rd_gzfile(file_name)
      call read_gz_step_data
!
      call read_field_header_gz_mpi                                     &
     &   (fld_IO%num_field_IO,  istack_merged_nod)
      call alloc_phys_name_IO(fld_IO)
!
      call read_gz_multi_int(fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call deallocate_istack_merged_nod
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      end subroutine read_alloc_stp_fld_head_gz_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_field_data_gz_mpi(id_fld, ioff_gl,               &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use gz_field_data_MPI_IO
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
      integer(kind = kint) :: j, icou
!
!
      call gz_write_fld_header_mpi                                      &
     &   (id_fld, ioff_gl, step_data_buffer(nprocs-1))
      call gz_write_fld_header_mpi(id_fld, ioff_gl,                     &
     &    field_istack_nod_buffer(nprocs, istack_merged))
      call gz_write_fld_header_mpi(id_fld, ioff_gl,                     &
     &    field_num_buffer(num_field))
      call gz_write_fld_header_mpi                                      &
     &   (id_fld, ioff_gl, field_comp_buffer(num_field, ncomp_field))
!
      icou = 1
      do j = 1, num_field
        call gz_write_fld_header_mpi                                    &
     &     (id_fld, ioff_gl, each_field_name_buffer(field_name(j)))
        call gz_write_fld_vecotr_mpi(id_fld, ioff_gl,                   &
     &      nnod, ncomp_field(j), d_nod(1,icou))
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine write_field_data_gz_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_header_gz_mpi(num_field, istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use gz_field_data_MPI_IO
      use skip_gz_comment
!
      integer(kind=kint), intent(inout) :: num_field
      integer(kind = kint_gl), intent(inout) :: istack_merged(0:nprocs)
!
!
      istack_merged(0)  = 0
      call get_one_line_from_gz_f
      call read_gz_multi_int8(nprocs, istack_merged(1))
!
      call skip_gz_comment_int(num_field)
!
      end subroutine read_field_header_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_data_gz_mpi(nnod, num_field, ntot_comp,     &
     &          ncomp_field, field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use gz_field_data_MPI_IO
      use skip_gz_comment
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint) :: j, icou
!
!
      icou = 1
      do j = 1, num_field
        call skip_gz_comment_chara(field_name(j))
        call read_fld_vecotr_gz_mpi                                     &
     &     (nnod, ncomp_field(j), d_nod(1,icou), istack_merged)
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine read_field_data_gz_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_istack_merged_nod(nnod)
!
      use gz_field_data_MPI_IO
!
      integer(kind=kint), intent(in) :: nnod
!
      integer(kind = kint) :: ip
!
!
      call allocate_mpi_status_gz_mpi
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
      use gz_field_data_MPI_IO
!
      call deallocate_mpi_status_gz_mpi
      deallocate(istack_merged_nod, nnod_merged)
!
      end subroutine deallocate_istack_merged_nod
!
!  ---------------------------------------------------------------------
!
      end module gz_field_file_MPI_IO
