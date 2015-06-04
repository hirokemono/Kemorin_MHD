!>@file  field_file_MPI_IO_b.f90
!!       module field_file_MPI_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_step_field_file_mpi_b(file_name, fld_IO)
!!
!!      subroutine read_alloc_stp_fld_file_mpi_b(file_name, fld_IO)
!!@endverbatim
!
      module field_file_MPI_IO_b
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
      character(len=65536), private :: textbuf
      character(len=1), allocatable :: c1buf(:)
!
      integer(kind = kint), allocatable ::    nnod_merged(:)
      integer(kind = kint_gl), allocatable :: istack_merged_nod(:)
!
!>       status flag for sending
      integer, allocatable :: sta1(:)
!
      private :: nnod_merged, istack_merged_nod, sta1, c1buf
      private :: write_field_data_mpi
      private :: set_istack_merged_nod, deallocate_istack_merged_nod
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_step_field_file_mpi_b(file_name, fld_IO)
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
      call write_field_data_mpi(id_fld, ioff_gl,                        &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO,             &
     &    istack_merged_nod)
!
      call MPI_FILE_CLOSE(id_fld, ierr_MPI)
      call deallocate_istack_merged_nod
!
      end subroutine write_step_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_stp_fld_file_mpi_b(file_name, fld_IO)
!
      character(len=kchara), intent(in) :: file_name
!
      type(field_IO), intent(inout) :: fld_IO
!
      integer :: id_fld
      integer(kind = kint_gl) :: ioff_gl
!
!
      call set_istack_merged_nod(fld_IO%nnod_IO)
!
      call MPI_FILE_OPEN                                                &
     &   (CALYPSO_COMM, file_name, MPI_MODE_RDONLY,                     &
     &    MPI_INFO_NULL, id_fld, ierr_MPI)
!
      ioff_gl = 0
      call read_step_data_mpi_b(id_fld, ioff_gl, my_rank)
      call read_field_header_mpi_b(id_fld, ioff_gl,                     &
     &    fld_IO%num_field_IO, istack_merged_nod)
!
      call alloc_phys_name_IO(fld_IO)
      call read_field_num_mpi_b(id_fld, ioff_gl,                        &
     &    fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      fld_IO%nnod_IO = int(istack_merged_nod(my_rank+1)                 &
     &                   - istack_merged_nod(my_rank)  )
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
      call read_field_data_mpi_b(id_fld, ioff_gl,                       &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%fld_name, fld_IO%d_IO, istack_merged_nod)
!
      call MPI_FILE_CLOSE(id_fld, ierr_MPI)
!
      call deallocate_istack_merged_nod
!
      end subroutine read_alloc_stp_fld_file_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_field_data_mpi(id_fld, ioff_gl,                  &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use vtk_file_MPI_IO
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
      call write_field_nod_mpi_b                                        &
     &   (id_fld, ioff_gl, num_field, istack_merged)
      call write_field_num_mpi_b                                        &
     &   (id_fld, ioff_gl, num_field, ncomp_field)
!
      call write_fld_vecotr_mpi_b(id_fld, ioff_gl, nnod,                &
     &    num_field, ntot_comp, field_name, d_nod, istack_merged)
!
      end subroutine write_field_data_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_step_data_mpi_b(id_fld, ioff_gl, id_rank)
!
      integer(kind=kint), intent(in) :: id_rank
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      ilength = ione
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, id_rank, ilength,                   &
     &                       CALYPSO_INTEGER, sta1, ierr_MPI)
        ioffset = ioffset + kint * ilength
!
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, i_time_step_IO, ilength,            &
     &                       CALYPSO_INTEGER, sta1, ierr_MPI)
        ioffset = ioffset + kint * ilength
!
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, time_IO, ilength,                   &
     &                       CALYPSO_REAL, sta1, ierr_MPI)
        ioffset = ioffset + kreal * ilength
!
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, delta_t_IO, ilength,                &
     &                       CALYPSO_REAL, sta1, ierr_MPI)
        ioffset = ioffset + kreal * ilength
      end if
      ioff_gl = ioff_gl + 2*kint + 2*kreal
!
      end subroutine write_step_data_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine write_field_nod_mpi_b                                  &
     &         (id_fld, ioff_gl, num_field, istack_merged)
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      ilength = nprocs
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, istack_merged(1), ilength,          &
     &                       CALYPSO_GLOBAL_INT, sta1, ierr_MPI)
      end if
      ioff_gl = ioff_gl + ilength*kint_gl
!
      ilength = ione
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, num_field, ilength,                 &
     &                       CALYPSO_INTEGER, sta1, ierr_MPI)
      end if
      ioff_gl = ioff_gl + ilength*kint
!
      end subroutine write_field_nod_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine write_field_num_mpi_b                                  &
     &         (id_fld, ioff_gl, num_field, ncomp_field)
!
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      ilength = num_field
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, ncomp_field, ilength,               &
     &                       CALYPSO_INTEGER, sta1, ierr_MPI)
      end if
      ioff_gl = ioff_gl + ilength * kint
!
      end subroutine write_field_num_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_fld_vecotr_mpi_b(id_fld, ioff_gl, nnod,          &
     &          num_field, ncomp, field_name, vect, istack_merged)
!
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field, ncomp
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: vect(nnod,ncomp)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      ilength = num_field * kchara
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, field_name, ilength,                &
     &                        CALYPSO_CHARACTER, sta1, ierr_MPI)
      end if
      ioff_gl = ioff_gl + ilength
!
      ilength = nnod * ncomp
      ioffset = int(ioff_gl + kreal * ncomp*istack_merged(my_rank))
      call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_fld, vect, ilength,                        &
     &    CALYPSO_REAL, sta1, ierr_MPI)
      ioff_gl = ioff_gl + kreal * ncomp*istack_merged(nprocs)
!
      end subroutine write_fld_vecotr_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_step_data_mpi_b(id_fld, ioff_gl, my_rank)
!
      integer(kind=kint), intent(in) :: my_rank
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: id_rank
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      ilength = ione
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, id_rank, ilength,                    &
     &                       CALYPSO_INTEGER, sta1, ierr_MPI)
        ioffset = ioffset + kint * ilength
!
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, i_time_step_IO, ilength,             &
     &                       CALYPSO_INTEGER, sta1, ierr_MPI)
        ioffset = ioffset + kint * ilength
!
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, time_IO, ilength,                    &
     &                       CALYPSO_REAL, sta1, ierr_MPI)
        ioffset = ioffset + kreal * ilength
!
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, delta_t_IO, ilength,                 &
     &                       CALYPSO_REAL, sta1, ierr_MPI)
        ioffset = ioffset + kreal * ilength
      end if
      ioff_gl = ioff_gl + 2*kint + 2*kreal
!
      call MPI_BCAST(i_time_step_IO, ione, CALYPSO_INTEGER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(time_IO, ione, CALYPSO_REAL, izero,                &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(delta_t_IO, ione, CALYPSO_REAL, izero,             &
     &    CALYPSO_COMM, ierr_MPI)
!
        write(*,*) my_rank, id_rank, 'i_time_step_IO', i_time_step_IO,  &
     &          time_IO, delta_t_IO
!
      end subroutine read_step_data_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_field_header_mpi_b(id_fld, ioff_gl, num_field,    &
     &          istack_merged)
!
      use m_phys_constants
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(inout) :: num_field
      integer(kind = kint_gl), intent(inout) :: istack_merged(0:nprocs)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: id_rank
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      ilength = nprocs
      if(my_rank .eq. 0) then
        istack_merged(0) = 0
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, istack_merged(1), ilength,           &
     &                       CALYPSO_GLOBAL_INT, sta1, ierr_MPI)
      end if
      ioff_gl = ioff_gl + ilength*kint_gl
!
      call MPI_BCAST(istack_merged, (nprocs+1), CALYPSO_GLOBAL_INT,     &
     &    izero, CALYPSO_COMM, ierr_MPI)
!
!
      ilength = ione
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, num_field, ilength,                  &
     &                       CALYPSO_INTEGER, sta1, ierr_MPI)
      end if
      ioff_gl = ioff_gl + ilength*kint
!
      call MPI_BCAST(num_field, ione, CALYPSO_INTEGER, izero,           &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine read_field_header_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine read_field_num_mpi_b(id_fld, ioff_gl,                  &
     &          num_field, ncomp_field)
!
      use m_phys_constants
      use field_data_IO
      use vtk_file_MPI_IO
      use ucd_data_to_buffer
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      ilength = num_field
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, ncomp_field, ilength,                &
     &                       CALYPSO_INTEGER, sta1, ierr_MPI)
      end if
      ioff_gl = ioff_gl + ilength * kint
!
      call MPI_BCAST(ncomp_field, num_field, CALYPSO_INTEGER, izero,    &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine read_field_num_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_data_mpi_b(id_fld, ioff_gl, nnod,           &
     &          num_field, ncomp, field_name, vect, istack_merged)
!
      use m_phys_constants
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ncomp
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: vect(nnod,ncomp)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      ilength = num_field * kchara
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, field_name, ilength,                 &
     &                        CALYPSO_CHARACTER, sta1, ierr_MPI)
      end if
      ioff_gl = ioff_gl + ilength
!
      call MPI_BCAST(field_name, (num_field*kchara), CALYPSO_CHARACTER, &
     &      izero, CALYPSO_COMM, ierr_MPI)
!
      ioffset = int(ioff_gl + kreal * ncomp*istack_merged(my_rank))
      ilength = nnod * ncomp
      call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_fld, vect, ilength,                         &
     &    CALYPSO_REAL, sta1, ierr_MPI)
      ioff_gl = ioff_gl + kreal * ncomp*istack_merged(nprocs)
!
      end subroutine read_field_data_mpi_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_istack_merged_nod(nnod)
!
      integer(kind=kint), intent(in) :: nnod
!
      integer(kind = kint) :: ip
!
!
      allocate(nnod_merged(nprocs))
      allocate(istack_merged_nod(0:nprocs))
      allocate(sta1(MPI_STATUS_SIZE))
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
      deallocate(sta1)
      deallocate(istack_merged_nod, nnod_merged)
!
      end subroutine deallocate_istack_merged_nod
!
!  ---------------------------------------------------------------------
!
      end module field_file_MPI_IO_b
