!>@file  field_file_MPI_IO.f90
!!       module field_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_step_field_file_mpi(file_name, fld_IO)
!!
!!      subroutine read_alloc_step_fld_file_mpi(file_name, fld_IO)
!!@endverbatim
!
      module field_file_MPI_IO
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
      subroutine write_step_field_file_mpi(file_name, fld_IO)
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
      call deallocate_istack_merged_nod
!
      end subroutine write_step_field_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_step_fld_file_mpi(file_name, fld_IO)
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
      call read_field_header_mpi(id_fld, ioff_gl, fld_IO%num_field_IO,  &
     &    istack_merged_nod)
!
      call alloc_phys_name_IO(fld_IO)
      call read_field_num_mpi(id_fld, ioff_gl,                          &
     &    fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      fld_IO%nnod_IO = int(istack_merged_nod(my_rank+1)                 &
     &                   - istack_merged_nod(my_rank)  )
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
      call read_field_data_mpi(id_fld, ioff_gl,                         &
     &    fld_IO%nnod_IO, fld_IO%num_field_IO, fld_IO%ntot_comp_IO,     &
     &    fld_IO%num_comp_IO, fld_IO%fld_name, fld_IO%d_IO,             &
     &    istack_merged_nod)
!
      call MPI_FILE_CLOSE(id_fld, ierr_MPI)
!
      call deallocate_istack_merged_nod
!
      end subroutine read_alloc_step_fld_file_mpi
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
      integer(kind = kint) :: j, icou
!
!
      call write_vtk_header_mpi                                         &
     &   (id_fld, ioff_gl, step_data_buffer(nprocs-1))
      call write_vtk_header_mpi(id_fld, ioff_gl,                        &
     &    field_istack_nod_buffer(nprocs, istack_merged))
      call write_vtk_header_mpi(id_fld, ioff_gl,                        &
     &    field_num_buffer(num_field))
      call write_vtk_header_mpi                                         &
     &   (id_fld, ioff_gl, field_comp_buffer(num_field, ncomp_field))
!
      icou = 1
      do j = 1, num_field
        call write_vtk_header_mpi                                       &
     &     (id_fld, ioff_gl, each_field_name_buffer(field_name(j)))
        call write_fld_vecotr_mpi(id_fld, ioff_gl,                      &
     &      nnod, ncomp_field(j), d_nod(1,icou), istack_merged)
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine write_field_data_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_fld_vecotr_mpi(id_fld, ioff_gl,                  &
     &          nnod, ncomp, vect, istack_merged)
!
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: vect(nnod,ncomp)
!
      integer, intent(in) ::  id_fld
!
      real(kind = kreal) :: v1(ncomp)
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
      integer(kind = kint_gl) :: inod
!
!
      v1(1:ncomp) = 0.0d0
      ilength = len(each_field_data_buffer(ncomp, v1))
      ioffset = int(ioff_gl + ilength * istack_merged(my_rank))
      do inod = 1, nnod
        v1(1:ncomp) = vect(inod,1:ncomp)
        write(textbuf,'(a,a1)')                                         &
     &     each_field_data_buffer(ncomp, v1), char(0)
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_fld, textbuf, ilength,                   &
     &      MPI_CHARACTER, sta1, ierr_MPI)
        ioffset = ioffset + ilength
      end do
      ioff_gl = ioff_gl + ilength * istack_merged(nprocs)
!
      end subroutine write_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_header_mpi(id_fld, ioff_gl, num_field,      &
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
      integer(kind = kint_gl) :: ntot
!
!
      ilength = len(step_data_buffer(my_rank))
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        allocate(c1buf(ilength))
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, c1buf(1), ilength, MPI_CHARACTER,    &
     &                      sta1, ierr_MPI)
        call read_step_data_buffer(c1buf(1), id_rank)
        deallocate(c1buf)
      end if
      ioff_gl = ioff_gl + ilength
!
      call MPI_BCAST(i_time_step_IO, ione, CALYPSO_INTEGER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(time_IO, ione, CALYPSO_REAL, izero,                &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(delta_t_IO, ione, CALYPSO_REAL, izero,             &
     &    CALYPSO_COMM, ierr_MPI)
!
      ilength = len(field_istack_nod_buffer(nprocs, istack_merged))
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        allocate(c1buf(ilength))
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, c1buf(1), ilength, MPI_CHARACTER,    &
     &                      sta1, ierr_MPI)
        call read_field_istack_nod_buffer                               &
     &     (c1buf(1), nprocs, istack_merged)
        deallocate(c1buf)
      end if
      ioff_gl = ioff_gl + ilength
!
      call MPI_BCAST(istack_merged, (nprocs+1), CALYPSO_GLOBAL_INT,     &
     &    izero, CALYPSO_COMM, ierr_MPI)
!
      ilength = len(field_num_buffer(izero))
      if(my_rank .eq. 0) then
        ntot = 0
        ioffset = int(ioff_gl)
        allocate(c1buf(ilength))
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, c1buf(1), ilength, MPI_CHARACTER,    &
     &                      sta1, ierr_MPI)
        call read_field_num_buffer(c1buf(1), num_field)
        deallocate(c1buf)
      end if
      ioff_gl = ioff_gl + ilength
!
      call MPI_BCAST(num_field, ione, CALYPSO_INTEGER, izero,           &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine read_field_header_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_num_mpi(id_fld, ioff_gl,                    &
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
      ilength = len(field_comp_buffer(num_field, ncomp_field))
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        allocate(c1buf(ilength))
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, c1buf(1), ilength, MPI_CHARACTER,    &
     &                      sta1, ierr_MPI)
        call read_field_comp_buffer(c1buf(1), num_field, ncomp_field)
        deallocate(c1buf)
      end if
      ioff_gl = ioff_gl + ilength
!
      call MPI_BCAST(ncomp_field, num_field, CALYPSO_INTEGER, izero,    &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine read_field_num_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_data_mpi(id_fld, ioff_gl,                   &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: j, icou
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      icou = 1
      do j = 1, num_field
        if(my_rank .eq. 0) then
          ioffset = int(ioff_gl)
          ilength = kchara + 1
          allocate(c1buf(ilength))
          call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
          call MPI_FILE_READ(id_fld, c1buf(1), ilength, MPI_CHARACTER,  &
     &                      sta1, ierr_MPI)
          call read_each_field_name_buffer(c1buf(1), field_name(j))
          deallocate(c1buf)
        end if
        
        call MPI_BCAST(field_name(j), kchara, CALYPSO_CHARACTER, izero, &
     &      CALYPSO_COMM, ierr_MPI)
        call MPI_BCAST(field_name(j), ione, CALYPSO_INTEGER, izero,     &
     &      CALYPSO_COMM, ierr_MPI)
        ioff_gl = ioff_gl + len_trim(field_name(j)) + 1
!
        call read_fld_vecotr_mpi(id_fld, ioff_gl,                       &
     &      nnod, ncomp_field(j), d_nod(1,icou), istack_merged)
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine read_field_data_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_fld_vecotr_mpi(id_fld, ioff_gl,                   &
     &          nnod, ncomp, vect, istack_merged)
!
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: vect(nnod,ncomp)
!
      integer, intent(in) ::  id_fld
!
      real(kind = kreal) :: v1(ncomp)
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
      integer(kind = kint_gl) :: inod
!
!
      v1(1:ncomp) = 0.0d0
      ilength = len(each_field_data_buffer(ncomp, v1))
      ioffset = int(ioff_gl + ilength * istack_merged(my_rank))
      allocate(c1buf(ilength))
!
      do inod = 1, nnod
        call MPI_FILE_SEEK(id_fld, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_fld, c1buf(1), ilength,                   &
     &      MPI_CHARACTER, sta1, ierr_MPI)
        ioffset = ioffset + ilength
!
        call read_each_field_data_buffer(c1buf(1), ncomp, v1)
        vect(inod,1:ncomp) = v1(1:ncomp)
      end do
      deallocate(c1buf)
      ioff_gl = ioff_gl + ilength * istack_merged(nprocs)
!
      end subroutine read_fld_vecotr_mpi
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
      end module field_file_MPI_IO
