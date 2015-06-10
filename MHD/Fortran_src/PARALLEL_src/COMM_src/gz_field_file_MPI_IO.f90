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
      character(len=65536), private :: textbuf
      character(len=1), allocatable :: c1buf(:)
!
      character(len=1), allocatable :: gzip_buf(:)
!
      integer(kind = kint), allocatable ::    nnod_merged(:)
      integer(kind = kint_gl), allocatable :: istack_merged_nod(:)
!
!>       status flag for sending
      integer, allocatable :: sta1(:)
!
      private :: nnod_merged, istack_merged_nod, sta1, c1buf
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
      call read_field_num_gz_mpi                                        &
     &   (fld_IO%num_field_IO, fld_IO%num_comp_IO)
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
      call read_field_num_gz_mpi                                        &
     &   (fld_IO%num_field_IO, fld_IO%num_comp_IO)
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
      call read_field_num_gz_mpi                                        &
     &   (fld_IO%num_field_IO, fld_IO%num_comp_IO)
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
      use gz_vtk_file_MPI_IO
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
      call gz_write_vtk_header_mpi                                      &
     &   (id_fld, ioff_gl, step_data_buffer(nprocs-1))
      call gz_write_vtk_header_mpi(id_fld, ioff_gl,                     &
     &    field_istack_nod_buffer(nprocs, istack_merged))
      call gz_write_vtk_header_mpi(id_fld, ioff_gl,                     &
     &    field_num_buffer(num_field))
      call gz_write_vtk_header_mpi                                      &
     &   (id_fld, ioff_gl, field_comp_buffer(num_field, ncomp_field))
!
      icou = 1
      do j = 1, num_field
        call gz_write_vtk_header_mpi                                    &
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
      subroutine gz_write_fld_vecotr_mpi(id_vtk, ioff_gl,               &
     &          nnod, ncomp, vect)
!
      use field_data_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nnod, ncomp
      real(kind = kreal), intent(in) :: vect(nnod,ncomp)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
      real(kind = kreal) :: v1(ncomp)
      integer(kind = kint) :: ip, ilen_gz, ilen_gzipped
      integer(kind = kint) :: ilen_gzipped_list(nprocs)
      integer(kind = kint_gl) :: inod
!
!
      v1(1:ncomp) = 0.0d0
      ilength = len(each_field_data_buffer(ncomp, v1))
      ilen_gz = int(real(nnod*ilength) * 1.01) + 24
      allocate(gzip_buf(ilen_gz))
      if(nnod .eq. 1) then
        v1(1:ncomp) = vect(1,1:ncomp)
        call gzip_defleat_once(ilength,                                 &
     &      each_field_data_buffer(ncomp, v1),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
!
      else if(nnod .gt. 1) then
        v1(1:ncomp) = vect(1,1:ncomp)
        call gzip_defleat_begin(ilength,                                &
     &      each_field_data_buffer(ncomp, v1),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
        do inod = 2, nnod-1
          v1(1:ncomp) = vect(inod,1:ncomp)
          call gzip_defleat_cont(ilength,                               &
     &      each_field_data_buffer(ncomp, v1),                          &
     &      ilen_gz, ilen_gzipped)
        end do
        v1(1:ncomp) = vect(nnod,1:ncomp)
        call gzip_defleat_last(ilength,                                 &
     &      each_field_data_buffer(ncomp, v1),                          &
     &      ilen_gz, ilen_gzipped)
      else
        ilen_gzipped = 0
      end if
!
      call MPI_Allgather(ilen_gzipped, ione, CALYPSO_INTEGER,           &
     &    ilen_gzipped_list(1), ione, CALYPSO_INTEGER,                  &
     &    CALYPSO_COMM, ierr_MPI)
      ioffset = int(ioff_gl)
      do ip = 1, my_rank
        ioffset = ioffset + ilen_gzipped_list(ip)
      end do
!
      if(ilen_gzipped .gt. 0) then
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, gzip_buf(1), ilen_gzipped,          &
     &                      CALYPSO_CHARACTER, sta1, ierr_MPI)
      end if
      do ip = 1, nprocs
        ioff_gl = ioff_gl + ilen_gzipped_list(ip)
      end do
      deallocate(gzip_buf)
!
      end subroutine gz_write_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_header_gz_mpi(num_field, istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use skip_gz_comment
!
      integer(kind=kint), intent(inout) :: num_field
      integer(kind = kint_gl), intent(inout) :: istack_merged(0:nprocs)
!
!
      istack_merged(0)  = 0
      call get_one_line_from_gz_f
      call get_one_line_from_gz_f
      read(textbuf,*)  istack_merged(1:nprocs)
!
      call skip_gz_comment_int(num_field)
!
      end subroutine read_field_header_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_num_gz_mpi(num_field, ncomp_field)
!
      use skip_gz_comment
!
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
!
!
      call get_one_line_from_gz_f
      read(textbuf,*)  ncomp_field(1:num_field)
!
      end subroutine read_field_num_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_data_gz_mpi(nnod, num_field, ntot_comp,     &
     &          ncomp_field, field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use field_data_IO
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
        call get_one_line_from_gz_f
        read(textbuf,*) field_name(j)
!
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
      subroutine read_fld_vecotr_gz_mpi                                 &
     &         (nnod, ncomp, vect, istack_merged)
!
      use field_data_IO
      use skip_gz_comment
!
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: vect(nnod,ncomp)
!
      integer(kind = kint_gl) :: inod
!
!
      do inod = 1, istack_merged(my_rank)
        call get_one_line_from_gz_f
      end do
      do inod = 1, nnod
        call get_one_line_from_gz_f
        read(textbuf,*)  vect(inod,1:ncomp)
      end do
      do inod = istack_merged(my_rank+1)+1, istack_merged(nprocs)
        call get_one_line_from_gz_f
      end do
!
      end subroutine read_fld_vecotr_gz_mpi
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
      end module gz_field_file_MPI_IO
