!>@file  m_calypso_mpi_IO.f90
!!       module m_calypso_mpi_IO
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Base routines for MPI-IO
!!
!!@verbatim
!!      subroutine deallocate_mpi_IO_status
!!
!!      subroutine calypso_mpi_write_file_open(file_name, id_mpi_file)
!!      subroutine calypso_mpi_read_file_open(file_name, id_mpi_file)
!!      subroutine calypso_close_mpi_file(id_mpi_file)
!!
!!      subroutine calypso_mpi_seek_write_chara                         &
!!     &         (id_mpi_file, ioffset, ilength, textbuf)
!!      subroutine calypso_mpi_seek_write_head_c                        &
!!     &         (id_mpi_file, ioff_gl, textbuf)
!!
!!      subroutine calypso_gz_mpi_seek_write                            &
!!     &         (id_mpi_file, ioff_gl, ilen_gzipped, gzip_buf)
!!
!!      subroutine calypso_mpi_seek_read_chara                          &
!!     &         (id_mpi_file, ioffset, ilength, c1buf)
!!@endverbatim
!
      module m_calypso_mpi_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
!>       status flag for sending
      integer, allocatable :: sta1_IO(:)
!
      private :: init_mpi_IO_status
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_mpi_IO_status
!
      if (allocated(sta1_IO)) return
      allocate(sta1_IO(MPI_STATUS_SIZE))
!
      end subroutine init_mpi_IO_status
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_mpi_IO_status
!
      if (allocated(sta1_IO)) deallocate(sta1_IO)
!
      end subroutine deallocate_mpi_IO_status
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_write_file_open(file_name, id_mpi_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(inout) ::  id_mpi_file
!
!
      call init_mpi_IO_status
      call MPI_FILE_OPEN                                                &
     &   (CALYPSO_COMM, file_name, MPI_MODE_WRONLY+MPI_MODE_CREATE,     &
     &    MPI_INFO_NULL, id_mpi_file, ierr_MPI)
!
      end subroutine calypso_mpi_write_file_open
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_read_file_open(file_name, id_mpi_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(inout) ::  id_mpi_file
!
!
      call init_mpi_IO_status
      call MPI_FILE_OPEN                                                &
     &   (CALYPSO_COMM, file_name, MPI_MODE_RDONLY,                     &
     &    MPI_INFO_NULL, id_mpi_file, ierr_MPI)
!
      end subroutine calypso_mpi_read_file_open
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_close_mpi_file(id_mpi_file)
!
      integer, intent(in) ::  id_mpi_file
!
!
      call MPI_FILE_CLOSE(id_mpi_file, ierr_MPI)
!
      end subroutine calypso_close_mpi_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_chara                           &
     &         (id_mpi_file, ioffset, ilength, textbuf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      character(len=ilength), intent(in) :: textbuf
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_mpi_file, textbuf, ilength,                &
     &      CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
        ioffset = ioffset + ilength
!
      end subroutine calypso_mpi_seek_write_chara
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_head_c                          &
     &         (id_mpi_file, ioff_gl, textbuf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=*), intent(in) :: textbuf
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      ilength = len(textbuf)
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_write_chara                               &
     &     (id_mpi_file, ioffset, ilength, textbuf)
      end if
      ioff_gl = ioff_gl + ilength
!
      end subroutine calypso_mpi_seek_write_head_c
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_gz_mpi_seek_write                              &
     &         (id_mpi_file, ioff_gl, ilen_gzipped, gzip_buf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: ilen_gzipped
      character(len=1), intent(in) :: gzip_buf(ilen_gzipped)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gzipped_list(nprocs)
      integer(kind = kint) :: ip
!
!
      call MPI_Allgather(ilen_gzipped, ione, CALYPSO_INTEGER,           &
     &    ilen_gzipped_list(1), ione, CALYPSO_INTEGER,                  &
     &    CALYPSO_COMM, ierr_MPI)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = ioff_gl
        do ip = 1, my_rank
          ioffset = ioffset + ilen_gzipped_list(ip)
        end do
        call calypso_mpi_seek_write_chara                               &
     &    (id_mpi_file, ioffset, ilen_gzipped, gzip_buf(1))
      end if
      do ip = 1, nprocs
        ioff_gl = ioff_gl + ilen_gzipped_list(ip)
      end do
!
      end subroutine calypso_gz_mpi_seek_write
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_chara                            &
     &         (id_mpi_file, ioffset, ilength, c1buf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      integer(kind = kint), intent(in) :: ilength
      character(len=1), intent(inout) :: c1buf(ilength)
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, c1buf(1), ilength,                &
     &      CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
      ioffset = ioffset + ilength
!
      end subroutine calypso_mpi_seek_read_chara
!
!  ---------------------------------------------------------------------
!
      end module m_calypso_mpi_IO
