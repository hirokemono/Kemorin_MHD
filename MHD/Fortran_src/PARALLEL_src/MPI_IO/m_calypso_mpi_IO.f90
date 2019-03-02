!>@file  m_calypso_mpi_IO.f90
!!       module m_calypso_mpi_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Base routines for MPI-IO
!!
!!@verbatim
!!      subroutine deallocate_mpi_IO_status
!!
!!      subroutine calypso_mpi_write_file_open                          &
!!     &         (file_name, nprocs_in, id_mpi_file)
!!      subroutine calypso_mpi_read_file_open(file_name, id_mpi_file)
!!      subroutine calypso_close_mpi_file(id_mpi_file)
!!
!!      subroutine calypso_mpi_seek_write_endian(id_mpi_file, ioff_gl)
!!      subroutine calypso_mpi_seek_write_chara                         &
!!     &         (id_mpi_file, ioffset, ilength, textbuf)
!!      subroutine calypso_mpi_seek_wrt_mul_chara                       &
!!     &         (id_mpi_file, ioffset, ilength, nline, textbuf)
!!      subroutine calypso_mpi_seek_write_real                          &
!!     &         (id_mpi_file, ioffset, ilength, vector)
!!      subroutine calypso_mpi_seek_write_int                           &
!!     &         (id_mpi_file, ioffset, ilength, int_vector)
!!      subroutine calypso_mpi_seek_write_int8                          &
!!     &         (id_mpi_file, ioffset, ilength, i8_vector)
!!
!!      subroutine calypso_mpi_seek_write_head_c                        &
!!     &         (id_mpi_file, ioff_gl, textbuf)
!!!
!!      subroutine calypso_mpi_seek_read_endian                         &
!!     &         (id_mpi_file, iflag_bin_swap, ioff_gl)
!!      subroutine calypso_mpi_seek_read_lenchara                       &
!!     &         (id_mpi_file, ioffset, ilength, charabuf)
!!      function calypso_mpi_seek_read_chara                            &
!!     &       (id_mpi_file, ioffset, ilength)
!!        character(len=ilength) :: calypso_mpi_seek_read_chara
!!      subroutine calypso_mpi_seek_read_mul_chara                      &
!!     &         (id_mpi_file, ioffset, ilength, nline, textbuf)
!!      subroutine calypso_mpi_seek_read_real(id_mpi_file,              &
!!     &          iflag_bin_swap, ioffset, ilength, vector)
!!      subroutine calypso_mpi_seek_read_int(id_mpi_file,               &
!!     &          iflag_bin_swap, ioffset, ilength, int_vector)
!!      subroutine calypso_mpi_seek_read_int8(id_mpi_file,              &
!!     &          iflag_bin_swap, ioffset, ilength, i8_vector)
!!
!!      subroutine calypso_mpi_seek_read_gz                             &
!!     &         (id_mpi_file, ioffset, ilength, c1buf)
!!      subroutine calypso_mpi_seek_long_write_gz                       &
!!     &         (id_mpi_file, ioffset, zbuf)
!!      subroutine calypso_mpi_seek_long_read_gz                        &
!!     &         (id_mpi_file, ioffset, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine calypso_gz_mpi_seek_write(id_mpi_file, ioff_gl, zbuf)
!!        type(buffer_4_gzip), intent(in) :: zbuf
!!    calypso_gz_mpi_seek_write only work correctly when number of 
!!   subdomain is equal to number of threads
!!@endverbatim
!
      module m_calypso_mpi_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
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
      subroutine calypso_mpi_write_file_open                            &
     &         (file_name, nprocs_in, id_mpi_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in
      integer, intent(inout) ::  id_mpi_file
!
      integer(kind = MPI_OFFSET_KIND), parameter :: zerosize = 0
!
!
      call init_mpi_IO_status
      call MPI_FILE_OPEN(CALYPSO_COMM, file_name,                       &
     &    MPI_MODE_RDWR+MPI_MODE_APPEND+MPI_MODE_CREATE,                &
     &    MPI_INFO_NULL, id_mpi_file, ierr_MPI)
!
     if(nprocs_in .le. nprocs) then
       call MPI_FILE_SET_SIZE(id_mpi_file, zerosize, ierr_MPI)
     end if
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
      call calypso_mpi_barrier
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
      call calypso_mpi_barrier
      call deallocate_mpi_IO_status
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
     &    CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
      ioffset = ioffset + ilength
!
      end subroutine calypso_mpi_seek_write_chara
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_wrt_mul_chara                         &
     &         (id_mpi_file, ioffset, ilength, nline, textbuf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      integer(kind = kint_gl), intent(in) :: nline
      character(len=ilength), intent(in) :: textbuf(nline)
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
!
      integer(kind = kint) :: ntot
!
!
      ntot = ilength * int(nline)
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_mpi_file, textbuf, ntot,                   &
     &      CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
      ioffset = ioffset + ntot
!
      end subroutine calypso_mpi_seek_wrt_mul_chara
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_real                            &
     &         (id_mpi_file, ioffset, ilength, vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      real(kind = kreal), intent(in) :: vector(ilength)
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_mpi_file, vector, ilength,                 &
     &    CALYPSO_REAL, sta1_IO, ierr_MPI)
!
      end subroutine calypso_mpi_seek_write_real
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_int                             &
     &         (id_mpi_file, ioffset, ilength, int_vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      integer(kind = kint), intent(in) :: int_vector(ilength)
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_mpi_file, int_vector, ilength,             &
     &    CALYPSO_INTEGER, sta1_IO, ierr_MPI)
!
      end subroutine calypso_mpi_seek_write_int
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_int8                            &
     &         (id_mpi_file, ioffset, ilength, i8_vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      integer(kind = kint_gl), intent(in) :: i8_vector(ilength)
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_mpi_file, i8_vector, ilength,              &
     &    CALYPSO_GLOBAL_INT, sta1_IO, ierr_MPI)
!
      end subroutine calypso_mpi_seek_write_int8
!
!  ---------------------------------------------------------------------
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
        ioffset = ioff_gl
        call calypso_mpi_seek_write_chara                               &
     &     (id_mpi_file, ioffset, ilength, textbuf)
      end if
      ioff_gl = ioff_gl + ilength
!
      end subroutine calypso_mpi_seek_write_head_c
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_endian(id_mpi_file, ioff_gl)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = kint) :: int_vector(1)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        int_vector(1) = i_UNIX
        ioffset = ioff_gl
        call calypso_mpi_seek_write_int                                 &
     &         (id_mpi_file, ioffset, ione, int_vector)
      end if
      ioff_gl = ioff_gl + kint
!
      end subroutine calypso_mpi_seek_write_endian
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_endian                           &
     &         (id_mpi_file, iflag_bin_swap, ioff_gl)
!
      use m_error_IDs
      use binary_IO
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(inout) :: iflag_bin_swap
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: int_vector(1)
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call MPI_FILE_SEEK                                              &
     &     (id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_mpi_file, int_vector, ione,               &
     &      CALYPSO_INTEGER, sta1_IO, ierr_MPI)
!
        iflag_bin_swap = endian_check(my_rank, int_vector(1))
      end if
      ioff_gl = ioff_gl + kint
!
      call MPI_BCAST(iflag_bin_swap, ione, CALYPSO_INTEGER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)

      end subroutine calypso_mpi_seek_read_endian
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_lenchara                         &
     &         (id_mpi_file, ioffset, ilength, charabuf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      integer(kind = kint), intent(in) :: ilength
      character(len=ilength), intent(inout) :: charabuf
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, charabuf, ilength,                &
     &      CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
      ioffset = ioffset + ilength
!
      end subroutine calypso_mpi_seek_read_lenchara
!
!  ---------------------------------------------------------------------
!
      function calypso_mpi_seek_read_chara                              &
     &       (id_mpi_file, ioffset, ilength)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      integer(kind = kint), intent(in) :: ilength
      character(len=ilength) :: calypso_mpi_seek_read_chara
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, calypso_mpi_seek_read_chara,      &
     &    ilength, CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
      ioffset = ioffset + ilength
!
      end function calypso_mpi_seek_read_chara
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_mul_chara                        &
     &         (id_mpi_file, ioffset, ilength, nline, textbuf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer(kind = kint), intent(in) :: ilength
      integer(kind = kint_gl), intent(in) :: nline
      character(len=ilength), intent(in) :: textbuf(nline)
!
      integer(kind = kint) :: ilen_in
      integer(kind = kint_gl) :: l8_byte, ist
!
!
      ist = 0
      l8_byte = ioffset
      do
        ilen_in = int(min((nline-ist), huge_25/ilength))
        call MPI_FILE_SEEK                                              &
     &     (id_mpi_file, l8_byte, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_mpi_file, textbuf(ist+1),                 &
     &       (ilen_in*ilength), CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
        ist = ist + ilen_in
        l8_byte = l8_byte + ilen_in*ilength
        if(ist .ge. nline) exit
      end do
!
      end subroutine calypso_mpi_seek_read_mul_chara
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_real(id_mpi_file,                &
     &          iflag_bin_swap, ioffset, ilength, vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: ilength
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      real(kind = kreal), intent(inout) :: vector(ilength)
!
      integer(kind = kint_gl) :: l8_byte
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, vector, ilength,                  &
     &    CALYPSO_REAL, sta1_IO, ierr_MPI)
!
      if(iflag_bin_swap .eq. iendian_FLIP) then
        l8_byte = ilength * kreal
        call byte_swap_64bit_f(l8_byte, vector(1))
      end if
!
      end subroutine calypso_mpi_seek_read_real
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_int(id_mpi_file,                 &
     &          iflag_bin_swap, ioffset, ilength, int_vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: ilength
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      integer(kind = kint), intent(inout) :: int_vector(ilength)
!
      integer(kind = kint_gl) :: l8_byte
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, int_vector, ilength,              &
     &    CALYPSO_INTEGER, sta1_IO, ierr_MPI)
!
      if(iflag_bin_swap .eq. iendian_FLIP) then
        l8_byte = ilength * kint
        call byte_swap_32bit_f(l8_byte, int_vector(1))
      end if
!
      end subroutine calypso_mpi_seek_read_int
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_int8(id_mpi_file,                &
     &          iflag_bin_swap, ioffset, ilength, i8_vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: iflag_bin_swap
      integer(kind = kint), intent(in) :: ilength
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      integer(kind = kint_gl), intent(inout) :: i8_vector(ilength)
!
      integer(kind = kint_gl) :: l8_byte
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, i8_vector, ilength,              &
     &    CALYPSO_GLOBAL_INT, sta1_IO, ierr_MPI)
!
      if(iflag_bin_swap .eq. iendian_FLIP) then
        l8_byte = ilength * kint_gl
        call byte_swap_64bit_f(l8_byte, i8_vector(1))
      end if
!
      end subroutine calypso_mpi_seek_read_int8
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_gz                               &
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
      end subroutine calypso_mpi_seek_read_gz
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_long_write_gz                         &
     &         (id_mpi_file, ioffset, zbuf)
!
      use t_buffer_4_gzip
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      type(buffer_4_gzip), intent(in) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer(kind = kint) :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(zbuf%ilen_gzipped-ist, huge_25))
        call MPI_FILE_SEEK                                              &
     &     (id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_mpi_file, zbuf%gzip_buf(ist+1), ilen_in, &
     &      CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
        ist = ist + ilen_in
        ioffset = ioffset + ilen_in
        if(ist .ge. zbuf%ilen_gzipped) exit
      end do
!
      end subroutine calypso_mpi_seek_long_write_gz
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_long_read_gz                          &
     &         (id_mpi_file, ioffset, zbuf)
!
      use t_buffer_4_gzip
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer(kind = kint) :: ilen_in
!
!
      call alloc_zip_buffer(zbuf)
!
      ist = 0
      do
        ilen_in = int(min(zbuf%ilen_gz-ist, huge_25))
        call MPI_FILE_SEEK                                              &
     &     (id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_mpi_file, zbuf%gzip_buf(ist+1), ilen_in,  &
     &      CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
        ist = ist + ilen_in
        ioffset = ioffset + ilen_in
        if(ist .ge. zbuf%ilen_gz) exit
      end do
!
      end subroutine calypso_mpi_seek_long_read_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_gz_mpi_seek_write(id_mpi_file, ioff_gl, zbuf)
!
      use t_buffer_4_gzip
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(buffer_4_gzip), intent(in) :: zbuf
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: ilen_gzipped_gl(nprocs)
      integer(kind = kint) :: ip
!
!
      call MPI_Allgather(zbuf%ilen_gzipped, ione, CALYPSO_GLOBAL_INT,   &
     &    ilen_gzipped_gl, ione, CALYPSO_GLOBAL_INT, CALYPSO_COMM,      &
     &    ierr_MPI)
!
      if(zbuf%ilen_gzipped .gt. 0) then
        ioffset = ioff_gl
        do ip = 1, my_rank
          ioffset = ioffset + ilen_gzipped_gl(ip)
        end do
        call calypso_mpi_seek_long_write_gz                             &
     &    (id_mpi_file, ioffset, zbuf)
      end if
!
      do ip = 1, nprocs
        ioff_gl = ioff_gl + ilen_gzipped_gl(ip)
      end do
!
      end subroutine calypso_gz_mpi_seek_write
!
!  ---------------------------------------------------------------------
!
      end module m_calypso_mpi_IO
