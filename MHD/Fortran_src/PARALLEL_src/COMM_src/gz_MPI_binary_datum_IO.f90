!>@file  gz_MPI_binary_datum_IO.f90
!!       module gz_MPI_binary_datum_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_mpi_write_endian_flag(id_fld, ioff_gl)
!!        Substitution of  gz_write_endian_flag
!!      subroutine gz_mpi_write_one_inthead_b(id_fld, ioff_gl, int_dat)
!!        Substitution of gz_write_one_integer_b
!!      subroutine gz_mpi_write_one_realhead_b                          &
!!     &         (id_fld, ioff_gl, real_dat)
!!        Substitution of gz_write_one_real_b
!!
!!      subroutine gz_mpi_read_endian_flag(id_fld, ioff_gl)
!!        Substitution of gz_read_endian_flag
!!      subroutine gz_mpi_read_one_inthead_b(id_fld, ioff_gl, int_dat)
!!        Substitution of gz_read_one_integer_b
!!      subroutine gz_mpi_read_one_realhead_b                           &
!!     &         (id_fld, ioff_gl, real_dat)
!!        Substitution of gz_read_one_real_b
!!@endverbatim
!
      module gz_MPI_binary_datum_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_calypso_mpi_IO
!
      implicit none
!
      character(len=1), allocatable, private :: gzip_buf(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_endian_flag(id_fld, ioff_gl)
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call gz_mpi_write_one_inthead_b(id_fld, ioff_gl, i_UNIX)
!
      end subroutine gz_mpi_write_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_one_inthead_b(id_fld, ioff_gl, int_dat)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: int_dat
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ilength = ione * kint
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once                                          &
     &     (ilength, int_dat, ilen_gz, ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = ioff_gl
        call calypso_mpi_seek_write_chara                               &
     &     (id_fld, ioffset, ilen_gzipped, gzip_buf(1))
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_write_one_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_one_realhead_b                            &
     &         (id_fld, ioff_gl, real_dat)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      real(kind = kreal), intent(in) :: real_dat
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ilength = ione * kreal
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once                                          &
     &     (ilength, real_dat, ilen_gz, ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = ioff_gl
        call calypso_mpi_seek_write_chara                               &
     &     (id_fld, ioffset, ilen_gzipped, gzip_buf(1))
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_write_one_realhead_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_endian_flag(id_fld, ioff_gl)
!
      use m_error_IDs
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = kint) :: int_dat(1)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        ilength = kint
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_chara                                &
     &         (id_fld, ioffset, ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once                                          &
     &     (ilen_gz, gzip_buf(1), ilength, int_dat, ilen_gzipped)
        deallocate(gzip_buf)
!
        if(int_dat(1) .eq. i_UNIX) then
          write(*,*) 'binary data have correct endian!'
          iflag_endian = iendian_KEEP
        else if(int_dat(1) .eq. i_XINU) then
          write(*,*) 'binary data have opposite endian!'
          iflag_endian = iendian_FLIP
        else
          iflag_endian = -1
          call calypso_MPI_abort                                        &
     &       (ierr_fld,'Binary Data is someting wrong!')
        end if
      end if
!
      call MPI_BCAST(iflag_endian, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_read_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_one_inthead_b(id_fld, ioff_gl, int_dat)
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
      integer(kind = kint_gl) :: l8_byte
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        ilength = ione * kint
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_chara                                &
     &     (id_fld, ioffset, ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once                                          &
     &     (ilen_gz, gzip_buf(1), ilength, int_dat, ilen_gzipped)
        deallocate(gzip_buf)
!
        if(iflag_endian .eq. iendian_FLIP) then
          l8_byte = ilength
          call byte_swap_f(l8_byte, int_dat)
        end if
      end if
!
      call MPI_BCAST(int_dat, ione, CALYPSO_INTEGER, izero,             &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_read_one_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_one_realhead_b                             &
     &         (id_fld, ioff_gl, real_dat)
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      real(kind = kreal), intent(inout) :: real_dat
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
      integer(kind = kint_gl) :: l8_byte
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        ilength = ione * kreal
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_chara                                &
     &     (id_fld, ioffset, ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once                                          &
     &     (ilen_gz, gzip_buf(1), ilength, real_dat, ilen_gzipped)
        deallocate(gzip_buf)
!
        if(iflag_endian .eq. iendian_FLIP) then
          l8_byte = ilength
          call byte_swap_f(l8_byte, real_dat)
        end if
      end if
!
      call MPI_BCAST(real_dat, ione, CALYPSO_REAL, izero,               &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_read_one_realhead_b
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_binary_datum_IO
