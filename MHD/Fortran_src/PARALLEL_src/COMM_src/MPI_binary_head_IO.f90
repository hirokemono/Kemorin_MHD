!>@file  MPI_binary_head_IO.f90
!!       module MPI_binary_head_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine open_write_mpi_file_b                                &
!!     &         (file_name, nprocs_in, id_file, ioff_gl)
!!      subroutine open_read_mpi_file_b(file_name, id_file, ioff_gl)
!!
!!      subroutine mpi_write_one_inthead_b(id_file, ioff_gl, int_dat)
!!      subroutine mpi_write_one_realhead_b(id_file, ioff_gl, real_dat)
!!      subroutine mpi_write_one_integer_b(id_file,                     &
!!     &          nprocs_in, id_rank, ioff_gl, int_dat)
!!
!!      subroutine mpi_write_mul_inthead_b                              &
!!     &         (id_file, ioff_gl, num, int_dat)
!!        Substittion of gz_write_mul_integer_b
!!      subroutine mpi_write_mul_int8head_b                             &
!!     &         (id_file, ioff_gl, num, int8_dat)
!!        Substittion of gz_write_mul_int8_b
!!      subroutine mpi_write_mul_charahead_b                            &
!!     &         (id_file, ioff_gl, num, chara_dat)
!!       Substittion of gz_write_mul_character_b
!!      subroutine mpi_write_mul_realhead_b                             &
!!     &         (id_file, ioff_gl, num, real_dat)
!!
!!      subroutine mpi_read_one_inthead_b(id_file, ioff_gl, int_dat)
!!      subroutine mpi_read_one_realhead_b(id_file, ioff_gl, real_dat)
!!      subroutine mpi_read_one_integer_b(id_file,                      &
!!     &          nprocs_in, id_rank, ioff_gl, int_dat)
!!
!!      subroutine mpi_read_mul_inthead_b                               &
!!     &         (id_file, ioff_gl, num, int_dat)
!!        Substittion of gz_read_mul_integer_b
!!      subroutine mpi_read_mul_int8head_b                              &
!!     &         (id_file, ioff_gl, num, int_dat)
!!        Substittion of gz_read_mul_int8_b
!!      subroutine mpi_read_mul_charahead_b                             &
!!     &         (id_file, ioff_gl, num, chara_dat)
!!        Substittion of gz_read_mul_character_b
!!      subroutine mpi_read_mul_realhead_b                              &
!!     &         (id_file, ioff_gl, num, real_dat)
!!@endverbatim
!
      module MPI_binary_head_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use MPI_binary_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_write_mpi_file_b                                  &
     &         (file_name, nprocs_in, id_file, ioff_gl)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in
!
      integer, intent(inout) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call calypso_mpi_write_file_open                                  &
     &   (file_name, nprocs_in, id_file)
!
      ioff_gl = izero
      call calypso_mpi_seek_write_endian(id_file, ioff_gl)
!
      end subroutine open_write_mpi_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine open_read_mpi_file_b(file_name, id_file, ioff_gl)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(inout) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call calypso_mpi_read_file_open(file_name, id_file)
!
      ioff_gl = izero
      call calypso_mpi_seek_read_endian(id_file, ioff_gl)
!
      end subroutine open_read_mpi_file_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_one_inthead_b(id_file, ioff_gl, int_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: int_dat
!
      integer(kind = kint) :: itmp_IO(1)
!
!
      itmp_IO(1) = int_dat
      call mpi_write_mul_inthead_b(id_file, ioff_gl, ione, itmp_IO)
!
      end subroutine mpi_write_one_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_one_realhead_b(id_file, ioff_gl, real_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      real(kind = kreal), intent(in) :: real_dat
!
      real(kind = kreal) :: rtmp_IO(1)
!
!
      rtmp_IO(1) = real_dat
      call mpi_write_mul_realhead_b                                     &
     &    (id_file, ioff_gl, ione, rtmp_IO)
!
      end subroutine mpi_write_one_realhead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_one_integer_b(id_file,                       &
     &          nprocs_in, id_rank, ioff_gl, int_dat)
!
      integer, intent(in) ::  id_file
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer(kind = kint), intent(in) :: int_dat
!
      integer(kind = kint_gl) :: istack_merged(0:nprocs_in)
      integer(kind = kint) :: iemp_IO(1)
      integer(kind = kint) :: i
!
!
      do i = 0, nprocs_in
        istack_merged(i) = i
      end do
      iemp_IO(1) = int_dat
!
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank,          &
     &    ioff_gl, ione, iemp_IO, istack_merged)
!
      end subroutine mpi_write_one_integer_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_mul_inthead_b                                &
     &         (id_mpi_file, ioff_gl, num, int_dat)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_write_int                                 &
     &         (id_mpi_file, ioffset, num, int_dat)
      end if
      ioff_gl = ioff_gl + num * kint
!
      end subroutine mpi_write_mul_inthead_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_mul_int8head_b                               &
     &         (id_file, ioff_gl, num, int8_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_write_int8                                &
     &     (id_file, ioffset, num, int8_dat)
      end if
      ioff_gl = ioff_gl + num * kint_gl
!
      end subroutine mpi_write_mul_int8head_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_mul_charahead_b                              &
     &         (id_file, ioff_gl, num, chara_dat)
!
      integer, intent(in) ::  id_file
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      integer(kind = kint) :: ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength = num * kchara
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_write_chara                               &
     &     (id_file, ioffset, ilength, chara_dat(1))
      end if
      ioff_gl = ioff_gl + ilength
!
      end subroutine mpi_write_mul_charahead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_mul_realhead_b                               &
     &         (id_file, ioff_gl, num, real_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      real(kind = kreal), intent(in) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_write_real                                &
     &     (id_file, ioffset, num, real_dat)
      end if
      ioff_gl = ioff_gl + num * kreal
!
      end subroutine mpi_write_mul_realhead_b
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_one_inthead_b(id_file, ioff_gl, int_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint) :: itmp_IO(1)
!
!
      call mpi_read_mul_inthead_b(id_file, ioff_gl, ione, itmp_IO)
      int_dat = itmp_IO(1)
!
      end subroutine mpi_read_one_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_one_realhead_b(id_file, ioff_gl, real_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      real(kind = kreal), intent(inout) :: real_dat
!
      real(kind = kreal) ::   rtmp_IO(1)
!
!
      call mpi_read_mul_realhead_b(id_file, ioff_gl, ione, rtmp_IO)
      real_dat = rtmp_IO(1)
!
      end subroutine mpi_read_one_realhead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_one_integer_b(id_file,                        &
     &          nprocs_in, id_rank, ioff_gl, int_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint_gl) :: istack_merged(0:nprocs_in)
      integer(kind = kint) :: iemp_IO(1)
      integer(kind = kint) :: i
!
!
      do i = 0, nprocs_in
        istack_merged(i) = i
      end do
!
      call mpi_read_int_vector_b(id_file, nprocs_in, id_rank,           &
     &    ioff_gl, ione, iemp_IO(1), istack_merged)
      int_dat = iemp_IO(1)
!
      end subroutine mpi_read_one_integer_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_mul_inthead_b                                 &
     &         (id_file, ioff_gl, num, int_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_read_int                                  &
     &     (id_file, ioffset, num, int_dat(1))
      end if
      ioff_gl = ioff_gl + num*kint
!
      call MPI_BCAST(int_dat, num, CALYPSO_INTEGER, izero,              &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine mpi_read_mul_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_mul_int8head_b                                &
     &         (id_file, ioff_gl, num, int8_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_read_int8                                 &
     &     (id_file, ioffset, num, int8_dat(1))
      end if
      ioff_gl = ioff_gl + num*kint_gl
!
      call MPI_BCAST(int8_dat, num, CALYPSO_GLOBAL_INT, izero,          &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine mpi_read_mul_int8head_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_mul_charahead_b                               &
     &         (id_file, ioff_gl, num, chara_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      ilength = num * kchara
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_read_chara                                &
     &         (id_file, ioffset, ilength, chara_dat(1))
      end if
      ioff_gl = ioff_gl + ilength
!
      call MPI_BCAST(chara_dat, ilength, CALYPSO_CHARACTER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine mpi_read_mul_charahead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_mul_realhead_b                                &
     &         (id_file, ioff_gl, num, real_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_read_real                                 &
     &     (id_file, ioffset, num, real_dat(1))
      end if
      ioff_gl = ioff_gl + num*kreal
!
      call MPI_BCAST(real_dat, num, CALYPSO_REAL, izero,                &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine mpi_read_mul_realhead_b
!
! -----------------------------------------------------------------------
!
      end module MPI_binary_head_IO
