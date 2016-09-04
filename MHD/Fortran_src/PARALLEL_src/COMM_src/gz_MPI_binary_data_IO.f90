!>@file  gz_MPI_binary_data_IO.f90
!!       module gz_MPI_binary_data_IO
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief Output gzipped merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_mpi_write_integer_stack_b                         &
!!     &         (id_file, ioff_gl, num, istack)
!!        Substitution of gz_write_integer_stack_b
!!      subroutine gz_mpi_write_int_vector_b                            &
!!     &         (id_file, ioff_gl, num, int_dat)
!!        Substitutio of gz_write_mul_integer_b
!!      subroutine gz_mpi_write_int8_vector_b                           &
!!     &         (id_file, ioff_gl, num, int8_dat)
!!        Substitutio of gz_write_mul_int8_b
!!      subroutine gz_mpi_write_1d_vector_b                             &
!!     &         (id_file, ioff_gl, num, real_dat)
!!        Substitutio of gz_write_1d_vector_b
!!      subroutine gz_mpi_write_2d_vector_b                             &
!!     &         (id_file, ioff_gl, n1, n2, real_dat)
!!        Substitutio of gz_write_2d_vector_b
!!
!!      subroutine gz_mpi_read_integer_stack_b(id_file,                 &
!!     &          nprocs_in, id_rank, ioff_gl, num, istack, ntot)
!!        Substittion of  gz_read_integer_stack_b
!!      subroutine gz_mpi_read_int_vector_b                             &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, num, int_dat)
!!        Substitutio of gz_read_mul_integer_b
!!      subroutine gz_mpi_read_int8_vector_b                            &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, num, int8_dat)
!!        Substitutio of gz_read_mul_int8_b
!!      subroutine gz_mpi_read_1d_vector_b                              &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, num, real_dat)
!!        Substitutio of gz_read_1d_vector_b
!!      subroutine gz_mpi_read_2d_vector_b(id_file,                     &
!!     &          nprocs_in, id_rank, ioff_gl, n1, n2, real_dat)
!!        Substitutio of gz_read_2d_vector_b
!!@endverbatim
!
      module gz_MPI_binary_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use m_calypso_mpi_IO
      use gz_MPI_binary_head_IO
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
      subroutine gz_mpi_write_integer_stack_b                           &
     &         (id_file, ioff_gl, num, istack)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: istack(0:num)
!
!
      call gz_mpi_write_int_vector_b(id_file, ioff_gl, num, istack(1))
!
      end subroutine gz_mpi_write_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int_vector_b                              &
     &         (id_file, ioff_gl, num, int_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength, ip
      integer(kind = kint) :: ilen_gzipped_gl(nprocs)
      integer(kind = kint_gl) :: istack_buffer(0:nprocs)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength =  num * kint
      ilen_gz = int(real(ilength) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
      call gzip_defleat_once                                            &
     &   (ilength, int_dat(1), ilen_gz, ilen_gzipped, gzip_buf(1))
!
      call MPI_Allgather(ilen_gzipped, ione, CALYPSO_INTEGER,           &
     &    ilen_gzipped_gl, ione, CALYPSO_INTEGER, CALYPSO_COMM,         &
     &    ierr_MPI)
!
      istack_buffer(0) = 0
      do ip = 1, nprocs
        istack_buffer(ip) = istack_buffer(ip-1) + ilen_gzipped_gl(ip)
      end do
!
      call gz_mpi_write_mul_int8head_b                                  &
     &   (id_file, ioff_gl, nprocs, istack_buffer(1))
!
      if(ilen_gzipped .gt. 0) then
        ioffset = ioff_gl + istack_buffer(my_rank)
        call calypso_mpi_seek_write_chara                               &
     &    (id_file, ioffset, ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      ioff_gl = ioff_gl + istack_buffer(nprocs)
!
      end subroutine gz_mpi_write_int_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int8_vector_b                             &
     &         (id_file, ioff_gl, num, int8_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength, ip
      integer(kind = kint) :: ilen_gzipped_gl(nprocs)
      integer(kind = kint_gl) :: istack_buffer(0:nprocs)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength =  num * kint_gl
      ilen_gz = int(real(ilength) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
      call gzip_defleat_once                                            &
     &   (ilength, int8_dat(1), ilen_gz, ilen_gzipped, gzip_buf(1))
!
      call MPI_Allgather(ilen_gzipped, ione, CALYPSO_INTEGER,           &
     &    ilen_gzipped_gl, ione, CALYPSO_INTEGER, CALYPSO_COMM,         &
     &    ierr_MPI)
!
      istack_buffer(0) = 0
      do ip = 1, nprocs
        istack_buffer(ip) = istack_buffer(ip-1) + ilen_gzipped_gl(ip)
      end do
!
      call gz_mpi_write_mul_int8head_b                                  &
     &   (id_file, ioff_gl, nprocs, istack_buffer(1))
!
      if(ilen_gzipped .gt. 0) then
        ioffset = ioff_gl + istack_buffer(my_rank)
        call calypso_mpi_seek_write_chara                               &
     &    (id_file, ioffset, ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      ioff_gl = ioff_gl + istack_buffer(nprocs)
!
      end subroutine gz_mpi_write_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_1d_vector_b                               &
     &         (id_file, ioff_gl, num, real_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength, ip
      integer(kind = kint) :: ilen_gzipped_gl(nprocs)
      integer(kind = kint_gl) :: istack_buffer(0:nprocs)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength =  num * kreal
      ilen_gz = int(real(ilength) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
      call gzip_defleat_once                                            &
     &   (ilength, real_dat(1), ilen_gz, ilen_gzipped, gzip_buf(1))
!
      call MPI_Allgather(ilen_gzipped, ione, CALYPSO_INTEGER,           &
     &    ilen_gzipped_gl, ione, CALYPSO_INTEGER, CALYPSO_COMM,         &
     &    ierr_MPI)
!
      istack_buffer(0) = 0
      do ip = 1, nprocs
        istack_buffer(ip) = istack_buffer(ip-1) + ilen_gzipped_gl(ip)
      end do
!
      call gz_mpi_write_mul_int8head_b                                  &
     &   (id_file, ioff_gl, nprocs, istack_buffer(1))
!
      if(ilen_gzipped .gt. 0) then
        ioffset = ioff_gl + istack_buffer(my_rank)
        call calypso_mpi_seek_write_chara                               &
     &    (id_file, ioffset, ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      ioff_gl = ioff_gl + istack_buffer(nprocs)
!
      end subroutine gz_mpi_write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_2d_vector_b                               &
     &         (id_file, ioff_gl, n1, n2, real_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: n1, n2
!
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
!
      integer(kind = kint) :: num
!
!
      num = n1 * n2
      call gz_mpi_write_1d_vector_b                                     &
     &   (id_file, ioff_gl, num, real_dat(1,1))
!
      end subroutine gz_mpi_write_2d_vector_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_integer_stack_b(id_file,                   &
     &          nprocs_in, id_rank, ioff_gl, num, istack, ntot)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
!
!
      istack(0) = 0
      call gz_mpi_read_int_vector_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl, num, istack(1))
      ntot = istack(num)
!
      end subroutine gz_mpi_read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int_vector_b                               &
     &         (id_file, nprocs_in, id_rank, ioff_gl, num, int_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
      integer(kind = kint_gl) :: istack_buffer(0:nprocs_in)
      integer(kind = kint_gl) :: l8_byte
!
!
      istack_buffer(0) = 0
      call gz_mpi_read_mul_int8head_b                                   &
     &   (id_file, ioff_gl, nprocs_in, istack_buffer(1))
!
      if(id_rank .ge. nprocs_in) return
!
      ioffset = ioff_gl + istack_buffer(id_rank)
      ilength = num * kint
      ilen_gz = int(istack_buffer(id_rank+1) - istack_buffer(id_rank))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_chara                                  &
     &         (id_file, ioffset, ilen_gz, gzip_buf(1))
!
      call gzip_infleat_once                                            &
     &   (ilen_gz, gzip_buf(1), ilength, int_dat(1), ilen_gzipped)
      deallocate(gzip_buf)
      ioff_gl = ioff_gl + ilen_gz
!
      if(iflag_endian .eq. iendian_FLIP) then
        l8_byte = ilength
        call byte_swap_f(l8_byte, int_dat(1))
      end if
!
      end subroutine gz_mpi_read_int_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int8_vector_b                              &
     &         (id_file, nprocs_in, id_rank, ioff_gl, num, int8_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
      integer(kind = kint_gl) :: istack_buffer(0:nprocs_in)
      integer(kind = kint_gl) :: l8_byte
!
!
      istack_buffer(0) = 0
      call gz_mpi_read_mul_int8head_b                                   &
     &   (id_file, ioff_gl, nprocs_in, istack_buffer(1))
!
      if(id_rank .ge. nprocs_in) return
!
      ioffset = ioff_gl + istack_buffer(id_rank)
      ilength = num * kint_gl
      ilen_gz = int(istack_buffer(id_rank+1) - istack_buffer(id_rank))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_chara                                  &
     &         (id_file, ioffset, ilen_gz, gzip_buf(1))
!
      call gzip_infleat_once                                            &
     &   (ilen_gz, gzip_buf(1), ilength, int8_dat(1), ilen_gzipped)
      deallocate(gzip_buf)
      ioff_gl = ioff_gl + ilen_gz
!
      if(iflag_endian .eq. iendian_FLIP) then
        l8_byte = ilength
        call byte_swap_f(l8_byte, int8_dat(1))
      end if
!
      end subroutine gz_mpi_read_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_1d_vector_b                                &
     &         (id_file, nprocs_in, id_rank, ioff_gl, num, real_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
      integer(kind = kint_gl) :: istack_buffer(0:nprocs_in)
      integer(kind = kint_gl) :: l8_byte
!
!
      istack_buffer(0) = 0
      call gz_mpi_read_mul_int8head_b                                   &
     &   (id_file, ioff_gl, nprocs_in, istack_buffer(1))
!
      if(id_rank .ge. nprocs_in) return
!
      ioffset = ioff_gl + istack_buffer(id_rank)
      ilength = num * kreal
      ilen_gz = int(istack_buffer(id_rank+1) - istack_buffer(id_rank))
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_chara                                  &
     &         (id_file, ioffset, ilen_gz, gzip_buf(1))
!
      call gzip_infleat_once                                            &
     &   (ilen_gz, gzip_buf(1), ilength, real_dat(1), ilen_gzipped)
      deallocate(gzip_buf)
      ioff_gl = ioff_gl + ilen_gz
!
      if(iflag_endian .eq. iendian_FLIP) then
        l8_byte = ilength
        call byte_swap_f(l8_byte, real_dat(1))
      end if
!
      end subroutine gz_mpi_read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_2d_vector_b(id_file,                       &
     &          nprocs_in, id_rank, ioff_gl, n1, n2, real_dat)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
!
      integer(kind = kint) :: num
!
!
      num = n1 * n2
      call gz_mpi_read_1d_vector_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, num, real_dat(1,1))
!
      end subroutine gz_mpi_read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_binary_data_IO
