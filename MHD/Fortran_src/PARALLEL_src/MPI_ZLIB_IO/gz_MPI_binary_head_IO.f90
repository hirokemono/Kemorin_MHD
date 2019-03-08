!>@file  gz_MPI_binary_head_IO.f90
!!       module gz_MPI_binary_head_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_mpi_write_mul_inthead_b(IO_param, num, int_dat)
!!        Substittion of gz_write_mul_integer_b
!!      subroutine gz_mpi_write_i8stack_head_b(IO_param, num, i8stack)
!!      subroutine gz_mpi_write_mul_int8head_b(IO_param, num, int8_dat)
!!        Substittion of gz_write_mul_int8_b
!!      subroutine gz_mpi_write_charahead(IO_param, ilength, chara_dat)
!!      subroutine gz_mpi_write_mul_charahead_b(IO_param, num, chara_dat)
!!       Substittion of gz_write_mul_character_b
!!      subroutine gz_mpi_write_mul_realhead_b(IO_param, num, real_dat)
!!
!!      subroutine gz_mpi_read_mul_inthead_b(IO_param, num, int_dat)
!!        Substittion of gz_read_mul_integer_b
!!      subroutine gz_mpi_read_i8stack_head_b(IO_param, num, i8stack)
!!      subroutine gz_mpi_read_mul_int8head_b(IO_param, num, int_dat)
!!        Substittion of gz_read_mul_int8_b
!!      subroutine gz_mpi_read_mul_charahead_b(IO_param, num, chara_dat)
!!        Substittion of gz_read_mul_character_b
!!      subroutine gz_mpi_read_mul_realhead_b(IO_param, num, real_dat)
!!@endverbatim
!
      module gz_MPI_binary_head_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use t_buffer_4_gzip
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_mul_inthead_b(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: num64
!
!
      if(my_rank .eq. 0) then
        num64 = num
        call defleate_int_vector_b(num64, int_dat, zbuf)
!
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_gz(IO_param%id_file, ioffset, zbuf)
        call dealloc_zip_buffer(zbuf)
      end if
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT, 0,       &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_mpi_write_mul_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_i8stack_head_b(IO_param, num, i8stack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: i8stack(0:num)
!
!
      call gz_mpi_write_mul_int8head_b(IO_param, num, i8stack(1))
!
      end subroutine gz_mpi_write_i8stack_head_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_mul_int8head_b(IO_param, num, int8_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: num64
!
!
      if(my_rank .eq. 0) then
        num64 = num
        call defleate_int8_vector_b(num64, int8_dat, zbuf)
!
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_gz(IO_param%id_file, ioffset, zbuf)
        call dealloc_zip_buffer(zbuf)
      end if
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT, 0,       &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_mpi_write_mul_int8head_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_mul_charahead_b(IO_param, num, chara_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: num64
!
!
      if(my_rank .eq. 0) then
        num64 = num
        call defleate_1d_character_b(num64, chara_dat, zbuf)
!
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_gz(IO_param%id_file, ioffset, zbuf)
        call dealloc_zip_buffer(zbuf)
      end if
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT, 0,       &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_mpi_write_mul_charahead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_mul_realhead_b(IO_param, num, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: num64
!
!
      if(my_rank .eq. 0) then
        num64 = num
        call defleate_1d_vector_b(num64, real_dat, zbuf)
!
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_gz(IO_param%id_file, ioffset, zbuf)
        call dealloc_zip_buffer(zbuf)
      end if
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT, 0,       &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_mpi_write_mul_realhead_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_mul_inthead_b(IO_param, num, int_dat)
!
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        zbuf%ilen_gz = int(dble(num*kint)*1.01+24, KIND(zbuf%ilen_gz))
        call alloc_zip_buffer(zbuf)
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
        call infleate_int_vector_b(cast_long(num), int_dat, zbuf)
!
        if(IO_param%iflag_bin_swap .eq. iendian_FLIP) then
          call byte_swap_32bit_f(cast_long(num*kint), int_dat(1))
        end if
      end if
!
      call calypso_mpi_bcast_int(int_dat, cast_long(num), 0)
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT, 0,       &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_mpi_read_mul_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_i8stack_head_b(IO_param, num, i8stack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: i8stack(0:num)
!
!
      i8stack(0) = 0
      call gz_mpi_read_mul_int8head_b(IO_param, num, i8stack(1))
!
      end subroutine gz_mpi_read_i8stack_head_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_mul_int8head_b(IO_param, num, int8_dat)
!
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        zbuf%ilen_gz = int(dble(num*kint_gl)*1.01+24, KIND(zbuf%ilen_gz))
        call alloc_zip_buffer(zbuf)
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
        call infleate_int8_vector_b(cast_long(num), int8_dat, zbuf)
!
        if(IO_param%iflag_bin_swap .eq. iendian_FLIP) then
          call byte_swap_64bit_f(cast_long(num*kint_gl), int8_dat(1))
        end if
      end if
!
      call calypso_mpi_bcast_int8(int8_dat, cast_long(num), 0)
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT, 0,       &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_mpi_read_mul_int8head_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_mul_charahead_b(IO_param, num, chara_dat)
!
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        zbuf%ilen_gz = int(real(num*kchara)*1.01+24,KIND(zbuf%ilen_gz))
        call alloc_zip_buffer(zbuf)
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
        call infleate_1d_character_b(cast_long(num), chara_dat, zbuf)
      end if
!
      call calypso_mpi_bcast_character                                  &
     &   (chara_dat, cast_long(num*kchara), 0)
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT, 0,       &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_mpi_read_mul_charahead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_mul_realhead_b(IO_param, num, real_dat)
!
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        zbuf%ilen_gz = int(dble(num*kreal)*1.01+24, KIND(zbuf%ilen_gz))
        call alloc_zip_buffer(zbuf)
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
        call infleate_1d_vector_b(cast_long(num), real_dat, zbuf)
!
        if(IO_param%iflag_bin_swap .eq. iendian_FLIP) then
          call byte_swap_64bit_f(cast_long(num*kreal), real_dat(1))
        end if
      end if
!
      call calypso_mpi_bcast_real(real_dat, cast_long(num), 0)
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT, 0,       &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_mpi_read_mul_realhead_b
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_binary_head_IO
