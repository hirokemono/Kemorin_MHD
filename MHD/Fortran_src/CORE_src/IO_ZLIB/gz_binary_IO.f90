!>@file  gz_binary_IO.f90
!!       module gz_binary_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine open_wt_gzfile_b(gzip_name, zbuf)
!!      subroutine open_rd_gzfile_b(gzip_name, id_rank, bflag)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_write_one_integer_b(int_dat, zbuf)
!!      subroutine gz_write_one_real_b(real_dat, zbuf)
!!      subroutine gz_write_mul_int8_b(num, int8_dat, zbuf)
!!      subroutine gz_write_mul_integer_b(num, int_dat, zbuf)
!!      subroutine gz_write_integer_stack_b(num, istack, zbuf)
!!      subroutine gz_write_mul_character_b(num, chara_dat, zbuf)
!!      subroutine gz_write_1d_vector_b(num, real_dat, zbuf)
!!      subroutine gz_write_2d_vector_b(n1, n2, real_dat, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_endian_flag(id_rank, bflag)
!!      subroutine gz_read_one_integer_b(bflag, int_dat)
!!      subroutine gz_read_one_real_b(bflag, real_dat)
!!      subroutine gz_read_mul_int8_b(bflag, num, int8_dat)
!!      subroutine gz_read_mul_integer_b(bflag, num, int_dat)
!!      subroutine gz_read_integer_stack_b(bflag, num, istack, ntot)
!!      subroutine gz_read_mul_character_b(bflag, num, chara_dat)
!!      subroutine gz_read_1d_vector_b(bflag, num, real_dat)
!!      subroutine gz_read_2d_vector_b(bflag, n1, n2, real_dat)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!@endverbatim
!
      module gz_binary_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_error_IDs
      use t_buffer_4_gzip
      use binary_IO
!
      implicit none
!
      private :: gz_write_endian_flag, gz_read_endian_flag
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_wt_gzfile_b(gzip_name, zbuf)
!
      use set_parallel_file_name
      use skip_gz_comment
      use calypso_c_binding
!
      character(len=kchara), intent(in) :: gzip_name
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call open_wt_gzfile_f(gzip_name)
      call gz_write_endian_flag(zbuf)
!
      end subroutine open_wt_gzfile_b
!
!------------------------------------------------------------------
!
      subroutine open_rd_gzfile_b(gzip_name, id_rank, bflag)
!
      use set_parallel_file_name
      use skip_gz_comment
      use calypso_c_binding
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: gzip_name
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call open_rd_gzfile_f(gzip_name)
      call gz_read_endian_flag(id_rank, bflag)
!
      if(bflag%iflag_swap .eq. -1) bflag%ierr_IO = ierr_file
!
      end subroutine open_rd_gzfile_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_endian_flag(zbuf)
!
      use calypso_c_binding
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer, parameter :: i_UNIX4(1) = (/i_UNIX/)
!
!
      call gzwrite_int4_f(1, i_UNIX4, zbuf)
!
      end subroutine gz_write_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_one_integer_b(int_dat, zbuf)
!
      use transfer_to_long_integers
      use calypso_c_binding
!
      integer(kind = kint), intent(in) :: int_dat
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: i8tmp(1)
!
!
      i8tmp(1) = cast_long(int_dat)
      call gzwrite_int8_f(1, i8tmp, zbuf)
!
      end subroutine gz_write_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_one_real_b(real_dat, zbuf)
!
      use calypso_c_binding
!
      real(kind = kreal), intent(in) :: real_dat
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      real(kind = kreal) :: rtmp(1)
!
!
      rtmp(1) = real_dat
      call gzwrite_real_f(1, rtmp, zbuf)
!
      end subroutine gz_write_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_int8_b(num, int8_dat, zbuf)
!
      use calypso_c_binding
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer :: ilength
!
!
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
!
        call gzwrite_int8_f(ilength, int8_dat(ist+1), zbuf)
        ist = ist + ilength
        if(zbuf%ierr_zlib .ne. 0) return
        if(ist .ge. num) exit
      end do
      return
!
      end subroutine gz_write_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_integer_b(num, int_dat, zbuf)
!
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
      type(tmp_i8_array), intent(inout) :: zbuf
!
      type(tmp_i8_array)  :: tmp64
!
      if(num .le. 0) return
      call dup_from_short_array(num, int_dat, tmp64)
      call gz_write_mul_int8_b(num, tmp64%id_a, zbuf)
      if(zbuf%ierr_zlib .ne.  0) return
      call dealloc_1d_i8array(tmp64)
!
      end subroutine gz_write_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_integer_stack_b(num, istack, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: istack(0:num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call gz_write_mul_integer_b(num, istack(1), zbuf)
!
      end subroutine gz_write_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_character_b(num, chara_dat, zbuf)
!
      use calypso_c_binding
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) ::  ist
      integer :: lbyte, ilength
!
!
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kchara
!
        call gzwrite_chara_f(lbyte, chara_dat(ist+1), zbuf)
        ist = ist + ilength
        if(zbuf%ierr_zlib .ne. 0) return
        if(ist .ge. num) exit
      end do
      return
!
      end subroutine gz_write_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_1d_vector_b(num, real_dat, zbuf)
!
      use calypso_c_binding
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer :: ilength
!
!
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
!
        call gzwrite_real_f(ilength, real_dat(ist+1), zbuf)
        if(zbuf%ierr_zlib .ne. 0) return
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
      return
!
      end subroutine gz_write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_2d_vector_b(n1, n2, real_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i2
!
!
      do i2 = 1, n2
        call gz_write_1d_vector_b(n1, real_dat(1,i2), zbuf)
        if(zbuf%ierr_zlib .ne. 0) return
      end do
!
      end subroutine gz_write_2d_vector_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_endian_flag(id_rank, bflag)
!
      use binary_IO
      use calypso_c_binding
!
      integer, intent(in) :: id_rank
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer :: int_dat(1)
      type(buffer_4_gzip) :: zbuf1
!
!
      zbuf1%iflag_swap = iendian_KEEP
      call gzread_int4_f(1, int_dat, zbuf1)
      bflag%ierr_IO = int(zbuf1%ierr_zlib, KIND(bflag%ierr_IO))
      bflag%iflag_swap = endian_check(id_rank, int_dat(1))
!
      end subroutine gz_read_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_one_integer_b(bflag, int_dat)
!
      use calypso_c_binding
!
      integer(kind = kint), intent(inout) :: int_dat
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) :: int64(1)
      type(buffer_4_gzip) :: zbuf1
!
!
      zbuf1%iflag_swap = bflag%iflag_swap
      call gzread_int8_f(1, int64, zbuf1)
      bflag%ierr_IO = int(zbuf1%ierr_zlib, KIND(bflag%ierr_IO))
!
      int_dat = int(int64(1),KIND(int_dat))
!
      end subroutine gz_read_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_one_real_b(bflag, real_dat)
!
      use calypso_c_binding
!
      real(kind = kreal), intent(inout) :: real_dat
      type(binary_IO_flags), intent(inout) :: bflag
!
      real(kind = kreal) :: rtmp(1)
      type(buffer_4_gzip) :: zbuf1
!
!
      zbuf1%iflag_swap = bflag%iflag_swap
      call gzread_real_f(1, rtmp, zbuf1)
      bflag%ierr_IO = int(zbuf1%ierr_zlib, KIND(bflag%ierr_IO))
      real_dat = rtmp(1)
!
      end subroutine gz_read_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_int8_b(bflag, num, int8_dat)
!
      use calypso_c_binding
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: lbyte, ilength
      integer(kind = kint_gl) :: ist
      type(buffer_4_gzip) :: zbuf1
!
!
      zbuf1%iflag_swap = bflag%iflag_swap
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kint_gl
!
        call gzread_int8_f(ilength, int8_dat(ist+1), zbuf1)
        bflag%ierr_IO = int(zbuf1%ierr_zlib, KIND(bflag%ierr_IO))
        if(bflag%ierr_IO .ne. 0) return
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
!
      end subroutine gz_read_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_integer_b(bflag, num, int_dat)
!
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      type(tmp_i8_array)  :: tmp64
!
      if(num .le. 0) return
      call alloc_1d_i8array(num, tmp64)
      call gz_read_mul_int8_b(bflag, num, tmp64%id_a)
      if(bflag%ierr_IO .ne. 0) return
      call dup_to_short_array(tmp64, int_dat)
!
      end subroutine gz_read_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_integer_stack_b(bflag, num, istack, ntot)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      istack(0) = 0
      call gz_read_mul_integer_b(bflag, num, istack(1))
      if(bflag%ierr_IO .ne. 0) return
      ntot = istack(num)
!
      end subroutine gz_read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_character_b(bflag, num, chara_dat)
!
      use calypso_c_binding
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: lbyte, ilength
      integer(kind = kint_gl) :: ist
      type(buffer_4_gzip) :: zbuf1
!
!
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kchara
!
        zbuf1%iflag_swap = iendian_KEEP
        call gzread_chara_f(lbyte, chara_dat(ist+1), zbuf1)
        bflag%ierr_IO = int(zbuf1%ierr_zlib, KIND(bflag%ierr_IO))
        if(bflag%ierr_IO .ne. 0) return
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
!
      end subroutine gz_read_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_1d_vector_b(bflag, num, real_dat)
!
      use calypso_c_binding
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: ilength
      integer(kind = kint_gl) :: ist
      type(buffer_4_gzip) :: zbuf1
!
!
      zbuf1%iflag_swap = bflag%iflag_swap
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
!
        call gzread_real_f(ilength, real_dat(ist+1), zbuf1)
        bflag%ierr_IO = int(zbuf1%ierr_zlib, KIND(bflag%ierr_IO))
        if(bflag%ierr_IO .ne. 0) return
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
!
      end subroutine gz_read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_2d_vector_b(bflag, n1, n2, real_dat)
!
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint) :: i2
!
!
      do i2 = 1, n2
        call gz_read_1d_vector_b(bflag, n1, real_dat(1,i2))
        if(bflag%ierr_IO .ne. 0) return
      end do
!
      end subroutine gz_read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module gz_binary_IO
