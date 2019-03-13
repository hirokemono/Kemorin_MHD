!>@file  gz_binary_IO.f90
!!       module gz_binary_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine open_wt_gzfile_b(gzip_name)
!!      subroutine open_rd_gzfile_b                                     &
!!     &         (gzip_name, id_rank, iflag_swap, ierr)
!!
!!      subroutine gz_write_endian_flag
!!      subroutine gz_write_one_integer_b(int_dat)
!!      subroutine gz_write_one_real_b(real_dat)
!!      subroutine gz_write_mul_int8_b(num, int8_dat)
!!      subroutine gz_write_mul_integer_b(num, int_dat)
!!      subroutine gz_write_integer_stack_b(num, istack)
!!      subroutine gz_write_mul_character_b(num, chara_dat)
!!      subroutine gz_write_1d_vector_b(num, real_dat)
!!      subroutine gz_write_2d_vector_b(n1, n2, real_dat)
!!
!!      integer(kind = kint) function gz_read_endian_flag(id_rank)
!!      subroutine gz_read_one_integer_b(iflag_swap, int_dat, ierr)
!!      subroutine gz_read_one_real_b(iflag_swap, real_dat, ierr)
!!      subroutine gz_read_mul_int8_b(iflag_swap, num, int8_dat, ierr)
!!      subroutine gz_read_mul_integer_b(iflag_swap, num, int_dat, ierr)
!!      subroutine gz_read_integer_stack_b                              &
!!     &         (iflag_swap, num, istack, ntot, ierr)
!!      subroutine gz_read_mul_character_b(num, chara_dat, ierr)
!!      subroutine gz_read_1d_vector_b                                  &
!!     &         (iflag_swap, num, real_dat, ierr)
!!      subroutine gz_read_2d_vector_b                                  &
!!     &         (iflag_swap, n1, n2, real_dat, ierr)
!!@endverbatim
!
      module gz_binary_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_error_IDs
!
      implicit none
!
      private :: gz_read_endian_flag
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_wt_gzfile_b(gzip_name)
!
      use set_parallel_file_name
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
!
!
      call open_wt_gzfile_f(gzip_name)
      call gz_write_endian_flag
!
      end subroutine open_wt_gzfile_b
!
!------------------------------------------------------------------
!
      subroutine open_rd_gzfile_b                                       &
     &         (gzip_name, id_rank, iflag_swap, ierr)
!
      use set_parallel_file_name
      use skip_gz_comment
!
      integer(kind=kint), intent(in) :: id_rank
      character(len=kchara), intent(in) :: gzip_name
      integer(kind = kint), intent(inout) :: iflag_swap, ierr
!
!
      call open_rd_gzfile_f(gzip_name)
      iflag_swap = gz_read_endian_flag(id_rank)
!
      ierr = 0
      if(iflag_swap .eq. -1) ierr = ierr_file
!
      end subroutine open_rd_gzfile_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_endian_flag
!
      integer :: ierr
!
!
      call gzwrite_f(kint, i_UNIX, ierr)
!
      end subroutine gz_write_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_one_integer_b(int_dat)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: int_dat
!
      integer :: ierr
!
!
      call gzwrite_f(kint_gl, cast_long(int_dat), ierr)
!
      end subroutine gz_write_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_one_real_b(real_dat)
!
      real(kind = kreal), intent(in) :: real_dat
!
      integer:: ierr
!
!
      call gzwrite_f(kreal, real_dat, ierr)
!
      end subroutine gz_write_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_int8_b(num, int8_dat)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = kint) :: ist
      integer :: lbyte, ilength, ierr
!
!
      ierr = 0
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kint_gl
!
        call gzwrite_f(lbyte, int8_dat(ist+1), ierr)
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
!
      end subroutine gz_write_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_integer_b(num, int_dat)
!
      use transfer_to_long_integers
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      type(tmp_i8_array)  :: tmp64
!
      if(num .le. 0) return
      call dup_from_short_array(num, int_dat, tmp64)
      call gz_write_mul_int8_b(num, tmp64%id_a)
      call dealloc_1d_i8array(tmp64)
!
      end subroutine gz_write_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_integer_stack_b(num, istack)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: istack(0:num)
!
!
      call gz_write_mul_integer_b(num, istack(1))
!
      end subroutine gz_write_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_character_b(num, chara_dat)
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      integer(kind = kint) ::  ist
      integer :: lbyte, ilength, ierr
!
!
      ierr = 0
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kchara
!
        call gzwrite_f(lbyte, chara_dat(ist+1), ierr)
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
!
      end subroutine gz_write_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_1d_vector_b(num, real_dat)
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
!
      integer(kind = kint) :: ist
      integer :: lbyte, ilength, ierr
!
!
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte =  ilength * kreal
        call gzwrite_f(lbyte, real_dat(ist+1), ierr)
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
!
      end subroutine gz_write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_2d_vector_b(n1, n2, real_dat)
!
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
!
      integer(kind = kint) :: i2
!
!
      do i2 = 1, n2
        call gz_write_1d_vector_b(n1, real_dat(1,i2))
      end do
!
      end subroutine gz_write_2d_vector_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function gz_read_endian_flag(id_rank)
!
      use binary_IO
!
      integer, intent(in) :: id_rank
      integer :: ierr_IO, int_dat
!
!
      call gzread_32bit_f(iendian_KEEP, kint, int_dat, ierr_IO)
      gz_read_endian_flag = endian_check(id_rank, int_dat)
!
      end function gz_read_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_one_integer_b(iflag_swap, int_dat, ierr)
!
      integer, intent(in) :: iflag_swap
      integer(kind = kint), intent(inout) :: int_dat
      integer(kind = kint), intent(inout) :: ierr
!
      integer :: ierr_IO
      integer(kind = kint_gl) :: int64
!
!
      ierr = 0
      call gzread_64bit_f(iflag_swap, kint_gl, int64, ierr_IO)
      if(ierr_IO .ne. kint_gl) ierr = ierr_file
!
      int_dat = int(int64,KIND(int_dat))
!
      end subroutine gz_read_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_one_real_b(iflag_swap, real_dat, ierr)
!
      integer, intent(in) :: iflag_swap
      real(kind = kreal), intent(inout) :: real_dat
      integer(kind = kint), intent(inout) :: ierr
!
      integer :: ierr_IO
!
!
      ierr = 0
      call gzread_64bit_f(iflag_swap, kreal, real_dat, ierr_IO)
      if(ierr_IO .ne. kreal) ierr = ierr_file
!
      end subroutine gz_read_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_int8_b(iflag_swap, num, int8_dat, ierr)
!
      integer, intent(in) :: iflag_swap
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer :: ierr_IO
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      ierr = 0
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kint_gl
!
        call gzread_64bit_f(iflag_swap, lbyte, int8_dat(ist+1), ierr_IO)
        ist = ist + ilength
        if(ist .ge. num) exit
        if(ierr_IO .ne. lbyte) ierr = ierr_file
      end do
!
      end subroutine gz_read_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_integer_b(iflag_swap, num, int_dat, ierr)
!
      use transfer_to_long_integers
!
      integer, intent(in) :: iflag_swap
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      type(tmp_i8_array)  :: tmp64
!
      if(num .le. 0) return
      call alloc_1d_i8array(num, tmp64)
      call gz_read_mul_int8_b(iflag_swap, num, tmp64%id_a, ierr)
      call dup_to_short_array(tmp64, int_dat)
!
      end subroutine gz_read_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_integer_stack_b                                &
     &         (iflag_swap, num, istack, ntot, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      istack(0) = 0
      call gz_read_mul_integer_b(iflag_swap, num, istack(1), ierr)
      ntot = istack(num)
!
      end subroutine gz_read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_character_b(num, chara_dat, ierr)
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer :: ierr_IO
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      ierr = 0
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kchara
!
        call gzread_32bit_f                                             &
     &     (iendian_KEEP, lbyte, chara_dat(ist+1), ierr_IO)
        ist = ist + ilength
        if(ist .ge. num) exit
        if(ierr_IO .ne. lbyte) ierr = ierr_file
      end do
!
      end subroutine gz_read_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_1d_vector_b                                    &
     &         (iflag_swap, num, real_dat, ierr)
!
      integer, intent(in) :: iflag_swap
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer :: ierr_IO
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      ierr = 0
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kreal
!
        call gzread_64bit_f                                             &
     &     (iflag_swap, lbyte, real_dat(ist+1), ierr_IO)
        ist = ist + ilength
        if(ist .ge. num) exit
        if(ierr_IO .ne. lbyte) ierr = ierr_file
      end do
!
      end subroutine gz_read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_2d_vector_b                                    &
     &         (iflag_swap, n1, n2, real_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: i2
!
!
      do i2 = 1, n2
        call gz_read_1d_vector_b(iflag_swap, n1, real_dat(1,i2), ierr)
        if(ierr .ne. 0) exit
      end do
!
      end subroutine gz_read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module gz_binary_IO
