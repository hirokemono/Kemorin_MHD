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
!!     &         (gzip_name, my_rank, iflag_swap, ierr)
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
!!      integer(kind = kint) function gz_read_endian_flag(my_rank)
!!      subroutine gz_read_one_integer_b(iflag_swap, int_dat, ierr)
!!      subroutine gz_read_one_real_b(iflag_swap, real_dat, ierr)
!!      subroutine gz_read_mul_int8_b(iflag_swap, num, int8_dat, ierr)
!!      subroutine gz_read_mul_integer_b(iflag_swap, num, int_dat, ierr)
!!      subroutine gz_read_integer_stack_b                              &
!!     &         (iflag_swap, num, istack, ntot, ierr)
!!      subroutine gz_read_mul_character_b                              &
!!     &         (iflag_swap, num, chara_dat, ierr)
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
     &         (gzip_name, my_rank, iflag_swap, ierr)
!
      use set_parallel_file_name
      use skip_gz_comment
!
      integer(kind=kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: gzip_name
      integer(kind = kint), intent(inout) :: iflag_swap, ierr
!
!
      call open_rd_gzfile_f(gzip_name)
      iflag_swap = gz_read_endian_flag(my_rank)
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
      integer(kind = kint) :: ierr
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
      integer(kind = kint), intent(in) :: int_dat
!
      integer(kind = kint) :: ierr
!
!
      call gzwrite_f(kint, int_dat, ierr)
!
      end subroutine gz_write_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_one_real_b(real_dat)
!
      real(kind = kreal), intent(in) :: real_dat
!
      integer(kind = kint) :: ierr
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
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = kint) :: ierr, ilength
!
!
      ilength = num *  kint_gl
      call gzwrite_f(ilength, int8_dat, ierr)
!
      end subroutine gz_write_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_mul_integer_b(num, int_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: ierr, ilength
!
!
      ilength = num *  kint
      call gzwrite_f(ilength, int_dat(1), ierr)
!
      end subroutine gz_write_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_integer_stack_b(num, istack)
!
      integer(kind = kint), intent(in) :: num
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
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      integer(kind = kint) :: ierr, ilength
!
!
      ilength = num *  kchara
      call gzwrite_f(ilength, chara_dat(1), ierr)
!
      end subroutine gz_write_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_1d_vector_b(num, real_dat)
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
!
      integer(kind = kint) :: ilength, ierr
!
!
      ilength =  num * kreal
      call gzwrite_f(ilength, real_dat(1), ierr)
!
      end subroutine gz_write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_2d_vector_b(n1, n2, real_dat)
!
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
!
      integer(kind = kint) :: ierr, ilength
!
!
      ilength = n1 * n2 * kreal
      call gzwrite_f(ilength, real_dat(1,1), ierr)
!
      end subroutine gz_write_2d_vector_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function gz_read_endian_flag(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: ierr, int_dat
!
!
      call gzread_f(iendian_KEEP, kint, int_dat, ierr)
!
      if(int_dat .eq. i_UNIX) then
        if(my_rank.eq.0) write(*,*) 'binary data have correct endian!'
        gz_read_endian_flag = iendian_KEEP
      else if(int_dat .eq. i_XINU) then
        if(my_rank.eq.0) write(*,*) 'binary data have opposite endian!'
        gz_read_endian_flag = iendian_FLIP
      else
        gz_read_endian_flag = -1
        if(my_rank.eq.0) write(*,*) 'Binary Data is someting wrong!',   &
     &                   int_dat
      end if
!
      end function gz_read_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_one_integer_b(iflag_swap, int_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(inout) :: int_dat
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      call gzread_f(iflag_swap, kint, int_dat, ierr)
      if(ierr .ne. kint) ierr = ierr_file
!
      end subroutine gz_read_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_one_real_b(iflag_swap, real_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      real(kind = kreal), intent(inout) :: real_dat
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      call gzread_f(iflag_swap, kreal, real_dat, ierr)
      if(ierr .ne. kreal) ierr = ierr_file
!
      end subroutine gz_read_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_int8_b(iflag_swap, num, int8_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: ilength
!
!
      ierr = 0
      ilength = num * kint_gl
      call gzread_f(iflag_swap, ilength, int8_dat(1), ierr)
      if(ierr .ne. ilength) ierr = ierr_file
!
      end subroutine gz_read_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_mul_integer_b(iflag_swap, num, int_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: ilength
!
!
      ierr = 0
      ilength = num * kint
      call gzread_f(iflag_swap, ilength, int_dat(1), ierr)
      if(ierr .ne. ilength) ierr = ierr_file
!
      end subroutine gz_read_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_integer_stack_b                                &
     &         (iflag_swap, num, istack, ntot, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(in) :: num
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
      subroutine gz_read_mul_character_b                                &
     &         (iflag_swap, num, chara_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: ilength
!
!
      ierr = 0
      ilength = num * kchara
      call gzread_f(iflag_swap, ilength, chara_dat(1), ierr)
      if(ierr .ne. ilength) ierr = ierr_file
!
      end subroutine gz_read_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_1d_vector_b                                    &
     &         (iflag_swap, num, real_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: ilength
!
!
      ierr = 0
      ilength =  num * kreal
      call gzread_f(iflag_swap, ilength, real_dat(1), ierr)
      if(ierr .ne. ilength) ierr = ierr_file
!
      end subroutine gz_read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_2d_vector_b                                    &
     &         (iflag_swap, n1, n2, real_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: ilength
!
!
      ierr = 0
      ilength =  n1 * n2 * kreal
      call gzread_f(iflag_swap, ilength, real_dat(1,1), ierr)
      if(ierr .ne. ilength) ierr = ierr_file
!
      end subroutine gz_read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module gz_binary_IO
