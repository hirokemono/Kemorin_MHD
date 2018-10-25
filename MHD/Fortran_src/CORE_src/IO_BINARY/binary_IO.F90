!>@file  binary_IO.f90
!!       module binary_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Core routines for binary IO
!!
!!@verbatim
!!      subroutine open_write_binary_file(file_name)
!!      subroutine open_append_binary_file(file_name)
!!      subroutine open_read_binary_file(file_name, my_rank)
!!      subroutine close_binary_file
!!      subroutine seek_forward_binary_file(len_byte)
!!
!!      subroutine write_endian_flag
!!      subroutine write_one_integer_b(int_dat)
!!      subroutine write_one_real_b(real_dat)
!!      subroutine write_mul_int8_b(num, int_gl_dat)
!!      subroutine write_mul_integer_b(num, int_dat)
!!      subroutine write_integer_stack_b(num, istack)
!!      subroutine write_mul_character_b(num, chara_dat)
!!      subroutine write_mul_one_character_b(num, chara_dat)
!!      subroutine write_1d_vector_b(num, real_dat)
!!      subroutine write_2d_vector_b(n1, n2, real_dat)
!!
!!      integer(kind = kint) function read_endian_flag(my_rank)
!!      subroutine read_one_integer_b(int_dat)
!!      subroutine read_one_real_b(real_dat)
!!      subroutine read_mul_int8_b(num, int_gl_dat)
!!      subroutine read_mul_integer_b(num, int_dat)
!!      subroutine read_integer_stack_b(num, istack, ntot)
!!      subroutine read_mul_character_b(num, chara_dat)
!!      subroutine read_mul_one_character_b                             &
!!     &         (iflag_swap, num, chara_dat, ierr)
!!      subroutine read_1d_vector_b(num, real_dat)
!!      subroutine read_2d_vector_b(n1, n2, real_dat)
!!@endverbatim
!
      module binary_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      type file_IO_flags
!>        integer flag for byte swapping
        integer(kind = kint) :: iflag_bin_swap = -1
!>        Error flag for data IO
        integer(kind = kint) :: ierr_IO = 0
      end type file_IO_flags
!
      integer(kind = kint), parameter :: id_binary = 19
!
      integer(kind = kint), private :: ierr_IO
!
      private :: write_endian_flag, read_endian_flag
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_write_binary_file(file_name)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_name
      character(len=kchara) :: file_name_w_null
!
!
#ifdef ZLIB_IO
      call add_null_character(file_name, file_name_w_null)
      call open_wt_rawfile(file_name_w_null, ierr_IO)
#else
      open(id_binary, file = file_name, form='unformatted')
#endif
!
      call write_endian_flag
!
      end subroutine open_write_binary_file
!
! -----------------------------------------------------------------------
!
      subroutine open_append_binary_file(file_name)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_name
      character(len=kchara) :: file_name_w_null
!
!
#ifdef ZLIB_IO
      call add_null_character(file_name, file_name_w_null)
      call open_ad_rawfile(file_name_w_null, ierr_IO)
#else
      open(id_binary, file = file_name, form='unformatted',             &
     &      position='append')
#endif
!
      end subroutine open_append_binary_file
!
! -----------------------------------------------------------------------
!
      subroutine open_read_binary_file(file_name, my_rank)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      character(len=kchara) :: file_name_w_null
!
!
#ifdef ZLIB_IO
      call add_null_character(file_name, file_name_w_null)
      call open_rd_rawfile(file_name_w_null, ierr_IO)
#else
      open(id_binary, file = file_name, form='unformatted')
#endif
!
      iflag_endian = read_endian_flag(my_rank)
!
      end subroutine open_read_binary_file
!
! -----------------------------------------------------------------------
!
      subroutine close_binary_file
!
#ifdef ZLIB_IO
      call close_rawfile
#else
      close(id_binary)
#endif
!
      end subroutine close_binary_file
!
! -----------------------------------------------------------------------
!
      subroutine seek_forward_binary_file(len_byte)
!
      integer(kind = kint), intent(in) :: len_byte
      integer(kind = kint) :: len_result
!
      character(len=1) :: tmpchara(len_byte)
!
!
#ifdef ZLIB_IO
      call rawseek_go_fwd_f(len_byte, len_result)
#else
      read(id_binary) tmpchara(1:len_byte)
#endif
!
      end subroutine seek_forward_binary_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_endian_flag
!
!
!
#ifdef ZLIB_IO
      call rawwrite_f(kint, i_UNIX, ierr_IO)
#else
      write(id_binary)  i_UNIX
#endif
!
      end subroutine write_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine write_one_integer_b(int_dat)
!
      integer(kind = kint), intent(in) :: int_dat
!
!
!
#ifdef ZLIB_IO
      call rawwrite_f(kint, int_dat, ierr_IO)
#else
      write(id_binary)  int_dat
#endif
!
      end subroutine write_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine write_one_real_b(real_dat)
!
      real(kind = kreal), intent(in) :: real_dat
!
!
!
#ifdef ZLIB_IO
      call rawwrite_f(kreal, real_dat, ierr_IO)
#else
      write(id_binary)  real_dat
#endif
!
      end subroutine write_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_mul_int8_b(num, int_gl_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int_gl_dat(num)
!
      integer(kind = kint) :: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ilength = num *  kint_gl
      call rawwrite_f(ilength, int_gl_dat(1), ierr_IO)
#else
      write(id_binary)  int_gl_dat(1:num)
#endif
!
      end subroutine write_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine write_mul_integer_b(num, int_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ilength = num *  kint
      call rawwrite_f(ilength, int_dat(1), ierr_IO)
#else
      write(id_binary)  int_dat(1:num)
#endif
!
      end subroutine write_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine write_integer_stack_b(num, istack)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: istack(0:num)
!
!
      call write_mul_integer_b(num, istack(1))
!
      end subroutine write_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine write_mul_character_b(num, chara_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      integer(kind = kint) :: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ilength = num *  kchara
      call rawwrite_f(ilength, chara_dat(1), ierr_IO)
#else
      write(id_binary)  chara_dat(1:num)
#endif
!
      end subroutine write_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine write_mul_one_character_b(num, chara_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=1), intent(in) :: chara_dat(num)
!
      integer(kind = kint) :: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ilength = num
      call rawwrite_f(ilength, chara_dat(1), ierr_IO)
#else
      write(id_binary)  chara_dat(1:num)
#endif
!
end subroutine write_mul_one_character_b
!
! -----------------------------------------------------------------------
!
      subroutine write_1d_vector_b(num, real_dat)
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
!
      integer(kind = kint) :: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ilength =  num * kreal
      call rawwrite_f(ilength, real_dat(1), ierr_IO)
#else
      write(id_binary)  real_dat(1:num)
#endif
!
      end subroutine write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine write_2d_vector_b(n1, n2, real_dat)
!
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
!
      integer(kind = kint) :: ilength
!
!
#ifdef ZLIB_IO
      if(n1*n2 .le. 0) return
      ilength = n1 * n2 * kreal
      call rawwrite_f(ilength, real_dat(1,1), ierr_IO)
#else
      write(id_binary)  real_dat(1:n1,1:n2)
#endif
!
      end subroutine write_2d_vector_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function read_endian_flag(my_rank)
!
      integer(kind=kint), intent(in) :: my_rank
      integer(kind = kint) :: int_dat
!
!
#ifdef ZLIB_IO
      call rawread_f(iendian_KEEP, kint, int_dat, ierr_IO)
!
      if(int_dat .eq. i_UNIX) then
        if(my_rank.eq.0) write(*,*) 'binary data have correct endian!'
        read_endian_flag = iendian_KEEP
      else if(int_dat .eq. i_XINU) then
        if(my_rank.eq.0) write(*,*) 'binary data have opposite endian!'
        read_endian_flag = iendian_FLIP
      else
        read_endian_flag = -1
        if(my_rank.eq.0) write(*,*) 'Binary Data is someting wrong!',   &
     &                               int_dat
      end if
#else
      read(id_binary)  int_dat
      read_endian_flag = iendian_KEEP
#endif
!
      end function read_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine read_one_integer_b(int_dat)
!
      integer(kind = kint), intent(inout) :: int_dat
!
!
#ifdef ZLIB_IO
      call rawread_f(iflag_endian, kint, int_dat, ierr_IO)
      if(ierr_IO .ne. kint) goto 99
#else
      read(id_binary, err=99, end=99)  int_dat
#endif
      ierr_IO = 0
      return
!
  99  continue
      ierr_IO = 1
      return
!
      end subroutine read_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine read_one_real_b(real_dat)
!
      real(kind = kreal), intent(inout) :: real_dat
!
!
#ifdef ZLIB_IO
      call rawread_f(iflag_endian, kreal, real_dat, ierr_IO)
      if(ierr_IO .ne. kreal) goto 99
#else
      read(id_binary, err=99, end=99)  real_dat
#endif
      ierr_IO = 0
      return
!
  99  continue
      ierr_IO = 1
      return
!
      end subroutine read_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_mul_int8_b(num, int_gl_dat)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int_gl_dat(num)
!
      integer(kind = kint) :: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ilength = num * kint_gl
      call rawread_f(iflag_endian, ilength, int_gl_dat(1), ierr_IO)
      if(ierr_IO .ne. ilength) goto 99
#else
      read(id_binary, err=99, end=99)  int_gl_dat(1:num)
#endif
      ierr_IO = 0
      return
!
  99  continue
      ierr_IO = 1
      return
!
      end subroutine read_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_integer_b(num, int_dat)
!
!      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(in) :: num
!      integer(kind = kint), intent(inout) :: ierr
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      integer(kind = kint) :: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ilength = num * kint
      call rawread_f(iflag_endian, ilength, int_dat(1), ierr_IO)
      if(ierr_IO .ne. ilength) goto 99
#else
      read(id_binary, err=99, end=99)  int_dat(1:num)
#endif
      ierr_IO = 0
      return
!
  99  continue
      ierr_IO = 1
      return
!
      end subroutine read_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine read_integer_stack_b(num, istack, ntot)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
!
!
      istack(0) = 0
      call read_mul_integer_b(num, istack(1))
      ntot = istack(num)
!
      end subroutine read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_character_b(num, chara_dat)
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
!
      integer(kind = kint) :: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ilength = num * kchara
      call rawread_f(iflag_endian, ilength, chara_dat(1), ierr_IO)
      if(ierr_IO .ne. ilength) goto 99
#else
      read(id_binary, err=99, end=99)  chara_dat(1:num)
#endif
      ierr_IO = 0
      return
!
  99  continue
      ierr_IO = 1
      return
!
      end subroutine read_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_one_character_b                               &
     &         (iflag_swap, num, chara_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(in) :: num
      character(len=1), intent(inout) :: chara_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ilength = num
      call rawread_f(iflag_swap, ilength, chara_dat(1), ierr)
      if(ierr .ne. ilength) goto 99
#else
      read(id_binary, err=99, end=99)  chara_dat(1:num)
#endif
      ierr = 0
      return
!
  99  continue
      ierr = 1
      return
!
      end subroutine read_mul_one_character_b
!
! -----------------------------------------------------------------------
!
      subroutine read_1d_vector_b(num, real_dat)
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = kint) :: ilength
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ilength =  num * kreal
      call rawread_f(iflag_endian, ilength, real_dat(1), ierr_IO)
      if(ierr_IO .ne. ilength) goto 99
#else
      read(id_binary, err=99, end=99)  real_dat(1:num)
#endif
      ierr_IO = 0
      return
!
  99  continue
      ierr_IO = 1
      return
!
      end subroutine read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine read_2d_vector_b(n1, n2, real_dat)
!
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
!
      integer(kind = kint) :: ilength
!
!
#ifdef ZLIB_IO
      if(n1*n2 .le. 0) return
      ilength =  n1 * n2 * kreal
      call rawread_f(iflag_endian, ilength, real_dat(1,1), ierr_IO)
      if(ierr_IO .ne. ilength) goto 99
#else
      read(id_binary, err=99, end=99)  real_dat(1:n1,1:n2)
#endif
      ierr_IO = 0
      return
!
  99  continue
      ierr_IO = 1
      return
!
      end subroutine read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module binary_IO
