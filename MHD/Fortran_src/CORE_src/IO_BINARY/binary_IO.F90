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
!!      subroutine open_read_binary_file(file_name, my_rank, iflag_swap)
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
!!      integer(kind = kint) function endian_check(my_rank, int_dat)
!!      integer(kind = kint) function read_endian_flag(my_rank)
!!      subroutine read_one_integer_b(iflag_swap, int_dat, ierr)
!!      subroutine read_one_real_b(iflag_swap, real_dat, ierr)
!!      subroutine read_mul_int8_b(iflag_swap, num, int_gl_dat, ierr)
!!      subroutine read_mul_integer_b(iflag_swap, num, int_dat, ierr)
!!      subroutine read_integer_stack_b                                 &
!!     &         (iflag_swap, num, istack, ntot, ierr)
!!      subroutine read_mul_character_b(num, chara_dat, ierr)
!!      subroutine read_mul_one_character_b(num, chara_dat, ierr)
!!      subroutine read_1d_vector_b(iflag_swap, num, real_dat, ierr)
!!      subroutine read_2d_vector_b(iflag_swap, n1, n2, real_dat, ierr)
!!@endverbatim
!
      module binary_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_error_IDs
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
      integer(kind = kint), parameter, private :: id_binary = 19
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
      file_name_w_null = add_null_character(file_name)
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
      file_name_w_null = add_null_character(file_name)
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
      subroutine open_read_binary_file(file_name, my_rank, iflag_swap)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(inout) :: iflag_swap
!
      character(len=kchara) :: file_name_w_null
!
!
#ifdef ZLIB_IO
      file_name_w_null = add_null_character(file_name)
      call open_rd_rawfile(file_name_w_null, ierr_IO)
#else
      open(id_binary, file = file_name, form='unformatted')
#endif
!
      iflag_swap = read_endian_flag(my_rank)
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
      integer(kind = kint_gl), intent(in) :: len_byte
!
      integer(kind = kint) :: len_result, ilength
      character(len=1) :: tmpchara(len_byte)
      integer(kind = kint_gl) :: ist
!
!
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((len_byte - ist), huge_20))
        call rawseek_go_fwd_f(ilength, len_result)
        ist = ist + ilength
        if(ist .ge. len_byte) exit
      end do
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
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int_gl_dat(num)
!
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength *  kint_gl
!
        call rawwrite_f(lbyte, int_gl_dat(ist+1), ierr_IO)
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
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
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength *  kint
!
        call rawwrite_f(lbyte, int_dat(ist+1), ierr_IO)
        ist = ist + ilength
        if(ist .ge. num) exit
      end do
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
      integer(kind = kint_gl), intent(in) :: num
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
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength *  kchara
!
        call rawwrite_f(lbyte, chara_dat(ist+1), ierr_IO)
        ist = ist + ilength
        if(ierr_IO .ne. lbyte) goto 99
        if(ist .ge. num) exit
      end do
#else
      write(id_binary)  chara_dat(1:num)
#endif
!
  99  continue
      return
!
      end subroutine write_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine write_mul_one_character_b(num, chara_dat)
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=1), intent(in) :: chara_dat(num)
!
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength
!
        call rawwrite_f(lbyte, chara_dat(ist+1), ierr_IO)
        ist = ist + ilength
        if(ierr_IO .ne. lbyte) goto 99
        if(ist .ge. num) exit
      end do
#else
      write(id_binary)  chara_dat(1:num)
#endif
!
  99  continue
      return
!
      end subroutine write_mul_one_character_b
!
! -----------------------------------------------------------------------
!
      subroutine write_1d_vector_b(num, real_dat)
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
!
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte =  ilength * kreal
!
        call rawwrite_f(lbyte, real_dat(ist+1), ierr_IO)
        ist = ist + ilength
        if(ierr_IO .ne. lbyte) goto 99
        if(ist .ge. num) exit
      end do
#else
      write(id_binary)  real_dat(1:num)
#endif
!
  99  continue
      return
!
      end subroutine write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine write_2d_vector_b(n1, n2, real_dat)
!
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
!
      integer(kind = kint) :: i2
!
!
#ifdef ZLIB_IO
      do i2 = 1, n2
        call write_1d_vector_b(n1, real_dat(1,i2))
      end do
#else
      write(id_binary)  real_dat(1:n1,1:n2)
#endif
!
      end subroutine write_2d_vector_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function endian_check(my_rank, int_dat)
!
      integer(kind=kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: int_dat
!
!
      if(int_dat .eq. i_UNIX) then
        if(my_rank.eq.0) write(*,*) 'binary data have correct endian!'
        endian_check = iendian_KEEP
      else if(int_dat .eq. i_XINU) then
        if(my_rank.eq.0) write(*,*) 'binary data have opposite endian!'
        endian_check = iendian_FLIP
      else
        endian_check = -1
        if(my_rank.eq.0) write(*,*) 'Binary Data is someting wrong!',   &
     &                               int_dat
      end if
!
      end function endian_check
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function read_endian_flag(my_rank)
!
      integer(kind=kint), intent(in) :: my_rank
      integer(kind = kint) :: int_dat
!
!
#ifdef ZLIB_IO
      call rawread_32bit_f(iendian_KEEP, kint, int_dat, ierr_IO)
      read_endian_flag = endian_check(my_rank, int_dat)
#else
      read(id_binary)  int_dat
      read_endian_flag = iendian_KEEP
#endif
!
      end function read_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine read_one_integer_b(iflag_swap, int_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint), intent(inout) :: int_dat
      integer(kind = kint), intent(inout) :: ierr
!
!
#ifdef ZLIB_IO
      call rawread_32bit_f(iflag_swap, kint, int_dat, ierr)
      if(ierr .ne. kint) goto 99
#else
      read(id_binary, err=99, end=99)  int_dat
#endif
      ierr = 0
      return
!
  99  continue
      ierr = ierr_file
      return
!
      end subroutine read_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine read_one_real_b(iflag_swap, real_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      real(kind = kreal), intent(inout) :: real_dat
      integer(kind = kint), intent(inout) :: ierr
!
!
#ifdef ZLIB_IO
      call rawread_64bit_f(iflag_swap, kreal, real_dat, ierr)
      if(ierr .ne. kreal) goto 99
#else
      read(id_binary, err=99, end=99)  real_dat
#endif
      ierr = 0
      return
!
  99  continue
      ierr = ierr_file
      return
!
      end subroutine read_one_real_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_mul_int8_b(iflag_swap, num, int_gl_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int_gl_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kint_gl
!
        call rawread_64bit_f                                            &
     &     (iflag_swap, lbyte, int_gl_dat(ist+1), ierr)
        ist = ist + ilength
        if(ierr .ne. lbyte) goto 99
        if(ist .ge. num) exit
      end do
#else
      read(id_binary, err=99, end=99)  int_gl_dat(1:num)
#endif
      ierr = 0
      return
!
  99  continue
      ierr = ierr_file
      return
!
      end subroutine read_mul_int8_b
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_integer_b(iflag_swap, num, int_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength * kint
!
        call rawread_32bit_f(iflag_swap, lbyte, int_dat(ist+1), ierr)
        ist = ist + ilength
        if(ierr .ne. lbyte) goto 99
        if(ist .ge. num) exit
      end do
#else
      read(id_binary, err=99, end=99)  int_dat(1:num)
#endif
      ierr = 0
      return
!
  99  continue
      ierr = ierr_file
      return
!
      end subroutine read_mul_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine read_integer_stack_b                                   &
     &         (iflag_swap, num, istack, ntot, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
      integer(kind = kint), intent(inout) :: ierr
!
!
      istack(0) = 0
      call read_mul_integer_b(iflag_swap, num, istack(1), ierr)
      ntot = istack(num)
!
      end subroutine read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_character_b(num, chara_dat, ierr)
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength *  kchara
!
        call rawread_32bit_f                                            &
     &     (iendian_KEEP, lbyte, chara_dat(ist+1), ierr)
        ist = ist + ilength
        if(ierr .ne. lbyte) goto 99
        if(ist .ge. num) exit
      end do
#else
      read(id_binary, err=99, end=99)  chara_dat(1:num)
#endif
      ierr = 0
      return
!
  99  continue
      ierr = ierr_file
      return
!
      end subroutine read_mul_character_b
!
! -----------------------------------------------------------------------
!
      subroutine read_mul_one_character_b(num, chara_dat, ierr)
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=1), intent(inout) :: chara_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte = ilength
!
        call rawread_32bit_f                                            &
     &      (iendian_KEEP, lbyte, chara_dat(ist+1), ierr)
        ist = ist + ilength
        if(ierr .ne. lbyte) goto 99
        if(ist .ge. num) exit
      end do
#else
      read(id_binary, err=99, end=99)  chara_dat(1:num)
#endif
      ierr = 0
      return
!
  99  continue
      ierr = ierr_file
      return
!
      end subroutine read_mul_one_character_b
!
! -----------------------------------------------------------------------
!
      subroutine read_1d_vector_b(iflag_swap, num, real_dat, ierr)
!
      integer(kind = kint), intent(in) :: iflag_swap
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: lbyte, ilength, ist
!
!
      if(num .le. 0) return
#ifdef ZLIB_IO
      ist = 0
      do
        ilength = int(min((num - ist), huge_20))
        lbyte =  ilength * kreal
!
        call rawread_64bit_f(iflag_swap, lbyte, real_dat(ist+1), ierr)
        ist = ist + ilength
        if(ierr .ne. lbyte) goto 99
        if(ist .ge. num) exit
      end do
#else
      read(id_binary, err=99, end=99)  real_dat(1:num)
#endif
      ierr = 0
      return
!
  99  continue
      ierr = ierr_file
      return
!
      end subroutine read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine read_2d_vector_b(iflag_swap, n1, n2, real_dat, ierr)
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
#ifdef ZLIB_IO
      do i2 = 1, n2
        call read_1d_vector_b(iflag_swap, n1, real_dat(1,i2), ierr)
        if(ierr .ne. 0) goto 99
      end do
#else
      read(id_binary, err=99, end=99)  real_dat(1:n1,1:n2)
#endif
      ierr = 0
      return
!
  99  continue
      ierr = ierr_file
      return
!
      end subroutine read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module binary_IO
