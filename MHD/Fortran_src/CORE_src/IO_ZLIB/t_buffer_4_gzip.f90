!>@file  t_buffer_4_gzip.f90
!!       module t_buffer_4_gzip
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief Output gzipped merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine alloc_zip_buffer(zbuf)
!!      subroutine dealloc_zip_buffer(zbuf)
!!
!!      subroutine defleate_int_vector_b(num, int_dat, zbuf)
!!      subroutine defleate_int8_vector_b(num, int8_dat, zbuf)
!!      subroutine defleate_1d_vector_b(num, real_dat, zbuf)
!!      subroutine defleate_1d_character_b(num, chara_dat, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine infleate_int_vector_b(num, int_dat, zbuf)
!!      subroutine infleate_int8_vector_b(num, int8_dat, zbuf)
!!      subroutine infleate_1d_vector_b(num, real_dat, zbuf)
!!      subroutine infleate_1d_character_b(num, chara_dat, zbuf)
!!          type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module t_buffer_4_gzip
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!>      Structure of 
      type buffer_4_gzip
!>        Actual size of compressed data buffer
        integer(kind = kint_gl) :: ilen_gzipped
!>        Reserved size of compressed data buffer
        integer(kind = kint_gl) :: ilen_gz
!>        Compressed data buffer
        character(len=1), allocatable :: gzip_buf(:)
      end type buffer_4_gzip
!
      integer(kind = kint), parameter :: maxline = 10000
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_zip_buffer(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      allocate(zbuf%gzip_buf(zbuf%ilen_gz))
!
      end subroutine alloc_zip_buffer
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_zip_buffer(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      deallocate(zbuf%gzip_buf)
!
      end subroutine dealloc_zip_buffer
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine defleate_int_vector_b(num, int_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_in, ilen_used, ilen_line
      integer(kind = kint_gl) :: nline
!
!
      zbuf%ilen_gz = dble(num * kint) *1.01 + 24
      call alloc_zip_buffer(zbuf)
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = dble(maxline*kint) * 1.01 + 24
      do
        nline = int(min((num - ist), maxline))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kint
!
        call gzip_defleat_once(ilen_line, int_dat(ist+1), ilen_in,      &
     &      ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      end subroutine defleate_int_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine defleate_int8_vector_b(num, int8_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_in, ilen_used, ilen_line
      integer(kind = kint_gl) :: nline
!
!
      zbuf%ilen_gz =  dble(num * kint_gl) *1.01 + 24
      call alloc_zip_buffer(zbuf)
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = dble(maxline*kint_gl) * 1.01 + 24
      do
        nline = int(min((num - ist), maxline))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kint_gl
!
        call gzip_defleat_once(ilen_line, int8_dat(ist+1), ilen_in,     &
     &      ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      end subroutine defleate_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine defleate_1d_vector_b(num, real_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_in, ilen_used, ilen_line
      integer(kind = kint_gl) :: nline
!
!
      zbuf%ilen_gz = dble(num * kreal) *1.01 + 24
      call alloc_zip_buffer(zbuf)
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = dble(maxline*kreal) * 1.01 + 24
      do
        nline = int(min((num - ist), maxline))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kreal
!
        call gzip_defleat_once(ilen_line, real_dat(ist+1), ilen_in,     &
     &      ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      end subroutine defleate_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine defleate_1d_character_b(num, chara_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_in, ilen_used, ilen_line
      integer(kind = kint_gl) :: nline
!
!
      zbuf%ilen_gz = dble(num * kchara) *1.01 + 24
      call alloc_zip_buffer(zbuf)
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = dble(maxline*kchara) * 1.01 + 24
      do
        nline = int(min((num - ist), maxline))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kchara
!
        call gzip_defleat_once(ilen_line, chara_dat(ist+1), ilen_in,    &
     &      ilen_used, zbuf%gzip_buf(zbuf%ilen_gzipped+1))
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      end subroutine defleate_1d_character_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine infleate_int_vector_b(num, int_dat, zbuf)
!
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_in, ilen_used, ilen_line
      integer(kind = kint_gl) :: nline
!
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = dble(maxline*kint) * 1.01 + 24
      do
        nline = int(min((num - ist), maxline))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kint
!
        call gzip_infleat_once                                          &
     &     (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),                &
     &      ilen_line, int_dat(ist+1), ilen_used)
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_int_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine infleate_int8_vector_b(num, int8_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_in, ilen_used, ilen_line
      integer(kind = kint_gl) :: nline
!
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = dble(maxline*kint_gl) * 1.01 + 24
      do
        nline = int(min((num - ist), maxline))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kint_gl
!
        call gzip_infleat_once                                          &
     &     (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),                &
     &      ilen_line, int8_dat(ist+1), ilen_used)
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine infleate_1d_vector_b(num, real_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_in, ilen_used, ilen_line
      integer(kind = kint_gl) :: nline
!
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = dble(maxline*kreal) * 1.01 + 24
      do
        nline = int(min((num - ist), maxline))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kreal
!
        call gzip_infleat_once                                          &
     &     (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),                &
     &     ilen_line, real_dat(ist+1), ilen_used)
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine infleate_1d_character_b(num, chara_dat, zbuf)
!
      integer(kind = kint_gl), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: ist
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_in, ilen_used, ilen_line
      integer(kind = kint_gl) :: nline
!
!
      ist = 0
      zbuf%ilen_gzipped = 0
      ilen_tmp = dble(maxline*kchara) * 1.01 + 24
      do
        nline = int(min((num - ist), maxline))
        ilen_in = int(min(zbuf%ilen_gz-zbuf%ilen_gzipped, ilen_tmp))
        ilen_line = nline * kchara
!
        call gzip_infleat_once                                          &
     &     (ilen_in, zbuf%gzip_buf(zbuf%ilen_gzipped+1),                &
     &     ilen_line, chara_dat(ist+1), ilen_used)
!
        zbuf%ilen_gzipped = zbuf%ilen_gzipped + ilen_used
        ist = ist + nline
        if(ist .ge. num) exit
      end do
!
      call dealloc_zip_buffer(zbuf)
!
      end subroutine infleate_1d_character_b
!
! -----------------------------------------------------------------------
!
      end module t_buffer_4_gzip
