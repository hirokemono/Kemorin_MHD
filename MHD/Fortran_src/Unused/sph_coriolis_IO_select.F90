!>@file   sph_coriolis_IO_select.f90
!!@brief  module sph_coriolis_IO_select
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Spectr data IO selector
!!
!!@verbatim
!!      subroutine sel_read_int_4_sph_coriolis
!!      subroutine sel_write_int_4_sph_coriolis
!!@endverbatim
!!
!!@param my_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module sph_coriolis_IO_select
!
      use m_precision
!
#ifdef ZLIB_IO
use gz_int_4_sph_coriolis_IO
#endif
!
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_read_int_4_sph_coriolis
!
      use m_int_4_sph_coriolis_IO
      use m_file_format_switch
      use set_parallel_file_name
      use int_4_sph_coriolis_IO
      use int_4_sph_coriolis_IO_b
!
!
      if (ifmt_cor_int_file .eq. id_binary_file_fmt) then
        call read_int_4_sph_coriolis_b
!
#ifdef ZLIB_IO
      else if(ifmt_cor_int_file .eq. id_gzip_txt_file_fmt) then
        call read_int_4_sph_coriolis_gz
#endif
!
      else
        call read_int_4_sph_coriolis
      end if
!
      end subroutine sel_read_int_4_sph_coriolis
!
!------------------------------------------------------------------
!
      subroutine sel_write_int_4_sph_coriolis
!
      use m_int_4_sph_coriolis_IO
      use m_file_format_switch
      use set_parallel_file_name
      use int_4_sph_coriolis_IO
      use int_4_sph_coriolis_IO_b
!
!
      if (ifmt_cor_int_file .eq. id_binary_file_fmt) then
        call write_int_4_sph_coriolis_b
!
#ifdef ZLIB_IO
      else if(ifmt_cor_int_file .eq. id_gzip_txt_file_fmt) then
        call write_int_4_sph_coriolis_gz
#endif
!
      else
        call write_int_4_sph_coriolis
      end if
!
      end subroutine sel_write_int_4_sph_coriolis
!
!------------------------------------------------------------------
!
      end module sph_coriolis_IO_select
