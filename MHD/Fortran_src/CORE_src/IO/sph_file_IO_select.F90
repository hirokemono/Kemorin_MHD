!>@file   sph_file_IO_select.f90
!!@brief  module sph_file_IO_select
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Spectr data IO selector
!!
!!@verbatim
!!      subroutine sel_read_geom_rtp_file(my_rank)
!!      subroutine sel_read_spectr_modes_rj_file(my_rank)
!!      subroutine sel_read_geom_rtm_file(my_rank)
!!      subroutine sel_read_modes_rlm_file(my_rank)
!!
!!      subroutine sel_write_geom_rtp_file(my_rank)
!!      subroutine sel_write_spectr_modes_rj_file(my_rank)
!!      subroutine sel_write_geom_rtm_file(my_rank)
!!      subroutine sel_write_modes_rlm_file(my_rank)
!!
!!      integer(kind = kint) function check_exsist_rtp_file(my_rank)
!!      integer(kind = kint) function check_exsist_rj_file(my_rank)
!!      integer(kind = kint) function check_exsist_rtm_file(my_rank)
!!      integer(kind = kint) function check_exsist_rlm_file(my_rank)
!!@endverbatim
!!
!!@param my_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module sph_file_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use m_node_id_spherical_IO
      use set_parallel_file_name
      use set_mesh_file_names
!
      use sph_modes_grids_file_IO
      use sph_modes_grids_file_IO_b
      use gz_sph_modes_grids_file_IO
      use gz_sph_modes_file_IO_b
!
      implicit none
!
      character(len=kchara), private :: sph_file_name
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_read_geom_rtp_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_rtp_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_geom_rtp_file_b(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_geom_rtp_file_b(my_rank, sph_file_name)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_geom_rtp_file_gz(my_rank, sph_file_name)
#endif
!
      else
        call read_geom_rtp_file(my_rank, sph_file_name)
      end if
!
      end subroutine sel_read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_spectr_modes_rj_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_rj_file_name(sph_file_head, iflag_sph_file_fmt,      &
     &    my_rank, sph_file_name)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_spectr_modes_rj_file_b(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_spectr_modes_rj_file_b(my_rank, sph_file_name)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_spectr_modes_rj_file_gz(my_rank, sph_file_name)
#endif
!
      else
        call read_spectr_modes_rj_file(my_rank, sph_file_name)
      end if
!
      end subroutine sel_read_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_geom_rtm_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_rtm_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_geom_rtm_file_b(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_geom_rtm_file_b(my_rank, sph_file_name)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_geom_rtm_file_gz(my_rank, sph_file_name)
#endif
!
      else
        call read_geom_rtm_file(my_rank, sph_file_name)
      end if
!
      end subroutine sel_read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_modes_rlm_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_rlm_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_modes_rlm_file_b(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_read_modes_rlm_file_b(my_rank, sph_file_name)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_modes_rlm_file_gz(my_rank, sph_file_name)
#endif
!
      else
        call read_modes_rlm_file(my_rank, sph_file_name)
      end if
!
      end subroutine sel_read_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_geom_rtp_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_rtp_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call write_geom_rtp_file_b(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_write_geom_rtp_file_b(my_rank, sph_file_name)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call write_geom_rtp_file_gz(my_rank, sph_file_name)
#endif
!
      else
        call write_geom_rtp_file(my_rank, sph_file_name)
      end if
!
      end subroutine sel_write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_spectr_modes_rj_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_rj_file_name(sph_file_head, iflag_sph_file_fmt,      &
     &    my_rank, sph_file_name)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call write_spectr_modes_rj_file_b(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_write_spectr_modes_rj_file_b(my_rank, sph_file_name)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call write_spectr_modes_rj_file_gz(my_rank, sph_file_name)
#endif
!
      else
        call write_spectr_modes_rj_file(my_rank, sph_file_name)
      end if
!
      end subroutine sel_write_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_geom_rtm_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_rtm_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call write_geom_rtm_file_b(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_write_geom_rtm_file_b(my_rank, sph_file_name)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call write_geom_rtm_file_gz(my_rank, sph_file_name)
#endif
!
      else
        call write_geom_rtm_file(my_rank, sph_file_name)
      end if
!
      end subroutine sel_write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_modes_rlm_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_rlm_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call write_modes_rlm_file_b(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_bin_file_fmt) then
        call gz_write_modes_rlm_file_b(my_rank, sph_file_name)
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call write_modes_rlm_file_gz(my_rank, sph_file_name)
#endif
!
      else
        call write_modes_rlm_file(my_rank, sph_file_name)
      end if
!
      end subroutine sel_write_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      integer(kind = kint) function check_exsist_rtp_file(my_rank)
!
      use delete_data_files
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_rtp_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
      check_exsist_rtp_file = check_file_exist(sph_file_name)
!
      end function check_exsist_rtp_file
!
!------------------------------------------------------------------
!
      integer(kind = kint) function check_exsist_rj_file(my_rank)
!
      use delete_data_files
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_rj_file_name(sph_file_head, iflag_sph_file_fmt,      &
     &    my_rank, sph_file_name)
      check_exsist_rj_file = check_file_exist(sph_file_name)
!
      end function check_exsist_rj_file
!
!------------------------------------------------------------------
!
      integer(kind = kint) function check_exsist_rtm_file(my_rank)
!
      use delete_data_files
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_rtm_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
      check_exsist_rtm_file = check_file_exist(sph_file_name)
!
      end function check_exsist_rtm_file
!
!------------------------------------------------------------------
!
      integer(kind = kint) function check_exsist_rlm_file(my_rank)
!
      use delete_data_files
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_sph_rlm_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
      check_exsist_rlm_file = check_file_exist(sph_file_name)
!
      end function check_exsist_rlm_file
!
!------------------------------------------------------------------
!
      end module sph_file_IO_select
