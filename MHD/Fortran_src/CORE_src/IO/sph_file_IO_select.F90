!sph_file_IO_select.F90
!      module sph_file_IO_select
!
!     Written by H. Matsui on July, 2007
!
!      subroutine sel_read_geom_rtp_file(my_rank)
!      subroutine sel_read_spectr_modes_rj_file(my_rank)
!      subroutine sel_read_geom_rtm_file(my_rank)
!      subroutine sel_read_modes_rlm_file(my_rank)
!
!      subroutine sel_read_org_spectr_rj_file(my_rank)
!
!      subroutine sel_write_geom_rtp_file(my_rank)
!      subroutine sel_write_spectr_modes_rj_file(my_rank)
!      subroutine sel_write_geom_rtm_file(my_rank)
!      subroutine sel_write_modes_rlm_file(my_rank)
!
!      subroutine sel_read_int_4_sph_coriolis
!      subroutine sel_write_int_4_sph_coriolis
!
      module sph_file_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use set_parallel_file_name
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_read_geom_rtp_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
      use sph_modes_grids_file_IO_b
      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_fname_sph_rtp(my_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_geom_rtp_file_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_geom_rtp_file_gz(my_rank)
#endif
!
      else
        call read_geom_rtp_file(my_rank)
      end if
!
      end subroutine sel_read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_spectr_modes_rj_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
      use sph_modes_grids_file_IO_b
      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_fname_sph_rj(my_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_spectr_modes_rj_file_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_spectr_modes_rj_file_gz(my_rank)
#endif
!
      else
        call read_spectr_modes_rj_file(my_rank)
      end if
!
      end subroutine sel_read_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_geom_rtm_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
      use sph_modes_grids_file_IO_b
      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_fname_sph_rtm(my_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_geom_rtm_file_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_geom_rtm_file_gz(my_rank)
#endif
!
      else
        call read_geom_rtm_file(my_rank)
      end if
!
      end subroutine sel_read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_modes_rlm_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
      use sph_modes_grids_file_IO_b
      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_fname_sph_rlm(my_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_modes_rlm_file_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_modes_rlm_file_gz(my_rank)
#endif
!
      else
        call read_modes_rlm_file(my_rank)
      end if
!
      end subroutine sel_read_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_org_spectr_rj_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
      use sph_modes_grids_file_IO_b
      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_fname_org_sph_rj(my_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call read_spectr_modes_rj_file_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call read_spectr_modes_rj_file_gz(my_rank)
#endif
!
      else
        call read_spectr_modes_rj_file(my_rank)
      end if
!
      end subroutine sel_read_org_spectr_rj_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_geom_rtp_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
      use sph_modes_grids_file_IO_b
      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_fname_sph_rtp(my_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call write_geom_rtp_file_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call write_geom_rtp_file_gz(my_rank)
#endif
!
      else
        call write_geom_rtp_file(my_rank)
      end if
!
      end subroutine sel_write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_spectr_modes_rj_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
      use sph_modes_grids_file_IO_b
      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_fname_sph_rj(my_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call write_spectr_modes_rj_file_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call write_spectr_modes_rj_file_gz(my_rank)
#endif
!
      else
        call write_spectr_modes_rj_file(my_rank)
      end if
!
      end subroutine sel_write_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_geom_rtm_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
      use sph_modes_grids_file_IO_b
      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_fname_sph_rtm(my_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call write_geom_rtm_file_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call write_geom_rtm_file_gz(my_rank)
#endif
!
      else
        call write_geom_rtm_file(my_rank)
      end if
!
      end subroutine sel_write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_modes_rlm_file(my_rank)
!
      use m_node_id_spherical_IO
      use sph_modes_grids_file_IO
      use sph_modes_grids_file_IO_b
      use gz_sph_modes_grids_file_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_fname_sph_rlm(my_rank)
!
      if (iflag_sph_file_fmt .eq. id_binary_file_fmt) then
        call write_modes_rlm_file_b(my_rank)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt .eq. id_gzip_txt_file_fmt) then
        call write_modes_rlm_file_gz(my_rank)
#endif
!
      else
        call write_modes_rlm_file(my_rank)
      end if
!
      end subroutine sel_write_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_int_4_sph_coriolis
!
      use m_int_4_sph_coriolis_IO
      use int_4_sph_coriolis_IO
      use int_4_sph_coriolis_IO_b
      use gz_int_4_sph_coriolis_IO
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
      use int_4_sph_coriolis_IO
      use int_4_sph_coriolis_IO_b
      use gz_int_4_sph_coriolis_IO
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
      end module sph_file_IO_select
