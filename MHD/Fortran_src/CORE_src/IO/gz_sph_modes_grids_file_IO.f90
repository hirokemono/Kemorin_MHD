!gz_sph_modes_grids_file_IO.f90
!      module gz_sph_modes_grids_file_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_geom_rtp_file_gz(my_rank)
!      subroutine read_spectr_modes_rj_file_gz(my_rank)
!      subroutine read_geom_rtm_file_gz(my_rank)
!      subroutine read_modes_rlm_file_gz(my_rank)
!
!      subroutine write_geom_rtp_file_gz(my_rank)
!      subroutine write_spectr_modes_rj_file_gz(my_rank)
!      subroutine write_geom_rtm_file_gz(my_rank)
!      subroutine write_modes_rlm_file_gz(my_rank)
!
      module gz_sph_modes_grids_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_node_id_spherical_IO
      use set_parallel_file_name
      use gz_sph_modes_grids_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_file_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      ndir_sph_IO =  3
!
      call add_gzip_extension(sph_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped grid file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
      call read_geom_rtp_data_gz
      call close_gzfile
!
      end subroutine read_geom_rtp_file_gz
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_file_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      ndir_sph_IO =  2
!
      call add_gzip_extension(sph_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped spectr modes file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
      call read_spectr_modes_rj_data_gz
      call close_gzfile
!
      end subroutine read_spectr_modes_rj_file_gz
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_file_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      ndir_sph_IO =  3
!
      call add_gzip_extension(sph_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped grid file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
      call read_geom_rtm_data_gz
      call close_gzfile
!
      end subroutine read_geom_rtm_file_gz
!
!------------------------------------------------------------------
!
      subroutine read_modes_rlm_file_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      ndir_sph_IO =  2
!
      call add_gzip_extension(sph_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped spectr modes file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
      call read_spectr_modes_rlm_data_gz
      call close_gzfile
!
      end subroutine read_modes_rlm_file_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_file_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(sph_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Write gzipped grid file: ', trim(gzip_name)
!
      call open_wt_gzfile(gzip_name)
      call write_geom_rtp_data_gz
      call close_gzfile
!
      end subroutine write_geom_rtp_file_gz
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_file_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(sph_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Write gzipped spectr modes file: ', trim(gzip_name)
!
      call open_wt_gzfile(gzip_name)
      call write_spectr_modes_rj_data_gz
      call close_gzfile
!
      end subroutine write_spectr_modes_rj_file_gz
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_file_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(sph_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Write gzipped grid file: ', trim(gzip_name)
!
      call open_wt_gzfile(gzip_name)
      call write_geom_rtm_data_gz
      call close_gzfile
!
      end subroutine write_geom_rtm_file_gz
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_file_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(sph_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Write gzipped spectr modes file: ', trim(gzip_name)
!
      call open_wt_gzfile(gzip_name)
      call write_modes_rlm_data_gz
      call close_gzfile
!
      end subroutine write_modes_rlm_file_gz
!
!------------------------------------------------------------------
!
      end module gz_sph_modes_grids_file_IO
