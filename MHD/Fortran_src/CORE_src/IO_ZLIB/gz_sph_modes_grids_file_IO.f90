!>@file   gz_sph_modes_grids_file_IO.f90
!!@brief  module gz_sph_modes_grids_file_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Spectr data IO routines using zlib
!!
!!@verbatim
!!      subroutine read_geom_rtp_file_gz(my_rank, file_name)
!!      subroutine read_spectr_modes_rj_file_gz(my_rank, file_name)
!!      subroutine read_geom_rtm_file_gz(my_rank, file_name)
!!      subroutine read_modes_rlm_file_gz(my_rank, file_name)
!!
!!      subroutine write_geom_rtp_file_gz(my_rank, file_name)
!!      subroutine write_spectr_modes_rj_file_gz(my_rank, file_name)
!!      subroutine write_geom_rtm_file_gz(my_rank, file_name)
!!      subroutine write_modes_rlm_file_gz(my_rank, file_name)
!!@endverbatim
!!
!!@param my_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module gz_sph_modes_grids_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_node_id_spherical_IO
      use set_parallel_file_name
      use gz_sph_modes_grids_data_IO
      use skip_gz_comment
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_file_gz(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  3
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped grid file: ', trim(file_name)
!
      call open_rd_gzfile_f(file_name)
      call read_geom_rtp_data_gz
      call close_gzfile_f
!
      end subroutine read_geom_rtp_file_gz
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_file_gz(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  2
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped spectr modes file: ', trim(file_name)
!
      call open_rd_gzfile_f(file_name)
      call read_spectr_modes_rj_data_gz
      call close_gzfile_f
!
      end subroutine read_spectr_modes_rj_file_gz
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_file_gz(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  3
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped grid file: ', trim(file_name)
!
      call open_rd_gzfile_f(file_name)
      call read_geom_rtm_data_gz
      call close_gzfile_f
!
      end subroutine read_geom_rtm_file_gz
!
!------------------------------------------------------------------
!
      subroutine read_modes_rlm_file_gz(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  2
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped spectr modes file: ', trim(file_name)
!
      call open_rd_gzfile_f(file_name)
      call read_spectr_modes_rlm_data_gz
      call close_gzfile_f
!
      end subroutine read_modes_rlm_file_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_file_gz(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Write gzipped grid file: ', trim(file_name)
!
      call open_wt_gzfile_f(file_name)
      call write_geom_rtp_data_gz
      call close_gzfile_f
!
      end subroutine write_geom_rtp_file_gz
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_file_gz(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Write gzipped spectr modes file: ', trim(file_name)
!
      call open_wt_gzfile_f(file_name)
      call write_spectr_modes_rj_data_gz
      call close_gzfile_f
!
      end subroutine write_spectr_modes_rj_file_gz
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_file_gz(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Write gzipped grid file: ', trim(file_name)
!
      call open_wt_gzfile_f(file_name)
      call write_geom_rtm_data_gz
      call close_gzfile_f
!
      end subroutine write_geom_rtm_file_gz
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_file_gz(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Write gzipped spectr modes file: ', trim(file_name)
!
      call open_wt_gzfile_f(file_name)
      call write_modes_rlm_data_gz
      call close_gzfile_f
!
      end subroutine write_modes_rlm_file_gz
!
!------------------------------------------------------------------
!
      end module gz_sph_modes_grids_file_IO
