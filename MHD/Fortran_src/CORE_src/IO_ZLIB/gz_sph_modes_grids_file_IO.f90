!>@file   gz_sph_modes_grids_file_IO.f90
!!@brief  module gz_sph_modes_grids_file_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Spectr data IO routines using zlib
!!
!!@verbatim
!!      subroutine gz_read_geom_rtp_file(my_rank, file_name)
!!      subroutine gz_read_spectr_modes_rj_file(my_rank, file_name)
!!      subroutine gz_read_geom_rtm_file(my_rank, file_name)
!!      subroutine gz_read_modes_rlm_file(my_rank, file_name)
!!
!!      subroutine gz_write_geom_rtp_file(my_rank, file_name)
!!      subroutine gz_write_spectr_modes_rj_file(my_rank, file_name)
!!      subroutine gz_write_geom_rtm_file(my_rank, file_name)
!!      subroutine gz_write_modes_rlm_file(my_rank, file_name)
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
      subroutine gz_read_geom_rtp_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped grid file: ', trim(file_name)
!
      call open_rd_gzfile_f(file_name)
      call read_geom_rtp_data_gz
      call close_gzfile_f
!
      end subroutine gz_read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine gz_read_spectr_modes_rj_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped spectr modes file: ', trim(file_name)
!
      call open_rd_gzfile_f(file_name)
      call read_spectr_modes_rj_data_gz
      call close_gzfile_f
!
      end subroutine gz_read_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine gz_read_geom_rtm_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped grid file: ', trim(file_name)
!
      call open_rd_gzfile_f(file_name)
      call read_geom_rtm_data_gz
      call close_gzfile_f
!
      end subroutine gz_read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine gz_read_modes_rlm_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read gzipped spectr modes file: ', trim(file_name)
!
      call open_rd_gzfile_f(file_name)
      call read_spectr_modes_rlm_data_gz
      call close_gzfile_f
!
      end subroutine gz_read_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtp_file(my_rank, file_name)
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
      end subroutine gz_write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine gz_write_spectr_modes_rj_file(my_rank, file_name)
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
      end subroutine gz_write_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtm_file(my_rank, file_name)
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
      end subroutine gz_write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine gz_write_modes_rlm_file(my_rank, file_name)
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
      end subroutine gz_write_modes_rlm_file
!
!------------------------------------------------------------------
!
      end module gz_sph_modes_grids_file_IO
