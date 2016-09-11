!>@file   gz_sph_modes_file_IO_b.f90
!!@brief  module gz_sph_modes_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief gzipped binary spectr data IO routines
!!
!!@verbatim
!!      subroutine gz_read_geom_rtp_file_b(my_rank, file_name)
!!      subroutine gz_read_spectr_modes_rj_file_b(my_rank, file_name)
!!      subroutine gz_read_geom_rtm_file_b(my_rank, file_name)
!!      subroutine gz_read_modes_rlm_file_b(my_rank, file_name)
!!
!!      subroutine gz_write_geom_rtp_file_b(my_rank, file_name)
!!      subroutine gz_write_spectr_modes_rj_file_b(my_rank, file_name)
!!      subroutine gz_write_geom_rtm_file_b(my_rank, file_name)
!!      subroutine gz_write_modes_rlm_file_b(my_rank, file_name)
!!@endverbatim
!!
!!@param my_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module gz_sph_modes_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_comm_data_IO
      use m_node_id_spherical_IO
      use gz_sph_modes_data_IO_b
      use skip_gz_comment
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
      subroutine gz_read_geom_rtp_file_b(my_rank, file_name)
!
      use m_group_data_sph_specr_IO
      use gz_groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped binary grid file: ', trim(file_name)
      call open_rd_gzfile_b(file_name, my_rank)
      call gz_read_geom_rtp_data_b
      call close_gzfile_f
!
      end subroutine gz_read_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_spectr_modes_rj_file_b(my_rank, file_name)
!
      use m_group_data_sph_specr_IO
      use gz_groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped binary spectr modes file: ', trim(file_name)
      call open_rd_gzfile_b(file_name, my_rank)
      call gz_read_spectr_modes_rj_data_b
      call close_gzfile_f
!
      end subroutine gz_read_spectr_modes_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_geom_rtm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped binary grid file: ', trim(file_name)
      call open_rd_gzfile_b(file_name, my_rank)
      call gz_read_geom_rtm_data_b
      call gz_read_import_data_b(comm_IO)
!
      call close_gzfile_f
!
      end subroutine gz_read_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_modes_rlm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped binary spectr modes file: ', trim(file_name)
      call open_rd_gzfile_b(file_name, my_rank)
      call gz_read_modes_rlm_data_b
      call close_gzfile_f
!
      end subroutine gz_read_modes_rlm_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtp_file_b(my_rank, file_name)
!
      use m_group_data_sph_specr_IO
      use gz_groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write gzipped binary grid file: ', trim(file_name)
      call open_wt_gzfile_b(file_name)
      call gz_write_geom_rtp_data_b
      call close_gzfile_f
!
      end subroutine gz_write_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_spectr_modes_rj_file_b(my_rank, file_name)
!
      use m_group_data_sph_specr_IO
      use gz_groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'gzipped binary spectr modes file: ', trim(file_name)
      call open_wt_gzfile_b(file_name)
      call gz_write_spectr_modes_rj_data_b
      call close_gzfile_f
!
      end subroutine gz_write_spectr_modes_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write gzipped binary grid file: ', trim(file_name)
      call open_wt_gzfile_b(file_name)
      call gz_write_geom_rtm_data_b
      call close_gzfile_f
!
      end subroutine gz_write_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_modes_rlm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write gzipped binary spectr modes file: ', trim(file_name)
      call open_wt_gzfile_b(file_name)
      call gz_write_modes_rlm_data_b
      call close_gzfile_f
!
      end subroutine gz_write_modes_rlm_file_b
!
!------------------------------------------------------------------
!
      end module gz_sph_modes_file_IO_b
