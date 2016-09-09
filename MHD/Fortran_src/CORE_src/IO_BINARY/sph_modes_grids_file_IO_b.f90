!>@file   sph_modes_grids_file_IO_b.f90
!!@brief  module sph_modes_grids_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Binary spectr data IO routines
!!
!!@verbatim
!!      subroutine read_geom_rtp_file_b(my_rank, file_name)
!!      subroutine read_spectr_modes_rj_file_b(my_rank, file_name)
!!      subroutine read_geom_rtm_file_b(my_rank, file_name)
!!      subroutine read_modes_rlm_file_b(my_rank, file_name)
!!
!!      subroutine write_geom_rtp_file_b(my_rank, file_name)
!!      subroutine write_spectr_modes_rj_file_b(my_rank, file_name)
!!      subroutine write_geom_rtm_file_b(my_rank, file_name)
!!      subroutine write_modes_rlm_file_b(my_rank, file_name)
!!@endverbatim
!!
!!@param my_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module sph_modes_grids_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_comm_data_IO
      use m_node_id_spherical_IO
      use domain_data_IO_b
      use spherical_model_IO_b
      use sph_global_1d_idx_IO_b
      use binary_IO
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
      subroutine read_geom_rtp_file_b(my_rank, file_name)
!
      use m_group_data_sph_specr_IO
      use groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  3
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read binary grid file: ', trim(file_name)
      call open_read_binary_file(file_name, my_rank)
!
!      write(*,*) '! domain and communication'
      call read_domain_info_b(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph_b
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph_b
!
!      write(*,*) '! global ID for each direction'
      call read_rtp_gl_1d_table_b
!
!      write(*,*) '! global radial ID and grid ID'
      call read_gl_nodes_sph_b
!
!      write(50+my_rank,*) '! communication table for rtp'
      call read_import_data_b(comm_IO)
!
!      write(*,*) '! Group data bc_rtp_grp_IO'
      call read_group_data_b(bc_rtp_grp_IO)
!      write(*,*) '! Group data radial_rtp_grp_IO'
      call read_group_data_b(radial_rtp_grp_IO)
!      write(*,*) '! Group data theta_rtp_grp_IO'
      call read_group_data_b(theta_rtp_grp_IO)
!      write(*,*) '! Group data zonal_rtp_grp_IO'
      call read_group_data_b(zonal_rtp_grp_IO)
!
      call close_binary_file
!
      end subroutine read_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_file_b(my_rank, file_name)
!
      use m_group_data_sph_specr_IO
      use groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  2
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read binary spectr modes file: ', trim(file_name)
      call open_read_binary_file(file_name, my_rank)
!
!      write(*,*) '! domain and communication'
      call read_domain_info_b(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph_b
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph_b
!
!      write(*,*) '! global ID for each direction'
      call read_rj_gl_1d_table_b
!
!      write(*,*) '! global radial ID and spectr ID'
      call read_gl_nodes_sph_b
!
!      write(50+my_rank,*) '! communication table for rj'
      call read_import_data_b(comm_IO)
!
!      write(*,*) '! Group data'
      call read_group_data_b(radial_rj_grp_IO)
      call read_group_data_b(sphere_rj_grp_IO)
!
      call close_binary_file
!
      end subroutine read_spectr_modes_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  3
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read binary grid file: ', trim(file_name)
      call open_read_binary_file(file_name, my_rank)
!
      call read_domain_info_b(my_rank_IO, comm_IO)
      call read_gl_resolution_sph_b
      call read_rank_4_sph_b
      call read_rtp_gl_1d_table_b
      call read_gl_nodes_sph_b
!
!      write(50+my_rank,*) '! communication table for rtm'
      call read_import_data_b(comm_IO)
!
      call close_binary_file
!
      end subroutine read_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine read_modes_rlm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  2
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read binary spectr modes file: ', trim(file_name)
      call open_read_binary_file(file_name, my_rank)
!
      call read_domain_info_b(my_rank_IO, comm_IO)
      call read_gl_resolution_sph_b
      call read_rank_4_sph_b
      call read_rj_gl_1d_table_b
      call read_gl_nodes_sph_b
!
!      write(50+my_rank,*) '! communication table for rj'
      call read_import_data_b(comm_IO)
!
      call close_binary_file
!
      end subroutine read_modes_rlm_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_file_b(my_rank, file_name)
!
      use m_group_data_sph_specr_IO
      use groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write binary grid file: ', trim(file_name)
      call open_write_binary_file(file_name)
!
!      write(*,*) '! domain and communication'
      call write_domain_info_b(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_b
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_b
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table_b
!
!      write(*,*) '! global radial ID and grid ID'
      call write_gl_nodes_sph_b
!
!      write(*,*) '! communication table between spectr data'
      call write_import_data_b(comm_IO)
!
!      write(*,*) '! Group data'
      call write_grp_data_b(bc_rtp_grp_IO)
      call write_grp_data_b(radial_rtp_grp_IO)
      call write_grp_data_b(theta_rtp_grp_IO)
      call write_grp_data_b(zonal_rtp_grp_IO)
!
      call close_binary_file
!
      end subroutine write_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_file_b(my_rank, file_name)
!
      use m_group_data_sph_specr_IO
      use groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'binary spectr modes file: ', trim(file_name)
      call open_write_binary_file(file_name)
!
!      write(*,*) '! domain and communication'
      call write_domain_info_b(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_b
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_b
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table_b
!
!      write(*,*) '! global radial ID and spectr ID'
      call write_gl_nodes_sph_b
!
!      write(*,*) '! communication table between spectr data'
      call write_import_data_b(comm_IO)
!
!      write(*,*) '! Group data'
      call write_grp_data_b(radial_rj_grp_IO)
      call write_grp_data_b(sphere_rj_grp_IO)
!
      call close_binary_file
!
      end subroutine write_spectr_modes_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write binary grid file: ', trim(file_name)
      call open_write_binary_file(file_name)
!
      call write_domain_info_b(my_rank_IO, comm_IO)
      call write_gl_resolution_sph_b
      call write_rank_4_sph_b
      call write_rtp_gl_1d_table_b
      call write_gl_nodes_sph_b
!
      call write_import_data_b(comm_IO)
!
      call close_binary_file
!
      end subroutine write_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write binary spectr modes file: ', trim(file_name)
      call open_write_binary_file(file_name)
!
      call write_domain_info_b(my_rank_IO, comm_IO)
      call write_gl_resolution_sph_b
      call write_rank_4_sph_b
      call write_rj_gl_1d_table_b
      call write_gl_nodes_sph_b
!
      call write_import_data_b(comm_IO)
!
      call close_binary_file
!
      end subroutine write_modes_rlm_file_b
!
!------------------------------------------------------------------
!
      end module sph_modes_grids_file_IO_b
