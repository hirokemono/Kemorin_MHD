!>@file   sph_modes_grids_data_IO_b.f90
!!@brief  module sph_modes_grids_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Binary spectr data IO routines
!!
!!@verbatim
!!      subroutine read_geom_rtp_data_b(my_rank, file_name)
!!      subroutine read_spectr_modes_rj_data_b(my_rank, file_name)
!!      subroutine read_geom_rtm_data_b(my_rank, file_name)
!!      subroutine read_modes_rlm_data_b(my_rank, file_name)
!!
!!      subroutine write_geom_rtp_data_b(my_rank, file_name)
!!      subroutine write_spectr_modes_rj_data_b(my_rank, file_name)
!!      subroutine write_geom_rtm_data_b(my_rank, file_name)
!!      subroutine write_modes_rlm_data_b(my_rank, file_name)
!!@endverbatim
!!
!!@param my_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module sph_modes_grids_data_IO_b
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
      subroutine read_geom_rtp_data_b
!
      use m_group_data_sph_specr_IO
      use groups_IO_b
!
!
      sph_IO1%numdir_sph =  3
!
!      write(*,*) '! domain and communication'
      call read_domain_info_b(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph_b(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph_b(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call read_rtp_gl_1d_table_b(sph_IO1)
!
!      write(*,*) '! global radial ID and grid ID'
      call read_gl_nodes_sph_b(sph_IO1)
!
!      write(*,*) '! communication table for rtp'
      call read_import_data_b(comm_IO)
!
!      write(*,*) '! Group data bc_rtp_grp_IO'
      call read_group_data_b(sph_grp_IO%bc_rtp_grp)
!      write(*,*) '! Group data radial_rtp_grp_IO'
      call read_group_data_b(sph_grp_IO%radial_rtp_grp)
!      write(*,*) '! Group data theta_rtp_grp_IO'
      call read_group_data_b(sph_grp_IO%theta_rtp_grp)
!      write(*,*) '! Group data zonal_rtp_grp_IO'
      call read_group_data_b(sph_grp_IO%zonal_rtp_grp)
!
      end subroutine read_geom_rtp_data_b
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_data_b
!
      use m_group_data_sph_specr_IO
      use groups_IO_b
!
!
      sph_IO1%numdir_sph =  2
!
!      write(*,*) '! domain and communication'
      call read_domain_info_b(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph_b(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph_b(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call read_rj_gl_1d_table_b(sph_IO1)
!
!      write(*,*) '! global radial ID and spectr ID'
      call read_gl_nodes_sph_b(sph_IO1)
!
!      write(*,*) '! communication table for rj'
      call read_import_data_b(comm_IO)
!
!      write(*,*) '! Group data'
      call read_group_data_b(sph_grp_IO%radial_rj_grp)
      call read_group_data_b(sph_grp_IO%sphere_rj_grp)
!
      end subroutine read_spectr_modes_rj_data_b
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_data_b
!
!
      sph_IO1%numdir_sph =  3
!
      call read_domain_info_b(my_rank_IO, comm_IO)
      call read_gl_resolution_sph_b(sph_IO1)
      call read_rank_4_sph_b(sph_IO1)
      call read_rtp_gl_1d_table_b(sph_IO1)
      call read_gl_nodes_sph_b(sph_IO1)
!
      call read_import_data_b(comm_IO)
!
      end subroutine read_geom_rtm_data_b
!
!------------------------------------------------------------------
!
      subroutine read_modes_rlm_data_b
!
!
      sph_IO1%numdir_sph =  2
!
      call read_domain_info_b(my_rank_IO, comm_IO)
      call read_gl_resolution_sph_b(sph_IO1)
      call read_rank_4_sph_b(sph_IO1)
      call read_rj_gl_1d_table_b(sph_IO1)
      call read_gl_nodes_sph_b(sph_IO1)
!
!      write(*,*) '! communication table for rj'
      call read_import_data_b(comm_IO)
!
      end subroutine read_modes_rlm_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_data_b
!
      use m_group_data_sph_specr_IO
      use groups_IO_b
!
!
!      write(*,*) '! domain and communication'
      call write_domain_info_b(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_b(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_b(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table_b(sph_IO1)
!
!      write(*,*) '! global radial ID and grid ID'
      call write_gl_nodes_sph_b(sph_IO1)
!
!      write(*,*) '! communication table between spectr data'
      call write_import_data_b(comm_IO)
!
!      write(*,*) '! Group data'
      call write_grp_data_b(sph_grp_IO%bc_rtp_grp)
      call write_grp_data_b(sph_grp_IO%radial_rtp_grp)
      call write_grp_data_b(sph_grp_IO%theta_rtp_grp)
      call write_grp_data_b(sph_grp_IO%zonal_rtp_grp)
!
      end subroutine write_geom_rtp_data_b
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_data_b
!
      use m_group_data_sph_specr_IO
      use groups_IO_b
!
!
!      write(*,*) '! domain and communication'
      call write_domain_info_b(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_b(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_b(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table_b(sph_IO1)
!
!      write(*,*) '! global radial ID and spectr ID'
      call write_gl_nodes_sph_b(sph_IO1)
!
!      write(*,*) '! communication table between spectr data'
      call write_import_data_b(comm_IO)
!
!      write(*,*) '! Group data'
      call write_grp_data_b(sph_grp_IO%radial_rj_grp)
      call write_grp_data_b(sph_grp_IO%sphere_rj_grp)
!
      end subroutine write_spectr_modes_rj_data_b
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_data_b
!
!
      call write_domain_info_b(my_rank_IO, comm_IO)
      call write_gl_resolution_sph_b(sph_IO1)
      call write_rank_4_sph_b(sph_IO1)
      call write_rtp_gl_1d_table_b(sph_IO1)
      call write_gl_nodes_sph_b(sph_IO1)
!
      call write_import_data_b(comm_IO)
!
      end subroutine write_geom_rtm_data_b
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_data_b
!
!
      call write_domain_info_b(my_rank_IO, comm_IO)
      call write_gl_resolution_sph_b(sph_IO1)
      call write_rank_4_sph_b(sph_IO1)
      call write_rj_gl_1d_table_b(sph_IO1)
      call write_gl_nodes_sph_b(sph_IO1)
!
      call write_import_data_b(comm_IO)
!
      end subroutine write_modes_rlm_data_b
!
!------------------------------------------------------------------
!
      end module sph_modes_grids_data_IO_b
