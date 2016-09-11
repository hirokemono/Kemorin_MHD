!>@file   gz_sph_modes_data_IO_b.f90
!!@brief  module gz_sph_modes_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief gzipped binary spectr data IO routines
!!
!!@verbatim
!!      subroutine gz_read_geom_rtp_data_b
!!      subroutine gz_read_spectr_modes_rj_data_b
!!      subroutine gz_read_geom_rtm_data_b
!!      subroutine gz_read_modes_rlm_data_b
!!
!!      subroutine gz_write_geom_rtp_data_b
!!      subroutine gz_write_spectr_modes_rj_data_b
!!      subroutine gz_write_geom_rtm_data_b
!!      subroutine gz_write_modes_rlm_data_b
!!@endverbatim
!!
!!@param my_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module gz_sph_modes_data_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_comm_data_IO
      use m_node_id_spherical_IO
      use gz_domain_data_IO_b
      use gz_spherical_model_IO_b
      use gz_sph_global_1d_idx_IO_b
      use gz_binary_IO
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
      subroutine gz_read_geom_rtp_data_b
!
      use m_group_data_sph_specr_IO
      use gz_groups_IO_b
!
!
      sph_IO1%numdir_sph =  3
!
!      write(*,*) '! domain and communication'
      call gz_read_domain_info_b(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call gz_read_gl_resolution_sph_b(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call gz_read_rank_4_sph_b(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call gz_read_rtp_gl_1d_table_b(sph_IO1)
!
!      write(*,*) '! global radial ID and grid ID'
      call gz_read_gl_nodes_sph_b(sph_IO1)
!
!      write(*,*) '! communication table for rtp'
      call gz_read_import_data_b(comm_IO)
!
!      write(*,*) '! Group data bc_rtp_grp_IO'
      call gz_read_group_data_b(sph_grp_IO%bc_rtp_grp)
!      write(*,*) '! Group data radial_rtp_grp_IO'
      call gz_read_group_data_b(sph_grp_IO%radial_rtp_grp)
!      write(*,*) '! Group data theta_rtp_grp_IO'
      call gz_read_group_data_b(sph_grp_IO%theta_rtp_grp)
!      write(*,*) '! Group data zonal_rtp_grp_IO'
      call gz_read_group_data_b(sph_grp_IO%zonal_rtp_grp)
!
      end subroutine gz_read_geom_rtp_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_spectr_modes_rj_data_b
!
      use m_group_data_sph_specr_IO
      use gz_groups_IO_b
!
!
      sph_IO1%numdir_sph =  2
!
!      write(*,*) '! domain and communication'
      call gz_read_domain_info_b(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call gz_read_gl_resolution_sph_b(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call gz_read_rank_4_sph_b(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call gz_read_rj_gl_1d_table_b(sph_IO1)
!
!      write(*,*) '! global radial ID and spectr ID'
      call gz_read_gl_nodes_sph_b(sph_IO1)
!
!      write(50+my_rank,*) '! communication table for rj'
      call gz_read_import_data_b(comm_IO)
!
!      write(*,*) '! Group data'
      call gz_read_group_data_b(sph_grp_IO%radial_rj_grp)
      call gz_read_group_data_b(sph_grp_IO%sphere_rj_grp)
!
      end subroutine gz_read_spectr_modes_rj_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_geom_rtm_data_b
!
!
      sph_IO1%numdir_sph =  3
!
      call gz_read_domain_info_b(my_rank_IO, comm_IO)
      call gz_read_gl_resolution_sph_b(sph_IO1)
      call gz_read_rank_4_sph_b(sph_IO1)
      call gz_read_rtp_gl_1d_table_b(sph_IO1)
      call gz_read_gl_nodes_sph_b(sph_IO1)
!
!      write(50+my_rank,*) '! communication table for rtm'
      call gz_read_import_data_b(comm_IO)
!
      end subroutine gz_read_geom_rtm_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_modes_rlm_data_b
!
!
      sph_IO1%numdir_sph =  2
!
      call gz_read_domain_info_b(my_rank_IO, comm_IO)
      call gz_read_gl_resolution_sph_b(sph_IO1)
      call gz_read_rank_4_sph_b(sph_IO1)
      call gz_read_rj_gl_1d_table_b(sph_IO1)
      call gz_read_gl_nodes_sph_b(sph_IO1)
!
!      write(50+my_rank,*) '! communication table for rj'
      call gz_read_import_data_b(comm_IO)
!
      end subroutine gz_read_modes_rlm_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtp_data_b
!
      use m_group_data_sph_specr_IO
      use gz_groups_IO_b
!
!
!      write(*,*) '! domain and communication'
      call gz_write_domain_info_b(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call gz_write_gl_resolution_sph_b(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call gz_write_rank_4_sph_b(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call gz_write_rtp_gl_1d_table_b(sph_IO1)
!
!      write(*,*) '! global radial ID and grid ID'
      call gz_write_gl_nodes_sph_b(sph_IO1)
!
!      write(*,*) '! communication table between spectr data'
      call gz_write_import_data_b(comm_IO)
!
!      write(*,*) '! Group data'
      call gz_write_grp_data_b(sph_grp_IO%bc_rtp_grp)
      call gz_write_grp_data_b(sph_grp_IO%radial_rtp_grp)
      call gz_write_grp_data_b(sph_grp_IO%theta_rtp_grp)
      call gz_write_grp_data_b(sph_grp_IO%zonal_rtp_grp)
!
      end subroutine gz_write_geom_rtp_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_spectr_modes_rj_data_b
!
      use m_group_data_sph_specr_IO
      use gz_groups_IO_b
!
!
!      write(*,*) '! domain and communication'
      call gz_write_domain_info_b(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call gz_write_gl_resolution_sph_b(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call gz_write_rank_4_sph_b(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call gz_write_rj_gl_1d_table_b(sph_IO1)
!
!      write(*,*) '! global radial ID and spectr ID'
      call gz_write_gl_nodes_sph_b(sph_IO1)
!
!      write(*,*) '! communication table between spectr data'
      call gz_write_import_data_b(comm_IO)
!
!      write(*,*) '! Group data'
      call gz_write_grp_data_b(sph_grp_IO%radial_rj_grp)
      call gz_write_grp_data_b(sph_grp_IO%sphere_rj_grp)
!
      end subroutine gz_write_spectr_modes_rj_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtm_data_b
!
!
      call gz_write_domain_info_b(my_rank_IO, comm_IO)
      call gz_write_gl_resolution_sph_b(sph_IO1)
      call gz_write_rank_4_sph_b(sph_IO1)
      call gz_write_rtp_gl_1d_table_b(sph_IO1)
      call gz_write_gl_nodes_sph_b(sph_IO1)
!
      call gz_write_import_data_b(comm_IO)
!
      end subroutine gz_write_geom_rtm_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_modes_rlm_data_b
!
!
      call gz_write_domain_info_b(my_rank_IO, comm_IO)
      call gz_write_gl_resolution_sph_b(sph_IO1)
      call gz_write_rank_4_sph_b(sph_IO1)
      call gz_write_rj_gl_1d_table_b(sph_IO1)
      call gz_write_gl_nodes_sph_b(sph_IO1)
!
      call gz_write_import_data_b(comm_IO)
!
      end subroutine gz_write_modes_rlm_data_b
!
!------------------------------------------------------------------
!
      end module gz_sph_modes_data_IO_b
