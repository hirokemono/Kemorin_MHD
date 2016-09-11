!gz_sph_modes_grids_data_IO.f90
!      module gz_sph_modes_grids_data_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_geom_rtp_data_gz
!      subroutine read_spectr_modes_rj_data_gz
!      subroutine read_geom_rtm_data_gz
!      subroutine read_spectr_modes_rlm_data_gz
!
!      subroutine write_geom_rtp_data_gz
!      subroutine write_spectr_modes_rj_data_gz
!      subroutine write_geom_rtm_data_gz
!      subroutine write_modes_rlm_data_gz
!
      module gz_sph_modes_grids_data_IO
!
      use m_precision
!
      use gz_domain_data_IO
      use gz_spherical_model_IO
      use gz_sph_global_1d_idx_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_data_gz
!
      use m_comm_data_IO
      use m_group_data_sph_specr_IO
      use gz_sph_rj_groups_IO
!
!
      sph_IO1%numdir_sph =  3
!
!      write(*,*) '! domain and communication'
      call read_domain_info_gz(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph_gz(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph_gz(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call read_rtp_gl_1d_table_gz(sph_IO1)
!
!      write(*,*) '! global radial ID and grid ID'
      call read_gl_nodes_sph_gz(sph_IO1)
!
!      write(*,*) '! communication table between spectr data'
      call read_import_data_gz(comm_IO)
!
!      write(*,*) 'read_rtp_node_grp_data_gz'
      call read_group_data_gz(sph_grp_IO%bc_rtp_grp)
!      write(*,*) 'read_rtp_radial_grp_data_gz'
      call read_group_data_gz(sph_grp_IO%radial_rtp_grp)
!      write(*,*) 'read_rtp_theta_grp_data_gz'
      call read_group_data_gz(sph_grp_IO%theta_rtp_grp)
!      write(*,*) 'read_rtp_zonal_grp_data_gz'
      call read_group_data_gz(sph_grp_IO%zonal_rtp_grp)
!
      end subroutine read_geom_rtp_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_data_gz
!
      use m_comm_data_IO
      use m_group_data_sph_specr_IO
      use gz_sph_rj_groups_IO
!
!
      sph_IO1%numdir_sph =  2
!
!      write(*,*) '! domain and communication'
      call read_domain_info_gz(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph_gz(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph_gz(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call read_rj_gl_1d_table_gz(sph_IO1)
!
!      write(*,*) '! global radial ID and spectr ID'
      call read_gl_nodes_sph_gz(sph_IO1)
!
!      write(*,*) '! communication table between spectr data'
      call read_import_data_gz(comm_IO)
!
!      write(*,*) '! Group data'
      call read_group_data_gz(sph_grp_IO%radial_rj_grp)
      call read_group_data_gz(sph_grp_IO%sphere_rj_grp)
!
      end subroutine read_spectr_modes_rj_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_data_gz
!
      use m_comm_data_IO
!
!
      sph_IO1%numdir_sph =  3
!
      call read_domain_info_gz(my_rank_IO, comm_IO)
      call read_gl_resolution_sph_gz(sph_IO1)
      call read_rank_4_sph_gz(sph_IO1)
      call read_rtp_gl_1d_table_gz(sph_IO1)
      call read_gl_nodes_sph_gz(sph_IO1)
!
      call read_import_data_gz(comm_IO)
!
      end subroutine read_geom_rtm_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rlm_data_gz
!
      use m_comm_data_IO
!
!
      sph_IO1%numdir_sph =  2
!
      call read_domain_info_gz(my_rank_IO, comm_IO)
      call read_gl_resolution_sph_gz(sph_IO1)
      call read_rank_4_sph_gz(sph_IO1)
      call read_rj_gl_1d_table_gz(sph_IO1)
      call read_gl_nodes_sph_gz(sph_IO1)
!
      call read_import_data_gz(comm_IO)
!
      end subroutine read_spectr_modes_rlm_data_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_data_gz
!
      use m_comm_data_IO
      use m_sph_modes_grid_labels
      use m_group_data_sph_specr_IO
      use gz_sph_rj_groups_IO
!
!
      textbuf = hd_sph_para() // char(0)
      call gz_write_textbuf_no_lf
      call write_domain_info_gz(my_rank_IO, comm_IO)
!
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_gz(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_gz(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table_gz(sph_IO1)
!
      textbuf = hd_rtp_glbl() // char(0)
      call gz_write_textbuf_no_lf
      call write_gl_nodes_sph_gz(sph_IO1)
!
      textbuf = hd_rtp_comm() // char(0)
      call gz_write_textbuf_no_lf
      call write_import_data_gz(comm_IO)
!
      textbuf = hd_grphd() // char(0)
      call gz_write_textbuf_no_lf
!
      textbuf = hd_ngrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(sph_grp_IO%bc_rtp_grp)
!
      textbuf = hd_rgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(sph_grp_IO%radial_rtp_grp)
!
      textbuf = hd_tgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(sph_grp_IO%theta_rtp_grp)
!
      textbuf = hd_pgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(sph_grp_IO%zonal_rtp_grp)
!
!      write(*,*) 'finish!!'
!
      end subroutine write_geom_rtp_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_data_gz
!
      use m_comm_data_IO
      use m_sph_modes_grid_labels
      use m_group_data_sph_specr_IO
      use gz_sph_rj_groups_IO
!
!
!
      textbuf = hd_sph_para() // char(0)
      call gz_write_textbuf_no_lf
      call write_domain_info_gz(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_gz(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_gz(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table_gz(sph_IO1)
!
      textbuf = hd_rj_comm() // char(0)
      call gz_write_textbuf_no_lf
      call write_gl_nodes_sph_gz(sph_IO1)
!
      textbuf = hd_rtp_comm() // char(0)
      call gz_write_textbuf_no_lf
      call write_import_data_gz(comm_IO)
!
!
      textbuf = hd_grphd() // char(0)
      call gz_write_textbuf_no_lf
!
      textbuf = hd_kgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(sph_grp_IO%radial_rj_grp)
!
      textbuf = hd_jgrphd() // char(0)
      call gz_write_textbuf_no_lf
      call write_grp_data_gz(sph_grp_IO%sphere_rj_grp)
!
      end subroutine write_spectr_modes_rj_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_data_gz
!
      use m_comm_data_IO
      use m_sph_modes_grid_labels
!
!
      textbuf = hd_sph_para() // char(0)
      call gz_write_textbuf_no_lf
      call write_domain_info_gz(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_gz(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_gz(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table_gz(sph_IO1)
!
      textbuf = hd_rtp_glbl() // char(0)
      call gz_write_textbuf_no_lf
      call write_gl_nodes_sph_gz(sph_IO1)
!
      textbuf = hd_rtp_comm() // char(0)
      call gz_write_textbuf_no_lf
      call write_import_data_gz(comm_IO)
!
      end subroutine write_geom_rtm_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_data_gz
!
      use m_comm_data_IO
      use m_sph_modes_grid_labels
!
!
      textbuf = hd_sph_para() // char(0)
      call gz_write_textbuf_no_lf
      call write_domain_info_gz(my_rank_IO, comm_IO)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_gz(sph_IO1)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_gz(sph_IO1)
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table_gz(sph_IO1)
!
      textbuf = hd_rlm_glbl() // char(0)
      call gz_write_textbuf_no_lf
      call write_gl_nodes_sph_gz(sph_IO1)
!
      textbuf = hd_rj_comm() // char(0)
      call gz_write_textbuf_no_lf
      call write_import_data_gz(comm_IO)
!
      end subroutine write_modes_rlm_data_gz
!
!------------------------------------------------------------------
!
      end module gz_sph_modes_grids_data_IO
