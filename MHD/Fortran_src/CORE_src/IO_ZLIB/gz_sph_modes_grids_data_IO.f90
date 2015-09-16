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
      use gz_sph_rtp_groups_IO
!
!
!      write(*,*) '! domain and communication'
      call read_domain_info_gz
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph_gz
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph_gz
!
!      write(*,*) '! global ID for each direction'
      call read_rtp_gl_1d_table_gz
!
!      write(*,*) '! global radial ID and grid ID'
      call read_gl_nodes_sph_gz
!
!      write(*,*) '! communication table between spectr data'
      call read_import_data_gz
!
!      write(*,*) '! Group data'
      call read_geom_rtp_groups_gz
!
      end subroutine read_geom_rtp_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_data_gz
!
      use gz_sph_rj_groups_IO
!
!
!      write(*,*) '! domain and communication'
      call read_domain_info_gz
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph_gz
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph_gz
!
!      write(*,*) '! global ID for each direction'
      call read_rj_gl_1d_table_gz
!
!      write(*,*) '! global radial ID and spectr ID'
      call read_gl_nodes_sph_gz
!
!      write(*,*) '! communication table between spectr data'
      call read_import_data_gz
!
!      write(*,*) '! Group data'
      call read_modes_rj_groups_gz
!
      end subroutine read_spectr_modes_rj_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_data_gz
!
!
      call read_domain_info_gz
      call read_gl_resolution_sph_gz
      call read_rank_4_sph_gz
      call read_rtp_gl_1d_table_gz
      call read_gl_nodes_sph_gz
!
      call read_import_data_gz
!
      end subroutine read_geom_rtm_data_gz
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rlm_data_gz
!
!
      call read_domain_info_gz
      call read_gl_resolution_sph_gz
      call read_rank_4_sph_gz
      call read_rj_gl_1d_table_gz
      call read_gl_nodes_sph_gz
!
      call read_import_data_gz
!
      end subroutine read_spectr_modes_rlm_data_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_data_gz
!
      use m_sph_modes_grid_labels
      use gz_sph_rtp_groups_IO
!
!
      textbuf = hd_sph_para() // char(0)
      call gz_write_textbuf_no_lf
      call write_domain_info_gz
!
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_gz
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_gz
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table_gz
!
      textbuf = hd_rtp_glbl() // char(0)
      call gz_write_textbuf_no_lf
      call write_gl_nodes_sph_gz
!
      textbuf = hd_rtp_comm() // char(0)
      call gz_write_textbuf_no_lf
      call write_import_data_gz
!
!      write(*,*) '! Group data'
      call write_geom_rtp_groups_gz
!
!      write(*,*) 'finish!!'
!
      end subroutine write_geom_rtp_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_data_gz
!
      use m_sph_modes_grid_labels
      use gz_sph_rj_groups_IO
!
!
!
      textbuf = hd_sph_para() // char(0)
      call gz_write_textbuf_no_lf
      call write_domain_info_gz
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_gz
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_gz
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table_gz
!
      textbuf = hd_rj_comm() // char(0)
      call gz_write_textbuf_no_lf
      call write_gl_nodes_sph_gz
!
      textbuf = hd_rtp_comm() // char(0)
      call gz_write_textbuf_no_lf
      call write_import_data_gz
!
!      write(*,*) '! Group data'
      call write_modes_rj_groups_gz
!
      end subroutine write_spectr_modes_rj_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_data_gz
!
      use m_sph_modes_grid_labels
!
!
      textbuf = hd_sph_para() // char(0)
      call gz_write_textbuf_no_lf
      call write_domain_info_gz
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_gz
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_gz
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table_gz
!
      textbuf = hd_rtp_glbl() // char(0)
      call gz_write_textbuf_no_lf
      call write_gl_nodes_sph_gz
!
      textbuf = hd_rtp_comm() // char(0)
      call gz_write_textbuf_no_lf
      call write_import_data_gz
!
      end subroutine write_geom_rtm_data_gz
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_data_gz
!
      use m_sph_modes_grid_labels
!
!
      textbuf = hd_sph_para() // char(0)
      call gz_write_textbuf_no_lf
      call write_domain_info_gz
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_gz
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_gz
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table_gz
!
      textbuf = hd_rlm_glbl() // char(0)
      call gz_write_textbuf_no_lf
      call write_gl_nodes_sph_gz
!
      textbuf = hd_rj_comm() // char(0)
      call gz_write_textbuf_no_lf
      call write_import_data_gz
!
      end subroutine write_modes_rlm_data_gz
!
!------------------------------------------------------------------
!
      end module gz_sph_modes_grids_data_IO
