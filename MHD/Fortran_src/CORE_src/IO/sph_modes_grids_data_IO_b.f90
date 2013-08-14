!sph_modes_grids_data_IO_b.f90
!      module sph_modes_grids_data_IO_b
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_geom_rtp_data_b(mesh_file_id)
!      subroutine read_spectr_modes_rj_data_b(mesh_file_id)
!      subroutine read_geom_rtm_data_b(mesh_file_id)
!      subroutine read_spectr_modes_rlm_data_b(mesh_file_id)
!
!      subroutine write_geom_rtp_data_b(id_mesh)
!      subroutine write_spectr_modes_rj_data_b(id_mesh)
!      subroutine write_geom_rtm_data_b(id_mesh)
!      subroutine write_modes_rlm_data_b(id_mesh)
!
      module sph_modes_grids_data_IO_b
!
      use m_precision
!
      use domain_data_IO
      use spherical_model_IO
      use comm_stack_item_IO
      use sph_global_1d_idx_IO
      use sph_global_1d_idx_IO_b
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_data_b(mesh_file_id)
!
      use sph_rtp_groups_IO_b
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
!      write(*,*) '! domain and communication'
      call read_domain_info_b(mesh_file_id)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph_b(mesh_file_id)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph_b(mesh_file_id)
!
!      write(*,*) '! global ID for each direction'
      call read_rtp_gl_1d_table_b(mesh_file_id)
!
!      write(*,*) '! global radial ID and grid ID'
      call read_gl_nodes_sph_b(mesh_file_id)
!
!      write(*,*) '! communication table between spectr data'
      call read_import_data_b(mesh_file_id)
!
!      write(*,*) '! Group data'
      call read_geom_rtp_groups_b(mesh_file_id)
!
      end subroutine read_geom_rtp_data_b
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_data_b(mesh_file_id)
!
      use sph_rj_groups_IO_b
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
!      write(*,*) '! domain and communication'
      call read_domain_info_b(mesh_file_id)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph_b(mesh_file_id)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph_b(mesh_file_id)
!
!      write(*,*) '! global ID for each direction'
      call read_rj_gl_1d_table_b(mesh_file_id)
!
!      write(*,*) '! global radial ID and spectr ID'
      call read_gl_nodes_sph_b(mesh_file_id)
!
!      write(*,*) '! communication table between spectr data'
      call read_import_data_b(mesh_file_id)
!
!      write(*,*) '! Group data'
      call read_modes_rj_groups_b(mesh_file_id)
!
      end subroutine read_spectr_modes_rj_data_b
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_data_b(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call read_domain_info_b(mesh_file_id)
      call read_gl_resolution_sph_b(mesh_file_id)
      call read_rank_4_sph_b(mesh_file_id)
      call read_rtp_gl_1d_table_b(mesh_file_id)
      call read_gl_nodes_sph_b(mesh_file_id)
!
      call read_import_data_b(mesh_file_id)
!
      end subroutine read_geom_rtm_data_b
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rlm_data_b(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call read_domain_info_b(mesh_file_id)
      call read_gl_resolution_sph_b(mesh_file_id)
      call read_rank_4_sph_b(mesh_file_id)
      call read_rj_gl_1d_table_b(mesh_file_id)
      call read_gl_nodes_sph_b(mesh_file_id)
!
      call read_import_data_b(mesh_file_id)
!
      end subroutine read_spectr_modes_rlm_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_data_b(id_mesh)
!
      use sph_rtp_groups_IO_b
!
      integer(kind = kint), intent(in) :: id_mesh
!
!
!      write(*,*) '! domain and communication'
      call write_domain_info_b(id_mesh)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_b(id_mesh)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_b(id_mesh)
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table_b(id_mesh)
!
!      write(*,*) '! global radial ID and grid ID'
      call write_gl_nodes_sph_b(id_mesh)
!
!      write(*,*) '! communication table between spectr data'
      call write_import_data_b(id_mesh)
!
!      write(*,*) '! Group data'
      call write_geom_rtp_groups_b(id_mesh)
!
      end subroutine write_geom_rtp_data_b
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_data_b(id_mesh)
!
      use sph_rj_groups_IO_b
!
      integer(kind = kint), intent(in) :: id_mesh
!
!
!      write(*,*) '! domain and communication'
      call write_domain_info_b(id_mesh)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph_b(id_mesh)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph_b(id_mesh)
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table_b(id_mesh)
!
!      write(*,*) '! global radial ID and spectr ID'
      call write_gl_nodes_sph_b(id_mesh)
!
!      write(*,*) '! communication table between spectr data'
      call write_import_data_b(id_mesh)
!
!      write(*,*) '! Group data'
      call write_modes_rj_groups_b(id_mesh)
!
      end subroutine write_spectr_modes_rj_data_b
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_data_b(id_mesh)
!
      integer(kind = kint), intent(in) :: id_mesh
!
      call write_domain_info_b(id_mesh)
      call write_gl_resolution_sph_b(id_mesh)
      call write_rank_4_sph_b(id_mesh)
      call write_rtp_gl_1d_table_b(id_mesh)
      call write_gl_nodes_sph_b(id_mesh)
!
      call write_import_data_b(id_mesh)
!
      end subroutine write_geom_rtm_data_b
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_data_b(id_mesh)
!
      integer(kind = kint), intent(in) :: id_mesh
!
      call write_domain_info_b(id_mesh)
      call write_gl_resolution_sph_b(id_mesh)
      call write_rank_4_sph_b(id_mesh)
      call write_rj_gl_1d_table_b(id_mesh)
      call write_gl_nodes_sph_b(id_mesh)
!
      call write_import_data_b(id_mesh)
!
      end subroutine write_modes_rlm_data_b
!
!------------------------------------------------------------------
!
      end module sph_modes_grids_data_IO_b
