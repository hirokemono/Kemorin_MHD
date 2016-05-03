!>@file  sph_modes_grids_data_IO.f90
!!       module sph_modes_grids_data_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Routines for speherical grid data IO
!!
!!@verbatim
!!      subroutine read_geom_rtp_data(mesh_file_id)
!!      subroutine read_spectr_modes_rj_data(mesh_file_id)
!!      subroutine read_geom_rtm_data(mesh_file_id)
!!      subroutine read_spectr_modes_rlm_data(mesh_file_id)
!!
!!      subroutine write_geom_rtp_data(id_mesh)
!!      subroutine write_spectr_modes_rj_data(id_mesh)
!!      subroutine write_geom_rtm_data(id_mesh)
!!      subroutine write_modes_rlm_data(id_mesh)
!!@endverbatim
!
      module sph_modes_grids_data_IO
!
      use m_precision
!
      use domain_data_IO
      use spherical_model_IO
      use comm_stack_item_IO
      use sph_global_1d_idx_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_data(mesh_file_id)
!
      use m_group_data_sph_specr_IO
      use groups_IO
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!      write(*,*) '! domain and communication'
      call read_domain_info(mesh_file_id)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph(mesh_file_id)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph(mesh_file_id)
!
!      write(*,*) '! global ID for each direction'
      call read_rtp_gl_1d_table(mesh_file_id)
!
!      write(*,*) '! global radial ID and grid ID'
      call read_gl_nodes_sph(mesh_file_id)
!
!      write(*,*) '! communication table between spectr data'
      call read_import_data(mesh_file_id)
!
!      write(*,*) '! Group data'
      call read_group_data(mesh_file_id, bc_rtp_grp_IO)
      call read_group_data(mesh_file_id, radial_rtp_grp_IO)
      call read_group_data(mesh_file_id, theta_rtp_grp_IO)
      call read_group_data(mesh_file_id, zonal_rtp_grp_IO)
!
      end subroutine read_geom_rtp_data
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_data(mesh_file_id)
!
      use m_group_data_sph_specr_IO
      use groups_IO
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
!      write(*,*) '! domain and communication'
      call read_domain_info(mesh_file_id)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call read_gl_resolution_sph(mesh_file_id)
!      write(*,*) '! segment ID for each direction'
      call read_rank_4_sph(mesh_file_id)
!
!      write(*,*) '! global ID for each direction'
      call read_rj_gl_1d_table(mesh_file_id)
!
!      write(*,*) '! global radial ID and spectr ID'
      call read_gl_nodes_sph(mesh_file_id)
!
!      write(*,*) '! communication table between spectr data'
      call read_import_data(mesh_file_id)
!
!      write(*,*) '! Group data'
      call read_group_data(mesh_file_id, radial_rj_grp_IO)
      call read_group_data(mesh_file_id, sphere_rj_grp_IO)
!
      end subroutine read_spectr_modes_rj_data
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_data(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call read_domain_info(mesh_file_id)
      call read_gl_resolution_sph(mesh_file_id)
      call read_rank_4_sph(mesh_file_id)
      call read_rtp_gl_1d_table(mesh_file_id)
      call read_gl_nodes_sph(mesh_file_id)
!
      call read_import_data(mesh_file_id)
!
      end subroutine read_geom_rtm_data
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rlm_data(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call read_domain_info(mesh_file_id)
      call read_gl_resolution_sph(mesh_file_id)
      call read_rank_4_sph(mesh_file_id)
      call read_rj_gl_1d_table(mesh_file_id)
      call read_gl_nodes_sph(mesh_file_id)
!
      call read_import_data(mesh_file_id)
!
      end subroutine read_spectr_modes_rlm_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_data(id_mesh)
!
      use m_sph_modes_grid_labels
      use m_group_data_sph_specr_IO
      use groups_IO
!
      integer(kind = kint), intent(in) :: id_mesh
!
      write(id_mesh,'(a)', advance='NO') hd_sph_para()
      call write_domain_info(id_mesh)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph(id_mesh)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph(id_mesh)
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table(id_mesh)
!
      write(id_mesh,'(a)', advance='NO') hd_rtp_glbl()
      call write_gl_nodes_sph(id_mesh)
!
      write(id_mesh,'(a)', advance='NO') hd_rtp_comm()
      call write_import_data(id_mesh)
!
!      write(*,*) '! Group data'
!
      write(id_mesh,'(a)', advance='NO') hd_grphd()
!
      write(id_mesh,'(a)', advance='NO') hd_ngrphd()
      call write_grp_data(id_mesh, bc_rtp_grp_IO)
!
      write(id_mesh,'(a)', advance='NO') hd_rgrphd()
      call write_grp_data(id_mesh, radial_rtp_grp_IO)
!
      write(id_mesh,'(a)', advance='NO') hd_tgrphd()
      call write_grp_data(id_mesh, theta_rtp_grp_IO)
!
      write(id_mesh,'(a)', advance='NO') hd_pgrphd()
      call write_grp_data(id_mesh, zonal_rtp_grp_IO)
!
!      write(*,*) 'finish!!'
!
      end subroutine write_geom_rtp_data
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_data(id_mesh)
!
      use m_sph_modes_grid_labels
      use m_sph_modes_grid_labels
      use m_group_data_sph_specr_IO
!
      use groups_IO
!
      integer(kind = kint), intent(in) :: id_mesh
!
!
      write(id_mesh,'(a)', advance='NO') hd_sph_para()
      call write_domain_info(id_mesh)
!
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph(id_mesh)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph(id_mesh)
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table(id_mesh)
!
      write(id_mesh,'(a)', advance='NO') hd_rj_glbl()
      call write_gl_nodes_sph(id_mesh)
!
      write(id_mesh,'(a)', advance='NO') hd_rj_comm()
      call write_import_data(id_mesh)
!
!      write(*,*) '! Group data'
      write(id_mesh,'(a)', advance='NO') hd_grphd()
      write(id_mesh,'(a)', advance='NO') hd_kgrphd()
      call write_grp_data(id_mesh, radial_rj_grp_IO)
      write(id_mesh,'(a)', advance='NO') hd_jgrphd()
      call write_grp_data(id_mesh, sphere_rj_grp_IO)
!
      end subroutine write_spectr_modes_rj_data
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_data(id_mesh)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: id_mesh
!
      write(id_mesh,'(a)', advance='NO') hd_sph_para()
      call write_domain_info(id_mesh)
!      write(*,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph(id_mesh)
!      write(*,*) '! segment ID for each direction'
      call write_rank_4_sph(id_mesh)
!
!      write(*,*) '! global ID for each direction'
      call write_rtp_gl_1d_table(id_mesh)
!
      write(id_mesh,'(a)', advance='NO') hd_rtp_glbl()
      call write_gl_nodes_sph(id_mesh)
!
      write(id_mesh,'(a)', advance='NO') hd_rtp_comm()
      call write_import_data(id_mesh)
!
      end subroutine write_geom_rtm_data
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_data(id_mesh)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: id_mesh
!
      write(id_mesh,'(a)', advance='NO') hd_sph_para()
      call write_domain_info(id_mesh)
!      write(id_mesh,*) '! truncation level for spherical harmonics'
      call write_gl_resolution_sph(id_mesh)
!      write(id_mesh,*) '! segment ID for each direction'
      call write_rank_4_sph(id_mesh)
!
!      write(*,*) '! global ID for each direction'
      call write_rj_gl_1d_table(id_mesh)
!
      write(id_mesh,'(a)', advance='NO') hd_rlm_glbl()
      call write_gl_nodes_sph(id_mesh)
!
      write(id_mesh,'(a)', advance='NO') hd_rj_comm()
      call write_import_data(id_mesh)
!
      end subroutine write_modes_rlm_data
!
!------------------------------------------------------------------
!
      end module sph_modes_grids_data_IO
