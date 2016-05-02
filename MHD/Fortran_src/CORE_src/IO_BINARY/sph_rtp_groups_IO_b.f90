!sph_rtp_groups_IO_b.f90
!      module sph_rtp_groups_IO_b
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_geom_rtp_groups_b(mesh_file_id)
!      subroutine write_geom_rtp_groups_b(mesh_file_id)
!
      module sph_rtp_groups_IO_b
!
      use m_precision
!
      use m_group_data_sph_specr_IO
      use group_data_IO_b
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_groups_b(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call read_rj_grp_data_b(mesh_file_id, bc_rtp_grp_IO)
      call read_rj_grp_data_b(mesh_file_id, radial_rtp_grp_IO)
      call read_rj_grp_data_b(mesh_file_id, theta_rtp_grp_IO)
      call read_rj_grp_data_b(mesh_file_id, zonal_rtp_grp_IO)
!
      end subroutine read_geom_rtp_groups_b
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_groups_b(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call write_rj_grp_data_b(mesh_file_id, bc_rtp_grp_IO)
      call write_rj_grp_data_b(mesh_file_id, radial_rtp_grp_IO)
      call write_rj_grp_data_b(mesh_file_id, theta_rtp_grp_IO)
      call write_rj_grp_data_b(mesh_file_id, zonal_rtp_grp_IO)
!
      end subroutine write_geom_rtp_groups_b
!
!------------------------------------------------------------------
!
      end module sph_rtp_groups_IO_b
