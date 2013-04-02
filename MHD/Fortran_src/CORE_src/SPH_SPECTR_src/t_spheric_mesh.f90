!t_spheric_mesh.f90
!      module t_spheric_mesh
!
!     Written by H. Matsui on July, 2007
!
!> @brief Structure for grid and comm table for spherical transform
!
      module t_spheric_mesh
!
      use m_precision
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_group_data
!
      implicit none
!
!
!> Structure for grid and comm table for spherical transform
      type sph_group_data
!      groups for grid space
        type(group_data) :: bc_rtp_grp
        type(group_data) :: radial_rtp_grp
        type(group_data) :: theta_rtp_grp
        type(group_data) :: zonal_rtp_grp
!
!      groups for sphectral space
        type(group_data) :: radial_rj_grp
        type(group_data) :: sphere_rj_grp
      end type sph_group_data
!
!
      type sph_mesh_data
        type(sph_grids) ::       sph_mesh
        type(sph_comm_tables) :: sph_comms
        type(sph_group_data) ::  sph_grps
      end type sph_mesh_data
!
!
      end module t_spheric_mesh
