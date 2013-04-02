!
      program generate_equator_grid
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_read_ctl_gen_sph_shell
      use set_ctl_gen_shell_eq
!
      use m_nod_comm_table
      use m_read_mesh_data
      use set_global_sph_position
      use set_global_sph_ele_connect
      use set_node_group_global_sph
      use set_ele_group_global_sph
      use set_surf_group_global_sph
      use set_sph_grid_for_equator
      use load_data_for_sph_IO
      use load_mesh_data
!
      implicit none
!
!
      call read_control_4_gen_shell_grids
      call s_set_control_4_gen_shell_eq
!
!       generate spherical mrid data
!
      call s_set_sph_grid_for_equator
      call output_geom_rtp_sph_trans(izero)
!
!       from s_const_global_sphere_mesh
!
      num_neib = 0
      call set_sph_node_position_no_pole
      call set_sph_ele_connect_no_pole
      call set_node_grp_sph_no_pole
      call set_ele_grp_sph_no_pole
      call set_surf_grp_sph_no_pole
!
      call output_mesh(izero)
!
      end program generate_equator_grid
