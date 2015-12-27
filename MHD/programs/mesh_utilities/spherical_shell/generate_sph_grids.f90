!
      program generate_sph_grids
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_sph_trans_comm_tbl
!
      use m_geometry_data
      use m_read_ctl_gen_sph_shell
      use m_spheric_global_ranks
      use m_spheric_parameter
      use m_read_mesh_data
      use m_node_id_spherical_IO
      use gen_sph_grids_modes
      use set_ctl_gen_shell_grids
      use set_global_spherical_param
      use const_sph_radial_grid
      use const_global_sph_grids_modes
      use set_comm_table_rtp_rj
      use single_gen_sph_grids_modes
!
      use const_surface_mesh
!
      implicit none
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm(:)
      type(sph_comm_tbl), allocatable :: comm_rtm(:)
!
!
      call read_control_4_gen_shell_grids
      call s_set_control_4_gen_shell_grids
!
      call set_global_sph_resolution
!
      call check_global_spheric_parameter
      call output_set_radial_grid
!
!  ========= Generate spherical harmonics table ========================
!
      if(iflag_debug .gt. 0) write(*,*) 'count_nod_ele_4_sph_radial'
      call s_const_global_sph_grids_modes
!
      if(iflag_debug .gt. 0) write(*,*) 'gen_sph_rlm_grids'
      allocate(comm_rlm(ndomain_sph))
!
      call gen_sph_rlm_grids(ndomain_sph, comm_rlm)
      if(iflag_debug .gt. 0) write(*,*) 'gen_sph_rj_modes'
      call gen_sph_rj_modes(ndomain_sph, comm_rlm)
      call dealloc_all_comm_stacks_rlm(ndomain_sph, comm_rlm)
      deallocate(comm_rlm)
!
      allocate(comm_rtm(ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*) 'gen_sph_rtm_grids'
      call gen_sph_rtm_grids(ndomain_sph, comm_rtm)
      if(iflag_debug .gt. 0) write(*,*) 'gen_sph_rtp_grids'
      call gen_sph_rtp_grids(ndomain_sph, comm_rtm)
      call dealloc_all_comm_stacks_rtm(ndomain_sph, comm_rtm)
      deallocate(comm_rtm)
!
      if(iflag_debug .gt. 0) write(*,*) 'gen_fem_mesh_for_sph'
      call gen_fem_mesh_for_sph(ndomain_sph)
!
      if(iflag_shell_mode .lt. iflag_MESH_same) then
        stop "*** spherical shell mesh done"
      end if
!
!  ========= Construct subdomain information for viewer ==============
!
      if(iflag_excluding_FEM_mesh .eq. 0) then
        call choose_surface_mesh(sph_file_head, ele1, surf1, edge1)
      end if
!
      stop 'program is normally finished'
!
      end program generate_sph_grids
