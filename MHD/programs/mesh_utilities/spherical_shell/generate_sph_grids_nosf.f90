!generate_sph_grids_nosf.f90
      program generate_sph_grids_nosf
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_spheric_mesh
!
      use m_read_ctl_gen_sph_shell
      use m_spheric_global_ranks
      use m_spheric_parameter
      use m_read_mesh_data
      use m_node_id_spherical_IO
      use set_ctl_gen_shell_grids
      use const_sph_radial_grid
      use set_global_spherical_param
      use const_global_sph_grids_modes
      use set_comm_table_rtp_rj
      use single_gen_sph_grids_modes
!
      implicit none
!
!>      Structure for parallel spherical mesh table
      type(sph_mesh_data), allocatable :: sph_para(:)
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
      call s_const_global_sph_grids_modes
!
      allocate(sph_para(ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*) 'gen_sph_rlm_grids'
      call gen_sph_rlm_grids(ndomain_sph, sph_para)
      if(iflag_debug .gt. 0) write(*,*) 'gen_sph_rj_modes'
      call gen_sph_rj_modes(ndomain_sph, sph_para)
      call dealloc_all_comm_stacks_rlm(ndomain_sph, sph_para)
!
      if(iflag_debug .gt. 0) write(*,*) 'gen_sph_rtm_grids'
      call gen_sph_rtm_grids(ndomain_sph, sph_para)
      if(iflag_debug .gt. 0) write(*,*) 'gen_sph_rtp_grids'
      call gen_sph_rtp_grids(ndomain_sph, sph_para)
      call dealloc_all_comm_stacks_rtm(ndomain_sph, sph_para)
!
      deallocate(sph_para)
!
      if(iflag_debug .gt. 0) write(*,*) 'gen_fem_mesh_for_sph'
      call gen_fem_mesh_for_sph(ndomain_sph)
!
      if(iflag_shell_mode .lt. iflag_MESH_same) then
        stop "*** spherical shell mesh done"
      end if
!
      write(*,*) 'program is normally finished'
!
      end program generate_sph_grids_nosf
