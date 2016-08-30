!
      program generate_sph_grids
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_sph_trans_comm_tbl
      use t_spheric_parameter
      use t_field_data_IO
!
      use m_read_ctl_gen_sph_shell
      use m_spheric_global_ranks
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
      type(element_data), save :: ele_pick
      type(surface_data), save :: surf_pick
      type(edge_data), save :: edge_pick
!
!>      Structure of grid and spectr data for spherical spectr method
      type(sph_grids), save :: sph_const
!>      Structure of file name and formats
      type(field_IO_params), save :: sph_file_prm_const
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
      type(sph_comm_tbl), allocatable :: comm_rtm_mul(:)
!
      integer(kind = kint) :: ierr
!
!
      call read_control_4_gen_shell_grids
      call s_set_control_4_gen_shell_grids                              &
     &   (sph_const, sph_file_prm_const, ierr)
      if(ierr .gt. 0) then
        write(*,'(a)') e_message
        stop
      end if
!
!  =========  Set global resolutions ===================================
!
      call set_global_sph_resolution                                    &
     &   (sph_const%sph_params%l_truncation,                            &
     &    sph_const%sph_params%m_folding,                               &
     &    sph_const%sph_rtp, sph_const%sph_rtm,                         &
     &    sph_const%sph_rlm, sph_const%sph_rj)
!
      call check_global_spheric_parameter                               &
     &   (sph_const%sph_params, sph_const%sph_rtp)
      call output_set_radial_grid                                       &
     &   (sph_const%sph_params, sph_const%sph_rtp)
!
!  ========= Generate spherical harmonics table ========================
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_sph_grids_modes'
      call s_const_global_sph_grids_modes                               &
     &   (sph_const%sph_params, sph_const%sph_rtp,                      &
     &    sph_const%sph_rtm, sph_const%sph_rj)
!
      if(iflag_debug .gt. 0) write(*,*) 'gen_sph_rlm_grids'
      allocate(comm_rlm_mul(ndomain_sph))
!
      call gen_sph_rlm_grids                                            &
     &   (ndomain_sph, sph_const%sph_params, sph_const%sph_rlm,         &
     &    comm_rlm_mul)
      if(iflag_debug .gt. 0) write(*,*) 'gen_sph_rj_modes'
      call gen_sph_rj_modes(ndomain_sph, comm_rlm_mul,                  &
     &    sph_const%sph_params, sph_const%sph_rlm, sph_const%sph_rj)
      call dealloc_all_comm_stacks_rlm(ndomain_sph, comm_rlm_mul)
      deallocate(comm_rlm_mul)
!
      allocate(comm_rtm_mul(ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*) 'gen_sph_rtm_grids'
      call gen_sph_rtm_grids                                            &
     &   (ndomain_sph, sph_const%sph_params, sph_const%sph_rtm,         &
     &    comm_rtm_mul)
      if(iflag_debug .gt. 0) write(*,*) 'gen_sph_rtp_grids'
      call gen_sph_rtp_grids(ndomain_sph, comm_rtm_mul,                 &
     &    sph_const%sph_params, sph_const%sph_rtp, sph_const%sph_rtm)
      call dealloc_all_comm_stacks_rtm(ndomain_sph, comm_rtm_mul)
      deallocate(comm_rtm_mul)
!
      if(iflag_debug .gt. 0) write(*,*) 'gen_fem_mesh_for_sph'
      call gen_fem_mesh_for_sph(ndomain_sph,                            &
     &    sph_const%sph_params, sph_const%sph_rj, sph_const%sph_rtp)
!
      if(sph_const%sph_params%iflag_shell_mode .lt. iflag_MESH_same)    &
     &    stop "*** spherical shell mesh done"
!
!  ========= Construct subdomain information for viewer ==============
!
      if(iflag_output_mesh .gt. 0) then
        call choose_surface_mesh                                        &
     &     (sph_file_head, ele_pick, surf_pick, edge_pick)
      end if
!
      stop 'program is normally finished'
!
      end program generate_sph_grids
