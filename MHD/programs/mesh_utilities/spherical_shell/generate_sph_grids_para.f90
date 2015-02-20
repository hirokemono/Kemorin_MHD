!generate_sph_grids_para.f90
      program generate_sph_grids_para
!
      use m_precision
      use m_constants
      use m_work_time
      use calypso_mpi
      use m_machine_parameter
!
      use m_read_ctl_gen_sph_shell
      use m_spheric_parameter
      use m_read_mesh_data
      use m_node_id_spherical_IO
      use set_ctl_gen_shell_grids
      use const_sph_radial_grid
      use const_global_sph_grids_modes
      use para_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_1d_ele_connect_4_sph
!
      implicit none
!
!
      call calypso_MPI_init
      total_start = MPI_WTIME()
!
!     --------------------- 
!
      num_elapsed = 4
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                  '
      elapse_labels(2) = 'Generation of spherical transform table'
      elapse_labels(3) = 'Generation of spherical mode and grid'
      elapse_labels(4) = 'Generation of FEM mesh data'
!
!
      call start_eleps_time(1)
      call read_control_4_gen_shell_grids
      call s_set_control_4_gen_shell_grids
!
      if(my_rank .eq. 0) then
        call check_global_spheric_parameter
        call output_set_radial_grid
      end if
!
!  ========= Generate spherical harmonics table ========================
!
      call s_const_global_sph_grids_modes
!
      if(iflag_debug .gt. 0) write(*,*) 's_const_1d_ele_connect_4_sph'
      call s_const_1d_ele_connect_4_sph
!
      call alloc_parallel_sph_grids
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_grids'
      call start_eleps_time(2)
      call para_gen_sph_rlm_grids
      call end_eleps_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rj_modes'
      call start_eleps_time(3)
      call para_gen_sph_rj_modes
      call end_eleps_time(3)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtm_grids'
      call start_eleps_time(2)
      call para_gen_sph_rtm_grids
      call end_eleps_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtp_grids'
      call start_eleps_time(3)
      call para_gen_sph_rtp_grids
      call end_eleps_time(3)
!
      call start_eleps_time(4)
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_fem_mesh_for_sph'
      call para_gen_fem_mesh_for_sph
      call end_eleps_time(4)
!
      call end_eleps_time(1)
!
!  ========= Construct subdomain information for viewer ==============
!
      call output_elapsed_times
      call calypso_MPI_finalize
!
      write(*,*) 'program is normally finished'
!
      end program generate_sph_grids_para
