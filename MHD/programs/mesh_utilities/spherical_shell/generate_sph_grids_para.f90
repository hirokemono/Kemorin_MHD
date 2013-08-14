!generate_sph_grids_para.f90
      program generate_sph_grids_para
!
      use m_precision
      use m_constants
      use m_parallel_var_dof
      use m_work_time
!
      use m_read_ctl_gen_sph_shell
      use m_parallel_sph_grids
      use m_spheric_parameter
      use m_read_mesh_data
      use m_node_id_spherical_IO
      use set_ctl_gen_shell_grids
      use const_sph_radial_grid
      use const_global_sph_grids_modes
      use para_gen_sph_grids_modes
      use const_1d_ele_connect_4_sph
!
      use tri_sph_for_coriolis
!
      implicit none
!
!
      call parallel_cal_init
      total_start = MPI_WTIME()
!
!     --------------------- 
!
      num_elapsed = 5
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                  '
      elapse_labels(2) = 'Generation of spherical transform table'
      elapse_labels(3) = 'Generation of spherical mode and grid'
      elapse_labels(4) = 'Generation of FEM mesh data'
      elapse_labels(5) = 'Generation of 3j symbol integration'
!
!
      call start_eleps_time(1)
      call read_control_4_gen_shell_grids
      call s_set_control_4_gen_shell_grids
!
      call time_prog_barrier
      if(my_rank .eq. 0) then
        call check_global_spheric_parameter
      end if
        call output_set_radial_grid
!
!  ========= Generate spherical harmonics table ========================
!
      call time_prog_barrier
      call s_const_global_sph_grids_modes
!
      if(iflag_debug .gt. 0) write(*,*) 's_const_1d_ele_connect_4_sph'
      call s_const_1d_ele_connect_4_sph
!
      call time_prog_barrier
!
      call start_eleps_time(2)
      call para_gen_sph_transfer_grids
      call end_eleps_time(2)
!
      call start_eleps_time(3)
      call para_gen_sph_modes_grids
      call end_eleps_time(3)
!
      call start_eleps_time(4)
      call para_gen_fem_mesh_for_sph
      call end_eleps_time(4)
!
!  ========= Tri-spectral harmonics integration ===================
!C===
      call start_eleps_time(5)
      if(my_rank .eq. 0)  call gaunt_coriolis(l_truncation)
      call time_prog_barrier
      call end_eleps_time(5)
      call end_eleps_time(1)
!
!  ========= Construct subdomain information for viewer ==============
!
      call output_elapsed_times
      call parallel_cal_fin
!
      write(*,*) 'program normally terminated'
!
      end program generate_sph_grids_para
