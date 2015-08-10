!analyzer_zm_kinetic_energy.f90
!      module analyzer_zm_kinetic_energy
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine init_zm_kinetic_energy
!      subroutine analyze_zm_kinetic_energy
!
      module analyzer_zm_kinetic_energy
!
      use m_precision
      use calypso_mpi
      use m_work_time
!
      use t_field_data_IO
      use SPH_analyzer_back_trans
      use FEM_analyzer_back_trans
      use SPH_analyzer_zm_energies
      use visualizer_all
!
      implicit none
!
      type(field_IO), save, private :: sph_trns_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_zm_kinetic_energy
!
      use m_geometry_data
      use m_ctl_data_4_sph_trans
      use m_ctl_params_sph_trans
      use parallel_load_data_4_sph
      use load_mesh_data
!
!
      num_elapsed = 30
      call allocate_elapsed_times
!
!   ----  read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans
      call set_ctl_data_4_zm_energies
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_back_trans'
      call SPH_initialize_back_trans(node1%numnod, sph_trns_IO)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_visualize'
      call init_visualize
!
      end subroutine init_zm_kinetic_energy
!
! ----------------------------------------------------------------------
!
      subroutine analyze_zm_kinetic_energy
!
      use m_t_step_parameter
!
      integer(kind=kint ) :: visval, i_step
      integer(kind=kint ) :: istep_psf, istep_iso
      integer(kind=kint ) :: istep_pvr, istep_fline
!
!
      do i_step = i_step_init, i_step_number
        if (iflag_debug.gt.0) write(*,*) 'step ', i_step, 'start...'
!
        call SPH_analyze_zm_energies(i_step, visval, sph_trns_IO)
!
        call FEM_analyze_back_trans(i_step, istep_psf, istep_iso,       &
     &          istep_pvr, istep_fline, visval)
!
        if(visval .eq. 0) then
          call visualize_all(istep_psf, istep_iso,                      &
     &        istep_pvr, istep_fline)
        end if
      end do
!
      end subroutine analyze_zm_kinetic_energy
!
! ----------------------------------------------------------------------
!
      end module analyzer_zm_kinetic_energy
