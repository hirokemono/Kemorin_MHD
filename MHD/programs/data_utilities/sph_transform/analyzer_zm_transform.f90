!analyzer_zm_transform.f90
!      module analyzer_zm_transform
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
      module analyzer_zm_transform
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
      use m_work_time
      use t_field_data_IO
!
      use FEM_analyzer_sph_trans
      use SPH_analyzer_sph_trans
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
      subroutine init_zm_trans
!
      use m_ctl_data_4_sph_trans
      use m_ctl_params_sph_trans
      use parallel_load_data_4_sph
      use load_mesh_data
!
!     --------------------- 
!
      num_elapsed = 30
      call allocate_elapsed_times
!
! ----   read controls
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans
      call set_ctl_data_4_zm_trans
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh
!
!    Initialize FEM grid
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_sph_trans'
      call FEM_initialize_sph_trans
!
!    Initialization for spherical tranform
      if (iflag_debug.gt.0) write(*,*) 'SPH_init_sph_zm_trans'
      call SPH_initialize_sph_trans
!
!    Set field IOP array by spectr fields
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_sph_trans'
      call SPH_to_FEM_bridge_sph_trans(sph_trns_IO)
!
      end subroutine init_zm_trans
!
! ----------------------------------------------------------------------
!
      subroutine analyze_zm_trans
!
      use m_t_step_parameter
!
      integer(kind = kint) :: i_step, visval
!
!
      do i_step = i_step_init, i_step_number
!
!   Input field data
        call FEM_analyze_sph_trans(i_step, visval)
!
!   Spherical transform
        call SPH_analyze_sph_zm_trans(i_step, sph_trns_IO)
      end do
!
      call FEM_finalize_sph_trans
!
        end subroutine analyze_zm_trans
!
! ----------------------------------------------------------------------
!
      end module analyzer_zm_transform
