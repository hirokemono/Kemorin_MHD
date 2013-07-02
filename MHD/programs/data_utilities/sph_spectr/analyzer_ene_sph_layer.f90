!analyzer_ene_sph_layer.f90
!      module analyzer_ene_sph_layer
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine initialize_ene_sph_layer
!      subroutine analyze_ene_sph_layer
!
      module analyzer_ene_sph_layer
!
      use m_precision
      use m_constants
      use m_parallel_var_dof
      use m_machine_parameter
      use m_schmidt_poly_on_rtm
!
      use cal_rms_fields_by_sph
      use field_IO_select
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_ene_sph_layer
!
      use m_t_step_parameter
      use m_ctl_data_4_sph_utils
      use m_ctl_params_sph_utils
      use m_node_phys_address
      use m_sph_spectr_data
      use m_sph_phys_address
      use load_mesh_data
      use const_mesh_info
      use load_data_for_sph_IO
      use set_ucd_data
      use copy_rj_phys_data_4_IO
      use count_num_sph_smp
      use set_phys_name_4_sph_trans
      use init_sph_trans
      use cal_rms_fields_by_sph
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_utils'
      call read_control_data_sph_utils
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_data_4_sph_utils'
      call set_ctl_data_4_sph_utils
!
      call time_prog_barrier
!
!       set mesh informations
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
      call time_prog_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
      call time_prog_barrier
!
!       set spectr grids
!
      if (iflag_debug.gt.0) write(*,*) 'input_sph_trans_grids'
      call input_sph_trans_grids(my_rank)
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_SPH_file'
      call sel_read_alloc_step_SPH_file(my_rank, i_step_init)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp
!
!  -------------------------------
!
      call allocate_phys_rj_data
      call set_sph_sprctr_data_address
      call set_sph_nod_data_address
!
      call init_rms_4_sph_spectr
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      call initialize_sph_trans
!
      call time_prog_barrier
!      call check_schmidt_poly_rtm(my_rank+40)
!
      end subroutine initialize_ene_sph_layer
!
! ----------------------------------------------------------------------
!
      subroutine analyze_ene_sph_layer
!
      use m_t_step_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_ctl_params_sph_utils
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use copy_rj_phys_data_4_IO
      use output_sph_rms_one_layer
!
!
      integer(kind = kint) :: i_step
!
!
      do i_step = i_step_init, i_step_number, i_step_output_ucd
!
!   Input spectr data
!
        call sel_read_step_SPH_field_file(my_rank, i_step)
!
        call set_rj_phys_data_from_IO
        call time_prog_barrier
!
!  evaluate energies
!
        if (iflag_debug.gt.0) write(*,*) 'cal_rms_sph_spec_rms_whole'
        call cal_rms_sph_spec_rms_whole
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'write_sph_1layer_ms_file'
        call write_sph_1layer_ms_file(my_rank, i_step, time)
        call write_sph_1layer_ms_spec_file(my_rank, i_step, time)
      end do
!
      end subroutine analyze_ene_sph_layer
!
! ----------------------------------------------------------------------
!
      end module analyzer_ene_sph_layer
