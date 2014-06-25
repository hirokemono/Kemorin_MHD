!analyzer_pick_rms_sph.f90
!      module analyzer_pick_rms_sph
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine initialize_pick_rms_sph
!      subroutine analyze_pick_rms_sph
!
      module analyzer_pick_rms_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_schmidt_poly_on_rtm
      use calypso_mpi
!
      use field_IO_select
      use pickup_sph_rms_spectr 
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_pick_rms_sph
!
      use m_t_step_parameter
      use m_ctl_data_4_sph_utils
      use m_ctl_params_sph_utils
      use m_node_phys_address
      use m_sph_spectr_data
      use m_sph_phys_address
      use load_mesh_data
      use const_mesh_info
      use parallel_load_data_4_sph
      use copy_rj_phys_data_4_IO
      use count_num_sph_smp
      use set_phys_name_4_sph_trans
      use init_sph_trans
      use cal_rms_fields_by_sph
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_utils'
      call read_control_data_sph_utils
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_data_4_sph_utils'
      call set_ctl_data_4_sph_utils
!
!       set mesh informations
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
!       set spectr grids
!
      if (iflag_debug.gt.0) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh
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
      call copy_sph_trans_nums_from_rtp
      call initialize_sph_trans
!
      call allocate_work_pick_rms_sph
!
!      call check_schmidt_poly_rtm(my_rank+40)
!
      end subroutine initialize_pick_rms_sph
!
! ----------------------------------------------------------------------
!
      subroutine analyze_pick_rms_sph
!
      use m_t_step_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_ctl_params_sph_utils
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_pickup_sph_rms_data
      use copy_rj_phys_data_4_IO
!
!
      integer(kind = kint) :: i_step
!
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_rms_4_monitor'
      call init_sph_rms_4_monitor
!
      do i_step = i_step_init, i_step_number, i_step_output_ucd
!
!   Input spectr data
!
        call sel_read_step_SPH_field_file(my_rank, i_step)
!
        call set_rj_phys_data_from_IO
!
!  evaluate energies
!
        if (iflag_debug.gt.0) write(*,*) 'pickup_sph_rms_4_monitor'
        call pickup_sph_rms_4_monitor
!
        if (iflag_debug.gt.0) write(*,*) 'write_sph_rms_4_monitor'
        call write_sph_rms_4_monitor(my_rank, i_step, time)
      end do
!
      end subroutine analyze_pick_rms_sph
!
! ----------------------------------------------------------------------
!
      end module analyzer_pick_rms_sph
