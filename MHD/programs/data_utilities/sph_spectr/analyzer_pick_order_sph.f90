!analyzer_pick_order_sph
!      module analyzer_pick_order_sph
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_pick_order_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_schmidt_poly_on_rtm
      use m_parallel_var_dof
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
      subroutine init_analyzer
!
      use m_t_step_parameter
      use m_ctl_data_4_sph_utils
      use m_ctl_params_sph_utils
      use m_sph_spectr_data
      use m_sph_phys_address
      use load_data_for_sph_IO
      use copy_rj_phys_data_4_IO
      use count_num_sph_smp
!
!     --------------------- 
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
!       set spectr grids
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rj_sph_trans'
      call input_modes_rj_sph_trans(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 's_count_num_sph_smp'
      call s_count_num_sph_smp
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_SPH_file'
      call sel_read_alloc_step_SPH_file(my_rank, i_step_init)
!
!  -------------------------------
!
      call allocate_phys_rj_data
      call set_sph_sprctr_data_address
!
!  -------------------------------
!
      call time_prog_barrier
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_t_step_parameter
      use m_spheric_parameter
      use m_ctl_params_sph_utils
      use m_sph_spectr_data
      use m_pickup_sph_spectr_data
      use copy_rj_phys_data_4_IO
      use cal_zonal_mean_sph_spectr
      use set_parallel_file_name
!
      integer(kind = kint) :: i_step
!
!
      do i_step = i_step_init, i_step_number, i_step_output_ucd
!
!   Input spectr data
!
        phys_file_head = org_sph_file_head
        call sel_read_step_SPH_field_file(my_rank, i_step)
!
        if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
        call set_rj_phys_data_from_IO
        call time_prog_barrier
!
!  pickup orders
!
        call pick_order_sph_spectr(num_pick_sph_m, idx_pick_sph_m)
        call copy_rj_all_phys_data_to_IO
!
        phys_file_head = pickup_sph_head
        call sel_write_step_SPH_field_file(my_rank, i_step)
      end do
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
        end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_pick_order_sph
