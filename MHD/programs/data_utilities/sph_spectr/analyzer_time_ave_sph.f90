!analyzer_time_ave_sph.f90
!      module analyzer_time_ave_sph
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine initialization
!      subroutine evolution
!
!
      module analyzer_time_ave_sph
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_schmidt_poly_on_rtm
      use calypso_mpi
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
      subroutine initialization
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
      call calypso_MPI_barrier
!
      end subroutine initialization
!
! ----------------------------------------------------------------------
!
      subroutine evolution
!
      use m_t_step_parameter
      use m_spheric_parameter
      use m_ctl_params_sph_utils
      use m_sph_spectr_data
      use m_field_data_IO
      use copy_rj_phys_data_4_IO
      use set_parallel_file_name
      use cal_t_ave_sph_spectr_data
!
      integer(kind = kint) :: i_step
!
!
      call allocate_d_rj_tmp
!
      do i_step = i_step_init, i_step_number, i_step_output_ucd
!
!   Input spectr data
!
        phys_file_head = org_sph_file_head
        if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
        call sel_read_step_SPH_field_file(my_rank, i_step)
!
        if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
        call set_rj_phys_data_from_IO
!
!  evaluate energies
!
        call sum_sph_spectr_data
      end do
      call deallocate_phys_data_IO
      call deallocate_phys_data_name_IO
!
!
      call t_ave_sph_spectr_data(i_step_init, i_step_number)
      call copy_rj_all_phys_name_to_IO
      call allocate_phys_data_IO
      call copy_rj_all_phys_data_to_IO
!
      call add_int_suffix(i_step_init,                                  &
     &    org_sph_file_head, phys_file_head)
!
      if (iflag_debug.gt.0) write(*,*) 'sel_write_step_SPH_field_file'
      call sel_write_step_SPH_field_file(my_rank, i_step_number)
!
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
        end subroutine evolution
!
! ----------------------------------------------------------------------
!
      end module analyzer_time_ave_sph
