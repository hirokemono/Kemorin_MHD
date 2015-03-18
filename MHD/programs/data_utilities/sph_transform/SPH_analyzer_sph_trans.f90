!SPH_analyzer_sph_trans.f90
!     module SPH_analyzer_sph_trans
!
!      Written by H. Matsui
!
!      subroutine SPH_initialize_sph_trans
!
!      subroutine SPH_analyze_sph_trans(i_step)
!      subroutine SPH_analyze_sph_zm_trans(i_step)
!
      module SPH_analyzer_sph_trans
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_sph_trans
!
      use m_t_step_parameter
      use m_ctl_params_sph_trans
      use m_node_id_spherical_IO
      use m_field_data_IO
      use m_sph_spectr_data
!
      use count_num_sph_smp
      use set_phys_name_4_sph_trans
      use init_sph_trans
      use legendre_transform_select
      use sph_transfer_all_field
!
!  ---- allocate spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp
!
      call allocate_phys_rj_data
!
!  ---- initialize spherical harmonics transform
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      if(id_legendre_transfer.eq.iflag_leg_undefined)                   &
     &            id_legendre_transfer = iflag_leg_orginal_loop
      call copy_sph_trans_nums_from_rtp
      call initialize_sph_trans
!
      call allocate_d_rtp_4_all_trans
!
      end subroutine SPH_initialize_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_sph_trans(i_step)
!
      use m_sph_spectr_data
      use m_t_step_parameter
      use m_control_params_sph_data
      use m_time_data_IO
      use m_field_data_IO
      use m_node_id_spherical_IO
!
      use field_IO_select
      use copy_rj_phys_data_4_IO
      use sph_transfer_all_field
!
!
      integer(kind = kint), intent(in) :: i_step
!
!
!  spherical transform for vector
       call sph_f_trans_all_field
!
!      call check_rj_spectr_data(my_rank)
!
!     data output
!
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'copy_rj_all_phys_data_to_IO'
      call copy_rj_all_phys_data_to_IO
!
      i_time_step_IO = 0
      time_IO = zero
      delta_t_IO = zero
      call set_spectr_prefix_fmt_2_fld_IO
      call sel_write_step_SPH_field_file(my_rank, i_step)
!
      end subroutine SPH_analyze_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_sph_zm_trans(i_step)
!
      use m_sph_spectr_data
      use m_t_step_parameter
      use m_time_data_IO
      use m_field_data_IO
      use m_node_id_spherical_IO
!      use m_schmidt_poly_on_rtm
!
      use field_IO_select
      use copy_rj_phys_data_4_IO
!
      use sph_transfer_all_field
      use cal_zonal_mean_sph_spectr
!
      integer(kind = kint), intent(in) :: i_step
!
!
!  spherical transform for vector
       call sph_f_trans_all_field
!
!      call check_rj_spectr_data(my_rank)
!
!  pick zonal mean
!
        if (iflag_debug.gt.0)  write(*,*) 'zonal_mean_all_sph_spectr'
        call zonal_mean_all_sph_spectr
!
!     data output
!
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'copy_rj_all_phys_data_to_IO'
      call copy_rj_all_phys_data_to_IO
!
      i_time_step_IO = 0
      time_IO = zero
      delta_t_IO = zero
      call sel_write_step_SPH_field_file(my_rank, i_step)
!
      end subroutine SPH_analyze_sph_zm_trans
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_sph_trans
