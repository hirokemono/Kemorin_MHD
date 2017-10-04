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
      use m_machine_parameter
      use m_spheric_data_sph_spetr
      use calypso_mpi
!
      use t_time_data
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
      use m_ctl_data_4_sph_utils
      use m_ctl_params_sph_utils
      use parallel_load_data_4_sph
      use set_sph_phys_address
      use copy_rj_phys_data_4_IO
      use count_num_sph_smp
      use schmidt_poly_on_rtm_grid
      use cal_rms_fields_by_sph
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_utils'
      call read_control_data_sph_utils
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_data_4_sph_utils'
      call set_ctl_data_4_sph_utils(t_SHR, SPH_dat_ss%fld, pwr_spec)
!
!       set spectr grids
!
      if (iflag_debug.gt.0) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh                                           &
     &   (SPH_dat_ss%sph, SPH_dat_ss%comms, SPH_dat_ss%groups)
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_SPH_file'
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, t_SHR%init_d%i_time_step,                    &
     &    spec_fst_param, spec_time_IO, sph_spec_IO)
!
!  -------------------------------
!
      call set_sph_sprctr_data_address(SPH_dat_ss%sph%sph_rj,           &
     &    SPH_dat_ss%ipol, SPH_dat_ss%idpdr, SPH_dat_ss%itor,           &
     &    SPH_dat_ss%fld)
!
      call init_rms_4_sph_spectr                                        &
     &   (SPH_dat_ss%sph%sph_params, SPH_dat_ss%sph%sph_rj,             &
     &    SPH_dat_ss%fld, pwr_spec, WK_pwr_spec)
!
!
      call alloc_schmidt_normalize                                      &
     &   (SPH_dat_ss%sph%sph_rlm%nidx_rlm(2),                           &
     &    SPH_dat_ss%sph%sph_rj%nidx_rj(2), leg_s)
      call copy_sph_normalization_2_rj                                  &
     &   (SPH_dat_ss%sph%sph_rj, leg_s%g_sph_rj)
!
      end subroutine initialize_ene_sph_layer
!
! ----------------------------------------------------------------------
!
      subroutine analyze_ene_sph_layer
!
      use m_ctl_params_sph_utils
      use copy_rj_phys_data_4_IO
      use output_sph_m_square_file
!
!
      integer(kind = kint) :: i_step
!
!
      do i_step = t_SHR%init_d%i_time_step, t_SHR%finish_d%i_end_step,  &
     &           t_SHR%ucd_step%increment
        t_SHR%time_d%i_time_step = i_step
!
!   Input spectr data
!
        call sel_read_step_SPH_field_file                               &
     &     (nprocs, my_rank, t_SHR%time_d%i_time_step,                  &
     &      spec_fst_param, spec_time_IO, sph_spec_IO)
!
        call set_rj_phys_data_from_IO(sph_spec_IO, SPH_dat_ss%fld)
        call copy_time_step_data(spec_time_IO, t_SHR%time_d)
!
!  evaluate energies
!
        if (iflag_debug.gt.0) write(*,*) 'cal_mean_squre_in_shell'
        call cal_mean_squre_in_shell                                    &
     &     (SPH_dat_ss%sph%sph_params, SPH_dat_ss%sph%sph_rj,           &
     &      SPH_dat_ss%ipol, SPH_dat_ss%fld, leg_s%g_sph_rj,            &
     &      pwr_spec, WK_pwr_spec)
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'write_sph_1layer_ms_spec_file'
        call write_sph_layer_ms_file(my_rank, t_SHR%time_d,             &
     &      SPH_dat_ss%sph%sph_params, pwr_spec)
      end do
!
      end subroutine analyze_ene_sph_layer
!
! ----------------------------------------------------------------------
!
      end module analyzer_ene_sph_layer
