!analyzer_pick_rms_vol.f90
!      module analyzer_pick_rms_vol
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine initialyze_pick_rms_vol
!      subroutine analyze_pick_rms_vol
!
      module analyzer_pick_rms_vol
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_spheric_data_sph_spetr
      use calypso_mpi
!
      use t_time_data
      use field_IO_select
!
      implicit none
!
      character (len = kchara), parameter, private                      &
     &        :: control_file_name='ctl_sph_transform'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialyze_pick_rms_vol
!
      use m_ctl_params_sph_utils
      use parallel_load_data_4_sph
      use input_control_sph_utils
      use set_field_data_w_SGS
      use copy_rj_phys_data_4_IO
      use count_num_sph_smp
      use schmidt_poly_on_rtm_grid
      use cal_rms_fields_by_sph
!
!     ---------------------
!     read controls
!     ---------------------
      if (iflag_debug.gt.0) write(*,*) 's_input_control_sph_utils'
      call s_input_control_sph_utils(control_file_name, spu_ctl1,       &
     &    t_SHR, SPH_dat_ss%fld, monitor_ss%pwr)
!
!       set spectr grids
!
      if (iflag_debug.gt.0) write(*,*) 'load_sph_mesh'
      call load_sph_mesh(files_SHR%sph_file_param,                      &
     &    SPH_dat_ss%sph, SPH_dat_ss%comms, SPH_dat_ss%groups)
      if (iflag_debug.gt.0) write(*,*) 'sph_index_flags_and_params'
      call sph_index_flags_and_params                                   &
     &   (SPH_dat_ss%groups, SPH_dat_ss%sph, SPH_dat_ss%comms)
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
      call init_field_data_w_SGS(SPH_dat_ss%sph%sph_rj%nnod_rj,         &
     &    SPH_dat_ss%fld, SPH_dat_ss%ipol, ipol_LES_ss)
!
      call init_rms_4_sph_spectr_util                                   &
     &   (SPH_dat_ss%sph, SPH_dat_ss%fld, monitor_ss)
!
      call alloc_schmidt_normalize                                      &
     &   (SPH_dat_ss%sph%sph_rlm%nidx_rlm(2),                           &
     &    SPH_dat_ss%sph%sph_rj%nidx_rj(2), leg_s)
      call copy_sph_normalization_2_rj                                  &
     &   (SPH_dat_ss%sph%sph_rj, leg_s%g_sph_rj)
!
      end subroutine initialyze_pick_rms_vol
!
! ----------------------------------------------------------------------
!
      subroutine analyze_pick_rms_vol
!
      use m_ctl_params_sph_utils
      use copy_rj_phys_data_4_IO
      use MPI_picked_sph_mean_sq_IO
!
!
      integer(kind = kint) :: i_step
!
!
      monitor_ss%pick_rms%num_layer = pick_sph_u%num_layer
      if (iflag_debug.gt.0) write(*,*) 'init_sph_rms_4_monitor'
      call init_sph_rms_4_monitor                                       &
     &   (SPH_dat_ss%sph, pick_list_u, monitor_ss)
!
      do i_step = t_SHR%init_d%i_time_step, t_SHR%finish_d%i_end_step,  &
     &           t_SHR%ucd_step%increment
!
!   Input spectr data
!
        call sel_read_step_SPH_field_file(nprocs, my_rank, i_step,      &
     &      spec_fst_param, spec_time_IO, sph_spec_IO)
!
        call set_rj_phys_data_from_IO(sph_spec_IO, SPH_dat_ss%fld)
        call copy_time_step_data(spec_time_IO, t_SHR%time_d)
!
!  evaluate energies
!
        pick_sph_u%num_layer = 1
        monitor_ss%pick_rms%file_prefix = pick_sph_u%file_prefix
        monitor_ss%pick_rms%num_layer = pick_sph_u%num_layer
        monitor_ss%pick_rms%id_radius = pick_sph_u%id_radius
        monitor_ss%pick_rms%radius_gl = pick_sph_u%radius_gl
!
        if (iflag_debug.gt.0) write(*,*)                                &
     &       'append_picked_sph_vol_msq_file'
        t_SHR%time_d%i_time_step =i_step
        call append_picked_sph_vol_msq_file(t_SHR%time_d,               &
     &      SPH_dat_ss%sph%sph_params, SPH_dat_ss%sph%sph_rj, leg_s,    &
     &      SPH_dat_ss%ipol, ipol_LES_ss, SPH_dat_ss%fld,               &
     &      monitor_ss%pick_rms)
      end do
!
      end subroutine analyze_pick_rms_vol
!
! ----------------------------------------------------------------------
!
      end module analyzer_pick_rms_vol
