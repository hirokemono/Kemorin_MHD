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
      subroutine initialyze_pick_rms_vol
!
      use m_ctl_params_sph_utils
      use parallel_load_data_4_sph
      use set_sph_phys_address
      use copy_rj_phys_data_4_IO
      use count_num_sph_smp
      use schmidt_poly_on_rtm_grid
      use cal_rms_fields_by_sph
!
!     --------------------- 
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_utils'
      call read_control_data_sph_utils(spu_ctl1)
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_data_4_sph_utils'
      call set_ctl_data_4_sph_utils                                     &
     &   (spu_ctl1, t_SHR, SPH_dat_ss%fld, pwr_spec)
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
      call allocate_work_pick_rms_sph                                   &
     &   (SPH_dat_ss%sph%sph_rj%nidx_rj(1),                             &
     &    SPH_dat_ss%sph%sph_rj%nidx_rj(2))
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
      pick_rms1%num_layer = pick_sph_u%num_layer
      if (iflag_debug.gt.0) write(*,*) 'init_sph_rms_4_monitor'
      call init_sph_rms_4_monitor                                       &
     &   (SPH_dat_ss%sph%sph_params, SPH_dat_ss%sph%sph_rj,             &
     &    pwr_spec, pick_list_u, pick_rms1)
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
        pick_rms1%file_prefix = pick_sph_u%file_prefix
        pick_rms1%num_layer = pick_sph_u%num_layer
        pick_rms1%id_radius = pick_sph_u%id_radius
        pick_rms1%radius_gl = pick_sph_u%radius_gl
!
        if (iflag_debug.gt.0) write(*,*)                                &
     &       'append_picked_sph_vol_msq_file'
        t_SHR%time_d%i_time_step =i_step
        call append_picked_sph_vol_msq_file(t_SHR%time_d,               &
     &     SPH_dat_ss%sph%sph_params, SPH_dat_ss%sph%sph_rj, leg_s,     &
     &     SPH_dat_ss%ipol, SPH_dat_ss%fld, pick_rms1)
      end do
!
      end subroutine analyze_pick_rms_vol
!
! ----------------------------------------------------------------------
!
      end module analyzer_pick_rms_vol
