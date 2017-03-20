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
      use m_schmidt_poly_on_rtm
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
      use m_t_step_parameter
      use m_ctl_data_4_sph_utils
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
      call read_control_data_sph_utils
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_data_4_sph_utils'
      call set_ctl_data_4_sph_utils                                     &
     &   (rst_step_SHR, ucd_step_SHR, viz_step_SHR,                     &
     &    rj_fld_spec, pwr_spec)
!
!       set spectr grids
!
      if (iflag_debug.gt.0) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh(sph_mesh_spec%sph,                        &
     &    sph_mesh_spec%sph_comms, sph_mesh_spec%sph_grps)
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_SPH_file'
      call set_field_file_fmt_prefix                                    &
     &    (iflag_org_sph_file_fmt, org_sph_file_head, sph_spec_IO)
      call sel_read_alloc_step_SPH_file(nprocs, my_rank,                &
     &    init_d1%i_time_step, spec_time_IO, sph_spec_IO)
!
!  -------------------------------
!
      call set_sph_sprctr_data_address(sph_mesh_spec%sph%sph_rj,        &
     &    ipol_spec, idpdr_spec, itor_spec, rj_fld_spec)
!
      call init_rms_4_sph_spectr                                        &
     &   (sph_mesh_spec%sph%sph_params,                                 &
     &    sph_mesh_spec%sph%sph_rj, rj_fld_spec, pwr_spec, WK_pwr_spec)
!
      call allocate_work_pick_rms_sph                                   &
     &   (sph_mesh_spec%sph%sph_rj%nidx_rj(1),                          &
     &    sph_mesh_spec%sph%sph_rj%nidx_rj(2))
!
      call alloc_schmidt_normalize                                      &
     &   (sph_mesh_spec%sph%sph_rlm%nidx_rlm(2),                        &
     &    sph_mesh_spec%sph%sph_rj%nidx_rj(2), leg_s)
      call copy_sph_normalization_2_rj                                  &
     &   (sph_mesh_spec%sph%sph_rj, leg_s%g_sph_rj)
!
      end subroutine initialyze_pick_rms_vol
!
! ----------------------------------------------------------------------
!
      subroutine analyze_pick_rms_vol
!
      use m_t_step_parameter
      use m_ctl_params_sph_utils
      use copy_rj_phys_data_4_IO
      use picked_sph_spectr_data_IO
!
!
      integer(kind = kint) :: i_step
!
!
      pickup_sph_rms_head = pickup_sph_head
      pick_rms1%num_layer = pick_sph_u%num_layer
      if (iflag_debug.gt.0) write(*,*) 'init_sph_rms_4_monitor'
      call init_sph_rms_4_monitor                                       &
     &   (sph_mesh_spec%sph%sph_params%l_truncation,                    &
     &    sph_mesh_spec%sph%sph_rj, pwr_spec, pick_list_u, pick_rms1)
!
      do i_step = init_d1%i_time_step, finish_d1%i_end_step,            &
     &           ucd_step_SHR%increment
!
!   Input spectr data
!
        call set_field_file_fmt_prefix                                  &
     &     (iflag_org_sph_file_fmt, org_sph_file_head, sph_spec_IO)
        call sel_read_step_SPH_field_file                               &
     &     (nprocs, my_rank, i_step, spec_time_IO, sph_spec_IO)
!
        call set_rj_phys_data_from_IO(sph_spec_IO, rj_fld_spec)
!
!  evaluate energies
!
        if (iflag_debug.gt.0) write(*,*) 'pickup_sph_rms_vol_monitor'
        call pickup_sph_rms_vol_monitor                                 &
     &     (ione, sph_mesh_spec%sph%sph_rj%nidx_rj(1),                  &
     &      sph_mesh_spec%sph%sph_rj, leg_s, ipol_spec,                 &
     &      rj_fld_spec, pwr_spec, pick_rms1)
!
        pick_sph_u%num_layer = 1
        pick_rms1%num_layer = pick_sph_u%num_layer
        pick_rms1%id_radius = pick_sph_u%id_radius
        pick_rms1%radius_gl = pick_sph_u%radius_gl
!
        if (iflag_debug.gt.0) write(*,*) 'write_sph_spec_monitor'
        call write_sph_spec_monitor(pickup_sph_rms_head, my_rank,       &
     &      i_step, time_d1%time, pick_rms1)
      end do
!
      end subroutine analyze_pick_rms_vol
!
! ----------------------------------------------------------------------
!
      end module analyzer_pick_rms_vol
