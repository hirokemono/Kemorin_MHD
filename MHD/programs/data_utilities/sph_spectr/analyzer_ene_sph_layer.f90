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
      use m_schmidt_poly_on_rtm
      use m_spheric_data_sph_spetr
      use calypso_mpi
!
      use t_time_data_IO
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
      call set_ctl_data_4_sph_utils(rj_fld_spec, pwr_spec)
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
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, i_step_init, spec_time_IO, sph_spec_IO)
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
!
      call alloc_schmidt_normalize                                      &
     &   (sph_mesh_spec%sph%sph_rlm%nidx_rlm(2),                        &
     &    sph_mesh_spec%sph%sph_rj%nidx_rj(2), leg_s)
      call copy_sph_normalization_2_rj                                  &
     &   (sph_mesh_spec%sph%sph_rj, leg_s%g_sph_rj)
!
      end subroutine initialize_ene_sph_layer
!
! ----------------------------------------------------------------------
!
      subroutine analyze_ene_sph_layer
!
      use m_t_step_parameter
      use m_ctl_params_sph_utils
      use m_schmidt_poly_on_rtm
      use copy_rj_phys_data_4_IO
      use output_sph_m_square_file
!
!
      integer(kind = kint) :: i_step
!
!
      do i_step = i_step_init, i_step_number, i_step_output_ucd
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
        if (iflag_debug.gt.0) write(*,*) 'cal_mean_squre_in_shell'
        call cal_mean_squre_in_shell                                    &
     &     (sph_mesh_spec%sph%sph_params%l_truncation,                  &
     &      sph_mesh_spec%sph%sph_rj, ipol_spec, rj_fld_spec,           &
     &      leg_s%g_sph_rj, pwr_spec, WK_pwr_spec)
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'write_sph_1layer_ms_spec_file'
        call write_sph_layer_ms_file(my_rank, i_step, time,             &
     &      sph_mesh_spec%sph%sph_params, pwr_spec)
      end do
!
      end subroutine analyze_ene_sph_layer
!
! ----------------------------------------------------------------------
!
      end module analyzer_ene_sph_layer
