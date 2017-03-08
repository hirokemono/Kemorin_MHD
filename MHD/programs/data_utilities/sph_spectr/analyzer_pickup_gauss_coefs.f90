!analyzer_pickup_gauss_coefs.f90
!      module analyzer_pickup_gauss_coefs
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine initialize_pick_gauss_coef
!      subroutine analyze_pick_gauss_coef
!
      module analyzer_pickup_gauss_coefs
!
      use m_precision
      use m_machine_parameter
      use m_schmidt_poly_on_rtm
      use m_spheric_data_sph_spetr
      use calypso_mpi
!
      use t_time_data_IO
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
      subroutine initialize_pick_gauss_coef
!
      use m_t_step_parameter
      use m_ctl_data_4_sph_utils
      use m_ctl_params_sph_utils
      use parallel_load_data_4_sph
      use set_sph_phys_address
      use copy_rj_phys_data_4_IO
      use count_num_sph_smp
      use init_sph_trans
      use legendre_transform_select
      use cal_rms_fields_by_sph
!
!     --------------------- 
!
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_utils'
      call read_control_data_sph_utils
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_data_4_sph_utils'
      call set_ctl_data_4_sph_utils                                     &
     &   (rst_step1, ucd_step1, rj_fld_spec, pwr_spec)
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
     &     (iflag_org_sph_file_fmt, org_sph_file_head, sph_spec_IO)
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, i_step_init, spec_time_IO, sph_spec_IO)
!
!  -------------------------------
!
      call alloc_phys_data_type                                         &
     &   (sph_mesh_spec%sph%sph_rj%nnod_rj, rj_fld_spec)
!
      call init_rms_4_sph_spectr                                        &
     &   (sph_mesh_spec%sph%sph_params,                                 &
     &    sph_mesh_spec%sph%sph_rj, rj_fld_spec, pwr_spec, WK_pwr_spec)
!
      call set_sph_sprctr_data_address(sph_mesh_spec%sph%sph_rj,        &
     &    ipol_spec, idpdr_spec, itor_spec, rj_fld_spec)
!
      end subroutine initialize_pick_gauss_coef
!
! ----------------------------------------------------------------------
!
      subroutine analyze_pick_gauss_coef
!
      use m_t_step_parameter
      use m_ctl_params_sph_utils
!
      use copy_rj_phys_data_4_IO
      use gauss_coefs_monitor_IO
      use pickup_gauss_coefficients
!
      integer(kind = kint) :: i_step
!
!
      call init_gauss_coefs_4_monitor                                   &
     &   (sph_mesh_spec%sph%sph_params%l_truncation,                    &
     &    sph_mesh_spec%sph%sph_rj, ipol_spec, gauss_list_u, gauss_u)
      do i_step = i_step_init, i_step_number, ucd_step1%increment
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
!  pickup components
!
        call cal_gauss_coefficients                                     &
     &     (sph_mesh_spec%sph%sph_params%nlayer_ICB,                    &
     &      sph_mesh_spec%sph%sph_params%nlayer_CMB,                    &
     &      sph_mesh_spec%sph%sph_rj%nidx_rj,                           &
     &      sph_mesh_spec%sph%sph_rj%radius_1d_rj_r, ipol_spec,         &
     &      rj_fld_spec%n_point, rj_fld_spec%ntot_phys,                 &
     &      rj_fld_spec%d_fld, gauss_u)
        call write_gauss_coefs_4_monitor                                &
     &     (my_rank, i_step, time, gauss_coefs_file_prefix, gauss_u)
      end do
!
      end subroutine analyze_pick_gauss_coef
!
! ----------------------------------------------------------------------
!
      end module analyzer_pickup_gauss_coefs
