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
      use calypso_mpi
!
      use m_schmidt_poly_on_rtm
      use t_field_data_IO
      use cal_rms_fields_by_sph
      use field_IO_select
!
      implicit none
!
      type(field_IO), save, private :: sph_fld_IO
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
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_group_data_sph_specr
      use m_sph_spectr_data
      use m_sph_phys_address
      use parallel_load_data_4_sph
      use copy_rj_phys_data_4_IO
      use count_num_sph_smp
      use init_sph_trans
      use legendre_transform_select
      use cal_rms_fields_by_sph
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_utils'
      call read_control_data_sph_utils
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_data_4_sph_utils'
      call set_ctl_data_4_sph_utils(rj_fld1)
!
!       set spectr grids
!
      if (iflag_debug.gt.0) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh                                           &
     &   (sph_param1, sph1%sph_rtp, sph_rtm1, sph_rlm1, sph1%sph_rj,        &
     &    comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1, bc_rtp_grp1,       &
     &    radial_rtp_grp1, theta_rtp_grp1, zonal_rtp_grp,               &
     &    radial_rj_grp1, sphere_rj_grp1)
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_SPH_file'
      call set_field_file_fmt_prefix                                    &
     &    (iflag_org_sph_file_fmt, org_sph_file_head, sph_fld_IO)
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, i_step_init, sph_fld_IO)
!
!  -------------------------------
!
      call set_sph_sprctr_data_address(sph1%sph_rj, rj_fld1)
!
      call init_rms_4_sph_spectr                                        &
     &   (sph_param1%l_truncation, sph1%sph_rj, rj_fld1)
!
!
      end subroutine initialize_ene_sph_layer
!
! ----------------------------------------------------------------------
!
      subroutine analyze_ene_sph_layer
!
      use m_t_step_parameter
      use m_spheric_parameter
      use m_ctl_params_sph_utils
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
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
     &     (iflag_org_sph_file_fmt, org_sph_file_head, sph_fld_IO)
        call sel_read_step_SPH_field_file                               &
     &     (nprocs, my_rank, i_step, sph_fld_IO)
!
        call set_rj_phys_data_from_IO                                   &
     &     (sph1%sph_rj%nnod_rj, sph_fld_IO, rj_fld1)
!
!  evaluate energies
!
        if (iflag_debug.gt.0) write(*,*) 'cal_mean_squre_in_shell'
        call cal_mean_squre_in_shell(ione, sph1%sph_rj%nidx_rj(1),      &
     &      sph_param1%l_truncation, sph1%sph_rj, rj_fld1)
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'write_sph_1layer_ms_spec_file'
        call write_sph_layer_ms_file                                    &
     &     (my_rank, i_step, time, sph_param1%l_truncation,             &
     &      sph_param1%nlayer_ICB, sph_param1%nlayer_CMB)
      end do
!
      end subroutine analyze_ene_sph_layer
!
! ----------------------------------------------------------------------
!
      end module analyzer_ene_sph_layer
