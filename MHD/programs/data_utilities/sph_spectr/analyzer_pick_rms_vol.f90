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
      use calypso_mpi
!
      use m_schmidt_poly_on_rtm
      use t_field_data_IO
      use field_IO_select
      use pickup_sph_rms_spectr 
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
      subroutine initialyze_pick_rms_vol
!
      use m_t_step_parameter
      use m_ctl_data_4_sph_utils
      use m_ctl_params_sph_utils
      use m_sph_spectr_data
      use m_sph_phys_address
      use parallel_load_data_4_sph
      use copy_rj_phys_data_4_IO
      use count_num_sph_smp
      use init_sph_trans
      use legendre_transform_select
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
      call set_ctl_data_4_sph_utils(rj_fld1)
!
!       set spectr grids
!
      if (iflag_debug.gt.0) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_SPH_file'
      call set_field_file_fmt_prefix                                    &
     &    (iflag_org_sph_file_fmt, org_sph_file_head, sph_fld_IO)
      call sel_read_alloc_step_SPH_file                                 &
     &    (nprocs, my_rank, i_step_init, sph_fld_IO)
!
!  -------------------------------
!
      call set_sph_sprctr_data_address(rj_fld1)
!
      call init_rms_4_sph_spectr(rj_fld1)
!
      call allocate_work_pick_rms_sph
!
      end subroutine initialyze_pick_rms_vol
!
! ----------------------------------------------------------------------
!
      subroutine analyze_pick_rms_vol
!
      use m_t_step_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_ctl_params_sph_utils
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_pickup_sph_spectr_data
      use m_pickup_sph_rms_data
      use copy_rj_phys_data_4_IO
!
!
      integer(kind = kint) :: i_step
!
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_rms_4_monitor'
      call init_sph_rms_4_monitor
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
        call set_rj_phys_data_from_IO(nnod_rj, sph_fld_IO, rj_fld1)
!
!  evaluate energies
!
        if (iflag_debug.gt.0) write(*,*) 'pickup_sph_rms_vol_monitor'
        call pickup_sph_rms_vol_monitor(ione, nidx_rj(1), rj_fld1)
!
        num_pick_layer = 1
        if (iflag_debug.gt.0) write(*,*) 'write_sph_rms_4_monitor'
        call write_sph_rms_4_monitor(my_rank, i_step, time)
      end do
!
      end subroutine analyze_pick_rms_vol
!
! ----------------------------------------------------------------------
!
      end module analyzer_pick_rms_vol
