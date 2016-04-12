!analyzer_pickup_mode_sph.f90
!      module analyzer_pickup_mode_sph
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine initialization
!      subroutine evolution
!
      module analyzer_pickup_mode_sph
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use m_schmidt_poly_on_rtm
      use t_field_data_IO
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
      subroutine initialization
!
      use m_t_step_parameter
      use m_ctl_data_4_sph_utils
      use m_ctl_params_sph_utils
      use m_sph_spectr_data
      use parallel_load_data_4_sph
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
      call set_ctl_data_4_sph_utils
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
     &   (iflag_org_sph_file_fmt, org_sph_file_head, sph_fld_IO)
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, i_step_init, sph_fld_IO)
!
!  -------------------------------
!
      call alloc_phys_data_type(nnod_rj, rj_fld1)
!
      call init_rms_4_sph_spectr
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      if(id_legendre_transfer.eq.iflag_leg_undefined)                   &
     &            id_legendre_transfer = iflag_leg_orginal_loop
      call copy_sph_trans_nums_from_rtp
      call initialize_sph_trans
!
!      call check_schmidt_poly_rtm(my_rank+40)
!
      end subroutine initialization
!
! ----------------------------------------------------------------------
!
      subroutine evolution
!
      use m_t_step_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_ctl_params_sph_utils
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_pickup_sph_spectr_data
      use copy_rj_phys_data_4_IO
!
      use pickup_sph_coefs
!
      integer(kind = kint) :: i_step
!
!
      call init_sph_spec_4_monitor
!
      do i_step = i_step_init, i_step_number, i_step_output_ucd
!
!   Input spectr data
!
        call set_field_file_fmt_prefix                                  &
     &     (iflag_org_sph_file_fmt, org_sph_file_head, sph_fld_IO)
        call sel_read_step_SPH_field_file                               &
     &     (nprocs, my_rank, i_step, sph_fld_IO)
        call set_rj_phys_data_from_IO(sph_fld_IO)
!
!  pickup components
!
        call pickup_sph_spec_4_monitor                                  &
     &     (num_phys_rj, rj_fld1%ntot_phys,                             &
     &      rj_fld1%istack_component, rj_fld1%d_fld)
        call write_sph_spec_4_monitor(my_rank, i_step, time)
      end do
!
      end subroutine evolution
!
! ----------------------------------------------------------------------
!
      end module analyzer_pickup_mode_sph
