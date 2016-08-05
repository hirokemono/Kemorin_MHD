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
      use m_spheric_data_sph_spetr
      use m_schmidt_poly_on_rtm
      use calypso_mpi
!
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
      subroutine initialization
!
      use m_t_step_parameter
      use m_ctl_data_4_sph_utils
      use m_ctl_params_sph_utils
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
     &   (iflag_org_sph_file_fmt, org_sph_file_head, sph_spec_IO)
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, i_step_init, sph_spec_IO)
!
!  -------------------------------
!
      call alloc_phys_data_type                                         &
     &   (sph_mesh_spec%sph%sph_rj%nnod_rj, rj_fld_spec)
!
      call init_rms_4_sph_spectr                                        &
     &   (sph_mesh_spec%sph%sph_params%l_truncation,                    &
     &    sph_mesh_spec%sph%sph_rj, rj_fld_spec, pwr_spec, WK_pwr_spec)
!
      end subroutine initialization
!
! ----------------------------------------------------------------------
!
      subroutine evolution
!
      use m_t_step_parameter
      use m_ctl_params_sph_utils
      use copy_rj_phys_data_4_IO
      use pickup_sph_spectr_data
      use picked_sph_spectr_data_IO
!
      integer(kind = kint) :: i_step
!
!
      call init_sph_spec_4_monitor                                      &
     &   (sph_mesh_spec%sph%sph_params%l_truncation,                    &
     &    sph_mesh_spec%sph%sph_rj, rj_fld_spec,                        &
     &    pick_list_u, pick_sph_u)
!
      do i_step = i_step_init, i_step_number, i_step_output_ucd
!
!   Input spectr data
!
        call set_field_file_fmt_prefix                                  &
     &     (iflag_org_sph_file_fmt, org_sph_file_head, sph_spec_IO)
        call sel_read_step_SPH_field_file                               &
     &     (nprocs, my_rank, i_step, sph_spec_IO)
        call set_rj_phys_data_from_IO                                   &
     &     (sph_mesh_spec%sph%sph_rj%nnod_rj, sph_spec_IO, rj_fld_spec)
!
!  pickup components
!
        call pickup_sph_spec_4_monitor(sph_mesh_spec%sph%sph_rj,        &
     &      rj_fld_spec%n_point, rj_fld_spec%num_phys,                  &
     &      rj_fld_spec%ntot_phys, rj_fld_spec%istack_component,        &
     &      rj_fld_spec%d_fld, pick_sph_u)
        call write_sph_spec_monitor                                     &
     &     (pickup_sph_head, my_rank, i_step, time, pick_sph_u)
      end do
!
      end subroutine evolution
!
! ----------------------------------------------------------------------
!
      end module analyzer_pickup_mode_sph
