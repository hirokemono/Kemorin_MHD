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
      use calypso_mpi
!
      use t_time_data
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
      use m_ctl_params_sph_utils
      use m_legendre_transform_list
      use parallel_load_data_4_sph
      use copy_rj_phys_data_4_IO
      use count_num_sph_smp
      use init_sph_trans
      use cal_rms_fields_by_sph
!
!     --------------------- 
!
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_utils'
      call read_control_data_sph_utils(spu_ctl1)
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_data_4_sph_utils'
      call set_ctl_data_4_sph_utils                                     &
     &   (spu_ctl1, t_SHR, SPH_dat_ss%fld, monitor_ss%pwr)
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
      call alloc_phys_data(SPH_dat_ss%sph%sph_rj%nnod_rj,               &
     &                     SPH_dat_ss%fld)
!
      end subroutine initialization
!
! ----------------------------------------------------------------------
!
      subroutine evolution
!
      use m_ctl_params_sph_utils
      use copy_rj_phys_data_4_IO
      use cal_write_sph_monitor_data
      use pickup_sph_spectr_data
      use MPI_picked_sph_spectr_IO
!
      integer(kind = kint) :: i_step
!
!
      call init_sph_spec_4_monitor                                      &
     &   (SPH_dat_ss%sph%sph_params, SPH_dat_ss%sph%sph_rj,             &
     &    SPH_dat_ss%fld, pick_list_u, pick_sph_u)
!
      do i_step = t_SHR%init_d%i_time_step, t_SHR%finish_d%i_end_step,  &
     &           t_SHR%ucd_step%increment
!
!   Input spectr data
!
        call sel_read_step_SPH_field_file(nprocs, my_rank, i_step,      &
     &      spec_fst_param, spec_time_IO, sph_spec_IO)
        call set_rj_phys_data_from_IO(sph_spec_IO, SPH_dat_ss%fld)
        call copy_time_step_data(spec_time_IO, t_SHR%time_d)
!
!  pickup components
!
        t_SHR%time_d%i_time_step = i_step
        call append_picked_spectrum_file(t_SHR%time_d,                  &
     &      SPH_dat_ss%sph%sph_rj, SPH_dat_ss%fld, pick_sph_u)
      end do
!
      end subroutine evolution
!
! ----------------------------------------------------------------------
!
      end module analyzer_pickup_mode_sph
