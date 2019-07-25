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
      subroutine initialize_pick_gauss_coef
!
      use m_ctl_params_sph_utils
      use m_legendre_transform_list
      use parallel_load_data_4_sph
      use set_sph_phys_address
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
     &   (spu_ctl1, t_SHR, SPH_dat_ss%fld, pwr_spec)
!
!       set spectr grids
!
      if (iflag_debug.gt.0) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh(sph_file_param0,                          &
     &    SPH_dat_ss%sph, SPH_dat_ss%comms, SPH_dat_ss%groups)
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
      call alloc_phys_data_type                                         &
     &   (SPH_dat_ss%sph%sph_rj%nnod_rj, SPH_dat_ss%fld)
!
      call init_rms_4_sph_spectr                                        &
     &   (SPH_dat_ss%sph%sph_params, SPH_dat_ss%sph%sph_rj,             &
     &    SPH_dat_ss%fld, pwr_spec, WK_pwr_spec)
!
      call set_sph_sprctr_data_address(SPH_dat_ss%sph%sph_rj,           &
     &    SPH_dat_ss%ipol, SPH_dat_ss%idpdr, SPH_dat_ss%itor,           &
     &    SPH_dat_ss%fld)
!
      end subroutine initialize_pick_gauss_coef
!
! ----------------------------------------------------------------------
!
      subroutine analyze_pick_gauss_coef
!
      use m_ctl_params_sph_utils
!
      use copy_rj_phys_data_4_IO
      use cal_write_sph_monitor_data
      use pickup_gauss_coefficients
      use MPI_sph_gauss_coefs_IO
!
      integer(kind = kint) :: i_step
!
!
      call init_gauss_coefs_4_monitor                                   &
     &   (SPH_dat_ss%sph%sph_params, SPH_dat_ss%sph%sph_rj,             &
     &    SPH_dat_ss%ipol, gauss_list_u, gauss_u)
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
        t_SHR%time_d%i_time_step = i_step
        call append_sph_gauss_coefs_file(t_SHR%time_d,                  &
     &      SPH_dat_ss%sph%sph_params, SPH_dat_ss%sph%sph_rj,           &
     &      SPH_dat_ss%ipol, SPH_dat_ss%fld, gauss_u)
      end do
!
      end subroutine analyze_pick_gauss_coef
!
! ----------------------------------------------------------------------
!
      end module analyzer_pickup_gauss_coefs
