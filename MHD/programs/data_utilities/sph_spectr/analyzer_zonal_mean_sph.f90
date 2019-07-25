!analyzer_zonal_mean_sph.f90
!      module analyzer_zonal_mean_sph
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine init_zonal_mean_sph
!      subroutine analyze_zonal_mean_sph
!
      module analyzer_zonal_mean_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_spheric_data_sph_spetr
      use calypso_mpi
!
      use t_time_data
      use t_field_data_IO
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
      subroutine init_zonal_mean_sph
!
      use m_ctl_params_sph_utils
      use parallel_load_data_4_sph
      use set_sph_phys_address
      use copy_rj_phys_data_4_IO
      use count_num_sph_smp
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
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_rj_mesh'
      call load_para_SPH_rj_mesh(sph_file_param0,                       &
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
      call set_sph_sprctr_data_address(SPH_dat_ss%sph%sph_rj,           &
     &    SPH_dat_ss%ipol, SPH_dat_ss%idpdr, SPH_dat_ss%itor,           &
     &    SPH_dat_ss%fld)
!
      call calypso_MPI_barrier
!
      end subroutine init_zonal_mean_sph
!
! ----------------------------------------------------------------------
!
      subroutine analyze_zonal_mean_sph
!
      use m_ctl_params_sph_utils
      use copy_rj_phys_data_4_IO
      use cal_zonal_mean_sph_spectr
      use const_global_element_ids
!
      integer(kind = kint) :: i_step
!
!
      do i_step = t_SHR%init_d%i_time_step, t_SHR%finish_d%i_end_step,  &
     &           t_SHR%ucd_step%increment
!
!   Input spectr data
!
        if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
        call sel_read_step_SPH_field_file(nprocs, my_rank, i_step,      &
     &      spec_fst_param, spec_time_IO, sph_spec_IO)
!
        if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
        call set_rj_phys_data_from_IO(sph_spec_IO, SPH_dat_ss%fld)
!
!  evaluate energies
!
        call zonal_mean_all_sph_spectr                                  &
     &     (SPH_dat_ss%sph%sph_rj, SPH_dat_ss%fld)
        call copy_rj_phys_data_to_IO                                    &
     &     (SPH_dat_ss%fld%num_phys, SPH_dat_ss%fld, sph_spec_IO)
!
        call alloc_merged_field_stack(nprocs, sph_spec_IO)
        call count_number_of_node_stack                                 &
     &     (sph_spec_IO%nnod_IO, sph_spec_IO%istack_numnod_IO)
!
        if (iflag_debug.gt.0)                                           &
     &    write(*,*) 'sel_write_step_SPH_field_file'
        call sel_write_step_SPH_field_file(nprocs, my_rank, i_step,     &
     &      zm_sph_fst_param, spec_time_IO, sph_spec_IO)
        call dealloc_merged_field_stack(sph_spec_IO)
      end do
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze_zonal_mean_sph'
!
        end subroutine analyze_zonal_mean_sph
!
! ----------------------------------------------------------------------
!
      end module analyzer_zonal_mean_sph
