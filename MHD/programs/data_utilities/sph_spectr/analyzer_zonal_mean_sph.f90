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
      use m_schmidt_poly_on_rtm
      use calypso_mpi
!
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
      use m_t_step_parameter
      use m_ctl_data_4_sph_utils
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
      call read_control_data_sph_utils
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_data_4_sph_utils'
      call set_ctl_data_4_sph_utils(rj_fld_spec)
!
!       set spectr grids
!
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_rj_mesh'
      call load_para_SPH_rj_mesh(sph_mesh_spec%sph,                     &
     &    sph_mesh_spec%sph_comms, sph_mesh_spec%sph_grps)
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_SPH_file'
      call set_field_file_fmt_prefix                                    &
     &    (iflag_org_sph_file_fmt, org_sph_file_head, sph_spec_IO)
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, i_step_init, sph_spec_IO)
!
!  -------------------------------
!
      call set_sph_sprctr_data_address(sph_mesh_spec%sph%sph_rj,        &
     &    ipol_spec, idpdr_spec, itor_spec, rj_fld_spec)
!
      call calypso_MPI_barrier
!
      end subroutine init_zonal_mean_sph
!
! ----------------------------------------------------------------------
!
      subroutine analyze_zonal_mean_sph
!
      use m_t_step_parameter
      use m_ctl_params_sph_utils
      use copy_rj_phys_data_4_IO
      use cal_zonal_mean_sph_spectr
      use const_global_element_ids
!
      integer(kind = kint) :: i_step
!
!
      do i_step = i_step_init, i_step_number, i_step_output_ucd
!
!   Input spectr data
!
        if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
        call set_field_file_fmt_prefix                                  &
     &     (iflag_org_sph_file_fmt, org_sph_file_head, sph_spec_IO)
      call sel_read_step_SPH_field_file                                 &
     &     (nprocs, my_rank, i_step, sph_spec_IO)
!
        if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
        call set_rj_phys_data_from_IO                                   &
     &     (sph_mesh_spec%sph%sph_rj%nnod_rj, sph_spec_IO, rj_fld_spec)
!
!  evaluate energies
!
        call zonal_mean_all_sph_spectr                                  &
     &     (sph_mesh_spec%sph%sph_rj, rj_fld_spec)
        call copy_rj_all_phys_data_to_IO                                &
     &     (sph_mesh_spec%sph%sph_rj%nnod_rj, rj_fld_spec, sph_spec_IO)
!
        call alloc_merged_field_stack(nprocs, sph_spec_IO)
        call count_number_of_node_stack                                 &
     &     (sph_spec_IO%nnod_IO, sph_spec_IO%istack_numnod_IO)
!
!
        sph_spec_IO%file_prefix = zm_sph_file_head
        if (iflag_debug.gt.0)                                           &
     &    write(*,*) 'sel_write_step_SPH_field_file'
        call sel_write_step_SPH_field_file                              &
     &     (nprocs, my_rank, i_step, sph_spec_IO)
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
