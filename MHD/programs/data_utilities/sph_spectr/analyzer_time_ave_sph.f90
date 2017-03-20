!analyzer_time_ave_sph.f90
!      module analyzer_time_ave_sph
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine initialize_ave_sph
!      subroutine evolution_ave_sph
!
!
      module analyzer_time_ave_sph
!
      use m_precision
      use m_machine_parameter
      use m_spheric_data_sph_spetr
      use m_schmidt_poly_on_rtm
      use calypso_mpi
!
      use t_time_data
      use field_IO_select
!
      implicit none
!
      type(field_IO), save, private :: sph_fld_IN, sph_fld_OUT
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_ave_sph
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
      call set_ctl_data_4_sph_utils                                     &
     &   (rst_step_SHR, ucd_step_SHR, viz_step_SHR,                     &
     &    rj_fld_spec, pwr_spec)
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
     &    (iflag_org_sph_file_fmt, org_sph_file_head, sph_fld_IN)
      call sel_read_alloc_step_SPH_file(nprocs, my_rank,                &
     &    init_d1%i_time_step, spec_time_IO, sph_fld_IN)
!
!  -------------------------------
!
      call copy_time_data(spec_time_IO, init_d1)
      call copy_rj_phys_name_from_IO(sph_fld_IN, rj_fld_spec)
!
      call set_sph_sprctr_data_address(sph_mesh_spec%sph%sph_rj,        &
     &    ipol_spec, idpdr_spec, itor_spec, rj_fld_spec)
!
!  -------------------------------
!
      call calypso_MPI_barrier
!
      end subroutine initialize_ave_sph
!
! ----------------------------------------------------------------------
!
      subroutine evolution_ave_sph
!
      use m_t_step_parameter
      use m_ctl_params_sph_utils
      use copy_rj_phys_data_4_IO
      use set_parallel_file_name
      use cal_t_ave_sph_spectr_data
      use const_global_element_ids
!
      integer(kind = kint) :: i_step
!
!
      call allocate_d_rj_tmp                                            &
     &   (rj_fld_spec%n_point, rj_fld_spec%ntot_phys)
!
!   Averaging
      do i_step = init_d1%i_time_step, finish_d1%i_end_step,            &
     &           ucd_step_SHR%increment
        call set_field_file_fmt_prefix                                  &
     &   (iflag_org_sph_file_fmt, org_sph_file_head, sph_fld_IN)
        if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
        call sel_read_step_SPH_field_file                               &
     &     (nprocs, my_rank, i_step, spec_time_IO, sph_fld_IN)
!
        call copy_time_data(spec_time_IO, init_d1)
!
        if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
        call set_rj_phys_data_from_IO(sph_fld_IN, rj_fld_spec)
!
        call sum_sph_spectr_data                                        &
     &     (rj_fld_spec%n_point, rj_fld_spec%ntot_phys,                 &
     &      rj_fld_spec%d_fld)
      end do

      call calypso_mpi_barrier
      call t_ave_sph_spectr_data                                        &
     &   (init_d1%i_time_step, finish_d1%i_end_step,                    &
     &    rj_fld_spec%n_point, rj_fld_spec%ntot_phys,                   &
     &    rj_fld_spec%d_fld)
!
      call copy_rj_phys_name_to_IO                                      &
     &   (rj_fld_spec%num_phys, rj_fld_spec, sph_fld_OUT)
      call alloc_phys_data_IO(sph_fld_OUT)
      call copy_rj_phys_data_to_IO                                      &
     &   (rj_fld_spec%num_phys, rj_fld_spec, sph_fld_OUT)
!
      call alloc_merged_field_stack(nprocs, sph_fld_OUT)
      call count_number_of_node_stack                                   &
     &   (sph_fld_OUT%nnod_IO, sph_fld_OUT%istack_numnod_IO)
!
!
      call set_field_file_fmt_prefix                                    &
     &    (iflag_org_sph_file_fmt, org_sph_file_head, sph_fld_OUT)
      call add_int_suffix(init_d1%i_time_step,                          &
     &    tave_sph_file_head, sph_fld_OUT%file_prefix)
      call sel_write_step_SPH_field_file(nprocs, my_rank,               &
     &    finish_d1%i_end_step, spec_time_IO, sph_fld_OUT)
!
      call dealloc_phys_data_IO(sph_fld_OUT)
      call dealloc_phys_name_IO(sph_fld_OUT)
      call calypso_mpi_barrier
!
!   Standard deviation
!
      do i_step = init_d1%i_time_step, finish_d1%i_end_step,            &
     &           ucd_step_SHR%increment
        call set_field_file_fmt_prefix                                  &
     &   (iflag_org_sph_file_fmt, org_sph_file_head, sph_fld_IN)
        if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
        call sel_read_step_SPH_field_file                               &
     &     (nprocs, my_rank, i_step, spec_time_IO, sph_fld_IN)
!
        call copy_time_data(spec_time_IO, init_d1)
!
        if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
        call set_rj_phys_data_from_IO(sph_fld_IN, rj_fld_spec)
!
        call sum_deviation_sph_spectr                                   &
     &     (rj_fld_spec%n_point, rj_fld_spec%ntot_phys,                 &
     &      rj_fld_spec%d_fld)
      end do
!
      call calypso_mpi_barrier
      call dealloc_phys_data_IO(sph_fld_IN)
      call dealloc_phys_name_IO(sph_fld_IN)
!
      call sdev_sph_spectr_data                                         &
     &   (init_d1%i_time_step, finish_d1%i_end_step,                    &
     &    rj_fld_spec%n_point, rj_fld_spec%ntot_phys,                   &
     &    rj_fld_spec%d_fld)
!
      call copy_rj_phys_name_to_IO                                      &
     &   (rj_fld_spec%num_phys, rj_fld_spec, sph_fld_OUT)
      call alloc_phys_data_IO(sph_fld_OUT)
      call copy_rj_phys_data_to_IO                                      &
     &   (rj_fld_spec%num_phys, rj_fld_spec, sph_fld_OUT)
!
      call alloc_merged_field_stack(nprocs, sph_fld_OUT)
      call count_number_of_node_stack                                   &
     &   (sph_fld_OUT%nnod_IO, sph_fld_OUT%istack_numnod_IO)
!
!
      call set_field_file_fmt_prefix                                    &
     &    (iflag_org_sph_file_fmt, org_sph_file_head, sph_fld_OUT)
      call add_int_suffix(init_d1%i_time_step,                          &
     &    sdev_sph_file_head, sph_fld_OUT%file_prefix)
      call sel_write_step_SPH_field_file(nprocs, my_rank,               &
     &    finish_d1%i_end_step, spec_time_IO, sph_fld_OUT)
!
      call dealloc_phys_data_IO(sph_fld_OUT)
      call dealloc_phys_name_IO(sph_fld_OUT)
      call calypso_mpi_barrier
!
      call deallocate_d_rj_tmp
!
      if (iflag_debug.eq.1) write(*,*) 'exit evolution_ave_sph'
!
      end subroutine evolution_ave_sph
!
! ----------------------------------------------------------------------
!
      end module analyzer_time_ave_sph
