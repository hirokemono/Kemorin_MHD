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
      call load_para_SPH_rj_mesh(files_SHR%sph_file_param,              &
     &    SPH_dat_ss%sph, SPH_dat_ss%comms, SPH_dat_ss%groups)
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_SPH_file'
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, t_SHR%init_d%i_time_step,                    &
     &    spec_fst_param, spec_time_IO, sph_fld_IN)
!
!  -------------------------------
!
      call copy_time_data(spec_time_IO, t_SHR%init_d)
      call copy_rj_phys_name_from_IO(sph_fld_IN, SPH_dat_ss%fld)
!
      call set_sph_sprctr_data_address(SPH_dat_ss%sph%sph_rj,           &
     &    SPH_dat_ss%ipol, SPH_dat_ss%idpdr, SPH_dat_ss%itor,           &
     &    SPH_dat_ss%fld)
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
      use m_ctl_params_sph_utils
      use copy_rj_phys_data_4_IO
      use set_parallel_file_name
      use cal_t_ave_sph_spectr_data
      use const_global_element_ids
!
      integer(kind = kint) :: i_step
      type(field_IO_params) :: ave_fst_param
      type(sph_average_work) :: ave_WK_spec
!
!
      call allocate_d_rj_tmp(SPH_dat_ss%fld, ave_WK_spec)
!
!   Averaging
      do i_step = t_SHR%init_d%i_time_step, t_SHR%finish_d%i_end_step,  &
     &           t_SHR%ucd_step%increment
        if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
        call sel_read_step_SPH_field_file(nprocs, my_rank, i_step,      &
     &      spec_fst_param, spec_time_IO, sph_fld_IN)
!
        call copy_time_data(spec_time_IO, t_SHR%init_d)
!
        if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
        call set_rj_phys_data_from_IO(sph_fld_IN, SPH_dat_ss%fld)
!
        call sum_sph_spectr_data(SPH_dat_ss%fld, ave_WK_spec)
      end do

      call calypso_mpi_barrier
      call t_ave_sph_spectr_data                                        &
     &   (t_SHR%init_d%i_time_step, t_SHR%finish_d%i_end_step,          &
     &    ave_WK_spec, SPH_dat_ss%fld)
!
      call copy_rj_phys_name_to_IO                                      &
     &   (SPH_dat_ss%fld%num_phys, SPH_dat_ss%fld, sph_fld_OUT)
      call alloc_phys_data_IO(sph_fld_OUT)
      call copy_rj_phys_data_to_IO                                      &
     &   (SPH_dat_ss%fld%num_phys, SPH_dat_ss%fld, sph_fld_OUT)
!
      call alloc_merged_field_stack(nprocs, sph_fld_OUT)
      call count_number_of_node_stack                                   &
     &   (sph_fld_OUT%nnod_IO, sph_fld_OUT%istack_numnod_IO)
!
!
      call copy_file_params_type(spec_fst_param, ave_fst_param)
      ave_fst_param%file_prefix                                         &
     &   = add_int_suffix(t_SHR%init_d%i_time_step, tave_sph_file_head)
      call sel_write_step_SPH_field_file                                &
     &   (nprocs, my_rank, t_SHR%finish_d%i_end_step,                   &
     &    ave_fst_param, spec_time_IO, sph_fld_OUT)
!
      call dealloc_phys_data_IO(sph_fld_OUT)
      call dealloc_phys_name_IO(sph_fld_OUT)
      call calypso_mpi_barrier
!
!   Standard deviation
!
      do i_step = t_SHR%init_d%i_time_step, t_SHR%finish_d%i_end_step,  &
     &           t_SHR%ucd_step%increment
        if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
        call sel_read_step_SPH_field_file(nprocs, my_rank, i_step,      &
     &      spec_fst_param, spec_time_IO, sph_fld_IN)
!
        call copy_time_data(spec_time_IO, t_SHR%init_d)
!
        if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
        call set_rj_phys_data_from_IO(sph_fld_IN, SPH_dat_ss%fld)
!
        call sum_deviation_sph_spectr(SPH_dat_ss%fld, ave_WK_spec)
      end do
!
      call calypso_mpi_barrier
      call dealloc_phys_data_IO(sph_fld_IN)
      call dealloc_phys_name_IO(sph_fld_IN)
!
      call sdev_sph_spectr_data                                         &
     &   (t_SHR%init_d%i_time_step, t_SHR%finish_d%i_end_step,          &
     &    ave_WK_spec, SPH_dat_ss%fld)
!
      call copy_rj_phys_name_to_IO                                      &
     &   (SPH_dat_ss%fld%num_phys, SPH_dat_ss%fld, sph_fld_OUT)
      call alloc_phys_data_IO(sph_fld_OUT)
      call copy_rj_phys_data_to_IO                                      &
     &   (SPH_dat_ss%fld%num_phys, SPH_dat_ss%fld, sph_fld_OUT)
!
      call alloc_merged_field_stack(nprocs, sph_fld_OUT)
      call count_number_of_node_stack                                   &
     &   (sph_fld_OUT%nnod_IO, sph_fld_OUT%istack_numnod_IO)
!
!
      call copy_file_params_type(spec_fst_param, ave_fst_param)
      ave_fst_param%file_prefix                                         &
     &   = add_int_suffix(t_SHR%init_d%i_time_step, sdev_sph_file_head)
      call sel_write_step_SPH_field_file                                &
     &   (nprocs, my_rank, t_SHR%finish_d%i_end_step,                   &
     &    ave_fst_param, spec_time_IO, sph_fld_OUT)
!
      call dealloc_phys_data_IO(sph_fld_OUT)
      call dealloc_phys_name_IO(sph_fld_OUT)
      call calypso_mpi_barrier
!
      call deallocate_d_rj_tmp(ave_WK_spec)
!
      if (iflag_debug.eq.1) write(*,*) 'exit evolution_ave_sph'
!
      end subroutine evolution_ave_sph
!
! ----------------------------------------------------------------------
!
      end module analyzer_time_ave_sph
