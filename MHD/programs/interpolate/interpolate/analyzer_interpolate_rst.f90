!analyzer_interpolate_rst.f90
!      module analyzer_interpolate_rst
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initialize_itp_rst
!      subroutine analyze_itp_rst
!
      module analyzer_interpolate_rst
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_machine_parameter
!
      use t_structure_4_interolation
      use t_field_data_IO
      use t_IO_step_parameter
      use t_solver_SR
!
      implicit none
!
      type(structure_4_interolation), save :: itp_rst
!
      type(time_data), save :: itp_time_IO
      type(field_IO), save :: itp_fld_IO
!
      type(send_recv_status), save :: SR_sig7
      type(send_recv_real_buffer), save :: SR_r7
!
      private :: itp_fld_IO, itp_time_IO, itp_rst
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_itp_rst
!
      use set_field_data_w_SGS
      use const_mesh_information
      use set_size_4_smp_types
      use nod_phys_send_recv
      use set_field_to_restart
      use field_IO_select
      use append_phys_data
!
!
      integer(kind = kint) :: ierr, i_step
!
!
      if (my_rank.eq.0)  write(*,*) 'Interpolate data to new mesh'
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_interpolate'
      call s_input_control_interpolate(itp_rst, ierr)
!
!     --------------------- 
!
      call init_real_send_recv(itp_rst%org_fem%mesh%nod_comm,           &
     &                         SR_sig7, SR_r7)
!
!     ---------------------
!
      if (my_rank .lt. itp_rst%gen_itp_p%ndomain_dest) then
        call count_size_4_smp_mesh                                      &
     &     (itp_rst%new_fem%mesh%node, itp_rst%new_fem%mesh%ele)
        if (i_debug.eq.iflag_full_msg) then
          call check_mesh_smp_size(my_rank, itp_rst%new_fem%mesh)
        end if
      end if
!
!     --------------------- 
!
      i_step = int(itp_rst%t_ITP%init_d%i_time_step                     &
     &            / itp_rst%t_ITP%rst_step%increment, KIND(i_step))
      call sel_read_alloc_step_FEM_file                                 &
     &   (itp_rst%gen_itp_p%ndomain_org, 0, i_step,                     &
     &    itp_rst%gen_itp_p%org_fst_IO, itp_time_IO, itp_fld_IO)
      if (iflag_debug.eq.1) write(*,*) 'init_field_name_by_restart'
      call init_field_name_by_restart(itp_fld_IO, itp_rst%org_fld)
      call dealloc_phys_data_IO(itp_fld_IO)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'init_field_data_w_SGS'
      call init_field_data_w_SGS(itp_rst%org_fem%mesh%node%numnod,      &
     &                           itp_rst%org_fld, itp_rst%iphys,        &
     &                           itp_rst%iphys_LES)
!
      if (iflag_debug.eq.1) write(*,*) 'copy_field_name'
      call copy_field_name(itp_rst%org_fld, itp_rst%new_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_phys_data'
      call alloc_phys_data(itp_rst%new_fem%mesh%node%numnod,            &
     &                     itp_rst%new_fld)
!
      end subroutine initialize_itp_rst
!
! ----------------------------------------------------------------------
!
      subroutine analyze_itp_rst
!
      use calypso_mpi
      use calypso_mpi_real
      use calypso_mpi_int
!
      use field_IO_select
      use set_parallel_file_name
      use nod_phys_send_recv
      use set_field_to_restart
      use set_field_to_restart
      use interpolate_nod_field_2_type
      use const_global_element_ids
      use set_size_4_smp_types
!
      integer(kind = kint) :: i_step, i_start, i_end
!
!
      i_start = int(itp_rst%t_ITP%init_d%i_time_step                    &
     &            / itp_rst%t_ITP%rst_step%increment,KIND(i_start))
      i_end =   int(itp_rst%t_ITP%finish_d%i_end_step                   &
     &            / itp_rst%t_ITP%rst_step%increment, KIND(i_end))
      do i_step = i_start, i_end
!
        if (my_rank .lt. itp_rst%gen_itp_p%ndomain_org) then
          itp_fld_IO%nnod_IO = itp_rst%org_fem%mesh%node%numnod
          call alloc_phys_data_IO(itp_fld_IO)
!
          call sel_read_step_FEM_field_file(nprocs, my_rank, i_step,    &
     &        itp_rst%gen_itp_p%org_fst_IO, itp_time_IO, itp_fld_IO)
!
          call copy_field_data_from_restart                             &
     &       (itp_rst%org_fem%mesh%node, itp_fld_IO, itp_rst%org_fld)
          call dealloc_phys_data_IO(itp_fld_IO)
!
          call copy_time_step_data(itp_time_IO, itp_rst%t_ITP%init_d)
          call nod_fields_send_recv(itp_rst%org_fem%mesh,               &
     &        itp_rst%org_fld, itp_rst%v_1st_sol, SR_sig7, SR_r7)
        end if
!
        call calypso_mpi_bcast_one_real(itp_rst%t_ITP%init_d%time, 0)
        call calypso_mpi_bcast_one_int                                  &
     &     (itp_rst%t_ITP%init_d%i_time_step, 0)
!
        if (iflag_debug.gt.0)  write(*,*) 's_interpolate_nodal_data'
        call interpolate_nodal_data                                     &
     &     (itp_rst%org_fem%mesh%node, itp_rst%org_fld,                 &
     &      itp_rst%new_fem%mesh%nod_comm, itp_rst%itp_tbl,             &
     &      itp_rst%new_fem%mesh%node, itp_rst%new_fld,                 &
     &      itp_rst%v_1st_sol, itp_rst%v_2nd_sol, SR_sig7, SR_r7)
!
        if (my_rank .lt. itp_rst%gen_itp_p%ndomain_dest) then
          call copy_time_step_size_data(itp_rst%t_ITP%init_d,           &
     &                                  itp_time_IO)
!
          itp_fld_IO%nnod_IO = itp_rst%new_fem%mesh%node%numnod
          call alloc_phys_data_IO(itp_fld_IO)
          call copy_field_data_to_restart                               &
     &       (itp_rst%new_fem%mesh%node, itp_rst%new_fld, itp_fld_IO)
!
          call alloc_merged_field_stack(nprocs, itp_fld_IO)
          call count_number_of_node_stack                               &
     &       (itp_fld_IO%nnod_IO, itp_fld_IO%istack_numnod_IO)
!
          call sel_write_step_FEM_field_file                            &
     &       (i_step, itp_rst%gen_itp_p%itp_fst_IO,                     &
     &        itp_time_IO, itp_fld_IO)
!
          call dealloc_phys_data_IO(itp_fld_IO)
          call dealloc_merged_field_stack(itp_fld_IO)
        end if
      end do
!
      call dealloc_phys_name_IO(itp_fld_IO)
!
!
      if (my_rank .lt. itp_rst%gen_itp_p%ndomain_dest) then
        call count_size_4_smp_mesh                                      &
     &     (itp_rst%new_fem%mesh%node, itp_rst%new_fem%mesh%ele)
        if (i_debug.eq.iflag_full_msg) then
          call check_mesh_smp_size(my_rank, itp_rst%new_fem%mesh)
        end if
      end if
!
      end subroutine analyze_itp_rst
!
! ----------------------------------------------------------------------
!
      end module analyzer_interpolate_rst
