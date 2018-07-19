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
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_step_parameter
      use t_field_data_IO
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_interpolate_table
      use t_IO_step_parameter
      use t_ctl_data_gen_table
!
      implicit none
!
      type(ctl_data_gen_table), save :: gtbl_ctl1
!
      type(time_step_param), save :: t_ITP
!
      type(mesh_data), save :: org_femmesh
      type(element_geometry), save :: org_ele_mesh
!
      type(mesh_data), save :: new_femmesh
      type(element_geometry), save :: new_ele_mesh
!
      type(interpolate_table), save :: itp_rst
!
      type(phys_address), save :: iphys_ITP
      type(phys_data), save :: nod_fld_ITP
!
      type(phys_data), save :: new_phys
!
      type(time_data), save :: itp_time_IO
      type(field_IO), save :: itp_fld_IO
!
      private :: new_femmesh, new_ele_mesh
      private :: new_phys, itp_fld_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_itp_rst
!
      use m_ctl_params_4_gen_table
!
      use init_nodal_field_address
      use input_control_interpolate
      use const_mesh_information
      use set_size_4_smp_types
      use nod_phys_send_recv
      use set_field_to_restart
      use field_IO_select
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
      call s_input_control_interpolate(gtbl_ctl1,                       &
     &    org_femmesh, org_ele_mesh, new_femmesh, new_ele_mesh,         &
     &    itp_rst, t_ITP, ierr)
!
!     --------------------- 
!
      call init_nod_send_recv(org_femmesh%mesh)
!
!     ---------------------
!
      if (my_rank .lt. ndomain_dest) then
        call count_size_4_smp_mesh_type                                 &
     &     (new_femmesh%mesh%node, new_femmesh%mesh%ele)
        if (i_debug.eq.iflag_full_msg) then
          call check_smp_size_type(my_rank, new_femmesh%mesh)
        end if
      end if
!
!     --------------------- 
!
      i_step = int(t_ITP%init_d%i_time_step / t_ITP%rst_step%increment)
      call sel_read_alloc_step_FEM_file(ndomain_org, izero, i_step,     &
     &    org_fst_IO, itp_time_IO, itp_fld_IO)
      if (iflag_debug.eq.1) write(*,*) 'init_field_name_by_restart'
      call init_field_name_by_restart(itp_fld_IO, nod_fld_ITP)
      call dealloc_phys_data_IO(itp_fld_IO)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'init_nod_fld_address'
      call init_nod_fld_address(org_femmesh%mesh%node,                  &
     &                        nod_fld_ITP, iphys_ITP)
!
      if (iflag_debug.eq.1) write(*,*) 'copy_field_name_type'
      call copy_field_name_type(nod_fld_ITP, new_phys)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_phys_data_type'
      call alloc_phys_data_type(new_femmesh%mesh%node%numnod, new_phys)
!
      end subroutine initialize_itp_rst
!
! ----------------------------------------------------------------------
!
      subroutine analyze_itp_rst
!
      use calypso_mpi
      use m_ctl_params_4_gen_table
!
      use field_IO_select
      use set_parallel_file_name
      use nod_phys_send_recv
      use set_field_to_restart
      use set_field_to_restart
      use interpolate_nod_field_2_type
      use const_global_element_ids
!
      integer(kind = kint) :: i_step, i_rst_start, i_rst_end
!
!
      i_rst_start                                                       &
     &      = int(t_ITP%init_d%i_time_step / t_ITP%rst_step%increment)
      i_rst_end                                                         &
     &      = int(t_ITP%finish_d%i_end_step / t_ITP%rst_step%increment)
      do i_step = i_rst_start, i_rst_end
!
        if (my_rank .lt. ndomain_org) then
          itp_fld_IO%nnod_IO = org_femmesh%mesh%node%numnod
          call alloc_phys_data_IO(itp_fld_IO)
!
          call sel_read_step_FEM_field_file(nprocs, my_rank, i_step,    &
     &        org_fst_IO, itp_time_IO, itp_fld_IO)
!
          call copy_field_data_from_restart                             &
     &       (org_femmesh%mesh%node, itp_fld_IO, nod_fld_ITP)
          call dealloc_phys_data_IO(itp_fld_IO)
!
          call copy_time_step_data(itp_time_IO, t_ITP%init_d)
          call nod_fields_send_recv(org_femmesh%mesh, nod_fld_ITP)
        end if
!
        call MPI_Bcast(t_ITP%init_d%time, ione, CALYPSO_REAL,           &
     &      izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(t_ITP%init_d%i_time_step, ione, CALYPSO_INTEGER, &
     &      izero, CALYPSO_COMM, ierr_MPI)
!
        if (iflag_debug.gt.0)  write(*,*) 's_interpolate_nodal_data'
        call interpolate_nodal_data(org_femmesh%mesh%node, nod_fld_ITP, &
     &      new_femmesh%mesh%nod_comm, itp_rst,                         &
     &      new_femmesh%mesh%node, new_phys)
!
        if (my_rank .lt. ndomain_dest) then
          call copy_time_step_size_data(t_ITP%init_d, itp_time_IO)
!
          itp_fld_IO%nnod_IO = new_femmesh%mesh%node%numnod
          call alloc_phys_data_IO(itp_fld_IO)
          call copy_field_data_to_restart                               &
     &       (new_femmesh%mesh%node, new_phys, itp_fld_IO)
!
          call alloc_merged_field_stack(nprocs, itp_fld_IO)
          call count_number_of_node_stack                               &
     &       (itp_fld_IO%nnod_IO, itp_fld_IO%istack_numnod_IO)
!
          call sel_write_step_FEM_field_file(nprocs, my_rank, i_step,   &
     &        itp_fst_IO, itp_time_IO, itp_fld_IO)
!
          call dealloc_phys_data_IO(itp_fld_IO)
          call dealloc_merged_field_stack(itp_fld_IO)
        end if
      end do
!
      call dealloc_phys_name_IO(itp_fld_IO)
!
      end subroutine analyze_itp_rst
!
! ----------------------------------------------------------------------
!
      end module analyzer_interpolate_rst
