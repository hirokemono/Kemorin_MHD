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
      use m_t_step_parameter
      use t_field_data_IO
!
      use t_mesh_data
      use t_FEM_phys_data
      use t_phys_data
      use t_phys_address
      use t_interpolate_table
!
      implicit none
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
      use input_control_interpolate
      use const_mesh_information
      use set_size_4_smp_types
      use nod_phys_send_recv
      use set_field_to_restart
      use field_IO_select
!
!
      integer(kind = kint) :: ierr
!
!
      if (my_rank.eq.0)  write(*,*) 'Interpolate data to new mesh'
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_interpolate'
      call s_input_control_interpolate(org_femmesh, org_ele_mesh,       &
     &    new_femmesh, new_ele_mesh, itp_rst, ierr)
!
!     --------------------- 
!
      call init_send_recv(org_femmesh%mesh%nod_comm)
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
      istep_rst_start = int(i_step_init /   i_step_output_rst)
      call set_field_file_fmt_prefix                                    &
     &   (ifmt_org_rst_file, org_rst_file_head, itp_fld_IO)
      call sel_read_alloc_step_FEM_file                                 &
     &   (ndomain_org, izero, istep_rst_start, itp_fld_IO)
      if (iflag_debug.eq.1) write(*,*) 'init_field_name_by_restart'
      call init_field_name_by_restart(itp_fld_IO, nod_fld_ITP)
      call dealloc_phys_data_IO(itp_fld_IO)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_address_type'
      call set_field_address_type(org_femmesh%mesh%node%numnod,         &
     &                            nod_fld_ITP, iphys_ITP)
!
      if (iflag_debug.eq.1) write(*,*) 'link_field_name_type'
      call link_field_name_type(nod_fld_ITP, new_phys)
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
      use m_time_data_IO
!
      use field_IO_select
      use set_parallel_file_name
      use copy_time_steps_4_restart
      use nod_phys_send_recv
      use set_field_to_restart
      use set_field_to_restart
      use interpolate_nod_field_2_type
      use const_global_element_ids
!
      integer(kind = kint) :: i_step
!
!
      istep_rst_start = int(i_step_init /   i_step_output_rst)
      istep_rst_end =   int(i_step_number / i_step_output_rst)
      do i_step = istep_rst_start, istep_rst_end
!
        if (my_rank .lt. ndomain_org) then
          itp_fld_IO%nnod_IO = org_femmesh%mesh%node%numnod
          call alloc_phys_data_IO(itp_fld_IO)
!
          call set_field_file_fmt_prefix                                &
     &       (ifmt_org_rst_file, org_rst_file_head, itp_fld_IO)
          call sel_read_step_FEM_field_file                             &
     &       (nprocs, my_rank, i_step, itp_fld_IO)
!
          call copy_field_data_from_restart                             &
     &       (org_femmesh%mesh%node, itp_fld_IO, nod_fld_ITP)
          call dealloc_phys_data_IO(itp_fld_IO)
          time =       time_IO
          i_step_MHD = i_time_step_IO
!
          call nod_fields_send_recv(org_femmesh%mesh%node,              &
     &        org_femmesh%mesh%nod_comm, nod_fld_ITP)
        end if
!
        call MPI_Bcast(time, ione, CALYPSO_REAL, izero,                 &
     &      CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(i_step_MHD, ione, CALYPSO_INTEGER, izero,        &
     &      CALYPSO_COMM, ierr_MPI)
!
        if (iflag_debug.gt.0)  write(*,*) 's_interpolate_nodal_data'
        call interpolate_nodal_data(org_femmesh%mesh%node, nod_fld_ITP, &
     &      new_femmesh%mesh%nod_comm, itp_rst,                         &
     &      new_femmesh%mesh%node, new_phys)
!
        if (my_rank .lt. ndomain_dest) then
          call copy_time_steps_to_restart
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
          call set_field_file_fmt_prefix                                &
     &       (ifmt_itp_rst_file, itp_rst_file_head, itp_fld_IO)
          call sel_write_step_FEM_field_file                            &
     &       (nprocs, my_rank, i_step, itp_fld_IO)
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
