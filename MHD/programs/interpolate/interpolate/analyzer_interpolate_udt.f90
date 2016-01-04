!analyzer_interpolate_udt.f90
!      module analyzer_interpolate_udt
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initialize_itp_udt
!      subroutine analyze_itp_udt
!
      module analyzer_interpolate_udt
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
      use t_phys_data
      use t_ucd_data
!
      implicit none
!
      type(mesh_data), save :: new_femmesh
      type(surface_geometry), save :: new_surf_mesh
      type(edge_geometry), save ::  new_edge_mesh
!
      type(phys_data), save :: new_phys
!
      type(ucd_data), save :: fem_ucd
!
      private :: link_field_data_type_2_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_itp_udt
!
      use m_geometry_data
      use m_ctl_params_4_gen_table
      use m_t_step_parameter
      use m_nod_comm_table
      use m_node_phys_data
      use t_FEM_phys_data
!
      use input_control_interpolate
      use const_mesh_information
      use set_size_4_smp_types
      use nod_phys_send_recv
!
      integer(kind = kint) :: ierr
!
!
      if (my_rank.eq.0)  write(*,*) 'Interpolate data to new mesh'
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_interpolate'
      call s_input_control_interpolate(new_femmesh,                     &
     &    new_surf_mesh, new_edge_mesh, ierr)
      call set_ctl_interpolate_udt
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'init_send_recv'
      call init_send_recv(nod_comm)
!
!     --------------------- 
!
      if (my_rank .lt. ndomain_org) then
        if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
        call set_nod_and_ele_infos(node1, ele1)
      end if
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
      if (iflag_debug.eq.1) write(*,*) 'set_field_address_type'
      call set_field_address_type(node1%numnod, nod_fld1, iphys)
!
      if (iflag_debug.eq.1) write(*,*) 'link_field_name_type'
      call link_field_name_type(nod_fld1, new_phys)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_phys_data_type'
      call alloc_phys_data_type(new_femmesh%mesh%node%numnod, new_phys)
!
      end subroutine initialize_itp_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_itp_udt
!
      use m_t_step_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_data
      use m_ctl_params_4_gen_table
      use ucd_IO_select
      use set_ucd_data_to_type
      use nod_phys_send_recv
      use interpolate_nod_field_2_type
!
      integer(kind = kint) :: istep
!
!
      do istep = i_step_init, i_step_number, i_step_output_ucd
        if (my_rank .lt. ndomain_org) then
          call set_data_by_read_ucd_once(my_rank, istep,                &
   &          itype_org_udt_file, org_udt_file_head, nod_fld1)
!
          call nod_fields_send_recv(node1, nod_comm, nod_fld1)
        end if
!
!    interpolation
!
        if (iflag_debug.gt.0) write(*,*) 's_interpolate_nodal_data'
        call interpolate_nodal_data(node1, nod_fld1,                    &
     &      new_femmesh%mesh%nod_comm, new_femmesh%mesh%node, new_phys)
!
!    output udt data
!
        if (my_rank .lt. ndomain_dest) then
          call link_field_data_type_2_IO(new_femmesh%mesh%node,         &
     &        new_phys, fem_ucd)
!
          call set_ucd_file_format(itype_itp_udt_file, fem_ucd)
          call set_ucd_file_prefix(itp_udt_file_head, fem_ucd)
          call sel_write_udt_file(my_rank, istep, fem_ucd)
          call disconnect_ucd_data(fem_ucd)
          call disconnect_ucd_node(fem_ucd)
        end if
      end do
!
      end subroutine analyze_itp_udt
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine link_field_data_type_2_IO(node, nod_fld, ucd)
!
      use set_ucd_data_to_type
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_node_data_2_ucd(node, ucd)
      call link_field_data_to_ucd(nod_fld, ucd)
!
      end subroutine link_field_data_type_2_IO
!
!-----------------------------------------------------------------------
!
      end module analyzer_interpolate_udt
