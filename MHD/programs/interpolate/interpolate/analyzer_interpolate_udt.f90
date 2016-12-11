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
      use t_FEM_phys_data
      use t_phys_data
      use t_phys_address
      use m_time_data_IO
      use t_ucd_data
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
      type(interpolate_table), save :: itp_udt
!
      type(phys_address), save :: iphys_ITP
      type(phys_data), save :: nod_fld_ITP
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
      use m_ctl_params_4_gen_table
      use m_t_step_parameter
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
      call s_input_control_interpolate(org_femmesh, org_ele_mesh,       &
     &   new_femmesh, new_ele_mesh, itp_udt, ierr)
!
      call set_ctl_interpolate_udt(nod_fld_ITP)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'init_send_recv'
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
      if (iflag_debug.eq.1) write(*,*) 'init_field_address'
      call init_field_address(org_femmesh%mesh%node%numnod,             &
     &                        nod_fld_ITP, iphys_ITP)
!
      if (iflag_debug.eq.1) write(*,*) 'copy_field_name_type'
      call copy_field_name_type(nod_fld_ITP, new_phys)
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
          call set_data_by_read_ucd_once                                &
     &       (my_rank, istep, itype_org_udt_file, org_udt_file_head,    &
     &        nod_fld_ITP, t1_IO)
!
          call nod_fields_send_recv                                     &
     &       (org_femmesh%mesh%nod_comm, nod_fld_ITP)
        end if
!
!    interpolation
!
        if (iflag_debug.gt.0) write(*,*) 's_interpolate_nodal_data'
        call interpolate_nodal_data(org_femmesh%mesh%node, nod_fld_ITP, &
     &      new_femmesh%mesh%nod_comm, itp_udt,                         &
     &      new_femmesh%mesh%node, new_phys)
!
!    output udt data
!
        if (my_rank .lt. ndomain_dest) then
          call link_field_data_type_2_IO(new_femmesh%mesh%node,         &
     &        new_phys, fem_ucd)
!
          call set_ucd_file_format(itype_itp_udt_file, fem_ucd)
          call set_ucd_file_prefix(itp_udt_file_head, fem_ucd)
          call sel_write_udt_file(my_rank, istep, t1_IO, fem_ucd)
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
