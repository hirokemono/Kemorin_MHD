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
      implicit none
!
      private :: link_2nd_field_data_2_output
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
      use m_geometry_parameter
      use m_node_phys_address
      use m_2nd_geometry_param
      use m_2nd_phys_data
!
      use input_control_interpolate
      use const_mesh_info
      use set_smp_size_4_2nd
      use nodal_vector_send_recv
!
      integer(kind = kint) :: ierr
!
!
      if (my_rank.eq.0)  write(*,*) 'Interpolate data to new mesh'
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_interpolate'
      call s_input_control_interpolate(ierr)
      call set_ctl_interpolate_udt
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_element_info'
      call set_local_element_info
!
!     --------------------- 
!
      if (my_rank .lt. ndomain_org) then
        if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
        call set_nod_and_ele_infos
      end if
!
!     --------------------- 
!
      if (my_rank .lt. ndomain_dest) then
        call s_count_smp_size_4_2nd
        if (i_debug.eq.iflag_full_msg) call check_smp_size_2nd(my_rank)
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_nod_field_data'
      call initialize_nod_field_data
!
      if (iflag_debug.eq.1) write(*,*) 'link_nodal_field_names'
      call link_nodal_field_names
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_phys_data_type'
      call alloc_phys_data_type(nnod_2nd, phys_2nd)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'init_send_recv'
      call init_send_recv
!
      end subroutine initialize_itp_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_itp_udt
!
      use m_t_step_parameter
      use m_ucd_data
      use m_ucd_input_data
      use m_ctl_params_4_gen_table
      use set_udt_to_2nd_data
      use ucd_IO_select
      use interpolate_nodal_field
      use nod_phys_send_recv
!
      integer(kind = kint) :: istep
!
!
      do istep = i_step_init, i_step_number, i_step_output_ucd
        if (my_rank .lt. ndomain_org) then
          call set_data_by_read_ucd_once(my_rank, istep,                &
   &          itype_org_udt_file, org_udt_file_head)
!
          call phys_send_recv_all
        end if
!
!    interpolation
!
        if (iflag_debug.gt.0) write(*,*) 's_interpolate_nodal_data'
        call interpolate_nodal_data
!
!    output udt data
!
        if (my_rank .lt. ndomain_dest) then
          call link_2nd_field_data_2_output(fem_ucd)
!
          call set_ucd_file_format(itype_itp_udt_file)
          call set_ucd_file_prefix(itp_udt_file_head)
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
      subroutine link_2nd_field_data_2_output(ucd)
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_phys_data
      use set_ucd_data_to_type
      use set_ucd_data
!
      use t_ucd_data
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_node_data_2_output(nnod_2nd, globalnodid_2nd, xx_2nd,   &
     &    ucd)
      call link_field_data_type_2_output(nnod_2nd, phys_2nd, ucd)
!
      end subroutine link_2nd_field_data_2_output
!
!-----------------------------------------------------------------------
!
      end module analyzer_interpolate_udt
