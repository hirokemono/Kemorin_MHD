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
      use t_structure_4_interolation
      use t_ucd_data
      use t_IO_step_parameter
!
      implicit none
!
      type(structure_4_interolation), save :: itp_udt
!
      type(ctl_data_gen_table), save :: gtbl_ctl
!
      type(time_data), save :: itp_time_IO
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
      use set_field_data_w_SGS
      use const_mesh_information
      use set_size_4_smp_types
      use nod_phys_send_recv
      use append_phys_data
!
      integer(kind = kint) :: ierr
!
!
      if (my_rank.eq.0)  write(*,*) 'Interpolate data to new mesh'
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_interpolate'
      call s_input_control_interpolate(itp_udt, ierr)
!
      call set_ctl_interpolate_udt(itp_udt%gtbl_ctl%fld_gt_ctl,         &
     &    itp_udt%org_fld)
      call dealloc_phys_control(itp_udt%gtbl_ctl%fld_gt_ctl)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'init_nod_send_recv'
      call init_nod_send_recv(itp_udt%org_fem%mesh)
!
!     --------------------- 
!
      if (my_rank .lt. itp_udt%gen_itp_p%ndomain_dest) then
        call count_size_4_smp_mesh                                      &
     &     (itp_udt%new_fem%mesh%node, itp_udt%new_fem%mesh%ele)
        if (i_debug.eq.iflag_full_msg) then
          call check_mesh_smp_size(my_rank, itp_udt%new_fem%mesh)
        end if
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'init_field_data_w_SGS'
      call init_field_data_w_SGS(itp_udt%org_fem%mesh%node%numnod,      &
     &                           itp_udt%org_fld, itp_udt%iphys,        &
     &                           itp_udt%iphys_LES)
!
      if (iflag_debug.eq.1) write(*,*) 'copy_field_name'
      call copy_field_name(itp_udt%org_fld, itp_udt%new_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_phys_data'
      call alloc_phys_data(itp_udt%new_fem%mesh%node%numnod,            &
     &                     itp_udt%new_fld)
!
      end subroutine initialize_itp_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_itp_udt
!
      use parallel_ucd_IO_select
      use output_parallel_ucd_file
      use nod_phys_send_recv
      use interpolate_nod_field_2_type
      use set_size_4_smp_types
!
      integer(kind = kint) :: i_step
!
!
      do i_step = itp_udt%t_ITP%init_d%i_time_step,                     &
     &            itp_udt%t_ITP%finish_d%i_end_step,                    &
     &            itp_udt%t_ITP%ucd_step%increment
        if (my_rank .lt. itp_udt%gen_itp_p%ndomain_org) then
          call set_data_by_read_ucd_once                                &
     &       (my_rank, i_step, itp_udt%gen_itp_p%org_ucd_IO,            &
     &        itp_udt%org_fld, itp_time_IO)
!
          call nod_fields_send_recv(itp_udt%org_fem%mesh,               &
     &                              itp_udt%org_fld, itp_udt%v_1st_sol)
        end if
!
!    interpolation
!
        if (iflag_debug.gt.0) write(*,*) 's_interpolate_nodal_data'
        call interpolate_nodal_data                                     &
     &     (itp_udt%org_fem%mesh%node, itp_udt%org_fld,                 &
     &      itp_udt%new_fem%mesh%nod_comm, itp_udt%itp_tbl,             &
     &      itp_udt%new_fem%mesh%node, itp_udt%new_fld,                 &
     &      itp_udt%v_1st_sol, itp_udt%v_2nd_sol)
!
!    output udt data
!
        if (my_rank .lt. itp_udt%gen_itp_p%ndomain_dest) then
          call link_field_data_type_2_IO(itp_udt%new_fem%mesh%node,     &
     &        itp_udt%new_fld, fem_ucd)
!
          call sel_write_parallel_ucd_file                              &
     &       (i_step, itp_udt%gen_itp_p%itp_ucd_IO,                     &
     &        itp_time_IO, fem_ucd)
          call disconnect_ucd_data(fem_ucd)
          call disconnect_ucd_node(fem_ucd)
        end if
      end do
!
      if (my_rank .lt. itp_udt%gen_itp_p%ndomain_dest) then
        call count_size_4_smp_mesh                                      &
     &     (itp_udt%new_fem%mesh%node, itp_udt%new_fem%mesh%ele)
        if (i_debug.eq.iflag_full_msg) then
          call check_mesh_smp_size(my_rank, itp_udt%new_fem%mesh)
        end if
      end if
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
