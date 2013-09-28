!
!      module input_control_interpolate
!
!     Written by H. Matsui on July, 2006
!
!      subroutine s_input_control_interpolate
!      subroutine set_ctl_interpolate_udt
!
      module input_control_interpolate
!
      use m_precision
!
      use m_parallel_var_dof
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_interpolate
!
      use m_2nd_pallalel_vector
      use m_ctl_params_4_gen_table
      use m_ctl_data_gen_table
      use m_read_mesh_data
!
      use set_ctl_interpolation
!
      use load_mesh_data
      use load_2nd_mesh_data
!
      use itp_table_IO_select_4_zlib
      use copy_interpolate_dest_IO
      use copy_interpolate_org_IO
      use set_stack_tbl_wtype_org_smp
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_interpolate'
      call read_control_4_interpolate
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_interpolation'
      call set_ctl_params_interpolation
!
      call set_ctl_4_itp_steps
!
!  --  read geometry for origin (if exist)
!
      if (my_rank .lt. ndomain_org) then
!
        mesh_file_head = org_mesh_head
        iflag_mesh_file_fmt = ifmt_org_mesh_file
        call input_mesh(my_rank)
!
      end if
!
!  --  read 2nd mesh for target (if exist)
!
!
      if (my_rank .lt. ndomain_dest) then
        mesh_file_head = dest_mesh_head
        iflag_mesh_file_fmt = ifmt_itp_mesh_file
        call input_2nd_mesh(my_rank)
      end if
!
!  --  read interpolate table
!
      table_file_header = table_file_head
      if (iflag_debug.gt.0) write(*,*) 'sel_read_interpolate_table: ',  &
     &                                trim(table_file_header)
      call sel_read_interpolate_table(my_rank, ierr)
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_table_dest_from_IO'
      call copy_itp_table_dest_from_IO(my_rank)
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_table_org_from_IO'
      call copy_itp_table_org_from_IO(my_rank)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_stack_tbl_wtype_org_smp'
      call s_set_stack_tbl_wtype_org_smp
!
      end subroutine s_input_control_interpolate
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_interpolate_udt
!
      use calypso_mpi
      use set_control_nodal_data
!
!
      call s_set_control_nodal_data(ierr)
      if (ierr .ne. 0) call calypso_MPI_abort(ierr, e_message)
!
      end subroutine set_ctl_interpolate_udt
!
! ----------------------------------------------------------------------
!
      end module input_control_interpolate
