!
!      module input_control_interpolate
!
!     Written by H. Matsui on July, 2006
!
!      subroutine s_input_control_interpolate                           &
!     &         (org_femmesh, org_ele_mesh, new_femmesh, new_ele_mesh,  &
!     &          ierr)
!      subroutine set_ctl_interpolate_udt(nod_fld)
!
      module input_control_interpolate
!
      use m_precision
!
      use calypso_mpi
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
      subroutine s_input_control_interpolate                            &
     &         (org_femmesh, org_ele_mesh, new_femmesh, new_ele_mesh,   &
     &          ierr)
!
      use t_mesh_data
!
      use m_2nd_pallalel_vector
      use m_ctl_params_4_gen_table
      use m_ctl_data_gen_table
      use m_read_mesh_data
!
      use set_ctl_interpolation
!
      use load_mesh_data
      use const_mesh_information
!
      use itp_table_IO_select_4_zlib
      use copy_interpolate_dest_IO
      use copy_interpolate_org_IO
      use interpolate_nod_field_2_type
!
      type(mesh_data), intent(inout) :: org_femmesh
      type(element_geometry), intent(inout) :: org_ele_mesh
!
      type(mesh_data), intent(inout) :: new_femmesh
      type(element_geometry), intent(inout) :: new_ele_mesh
      integer(kind = kint), intent(inout) :: ierr
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
        mesh_file_head = org_mesh_head
        iflag_mesh_file_fmt = ifmt_org_mesh_file
        call input_mesh(my_rank, org_femmesh%mesh, org_femmesh%group,   &
     &     org_ele_mesh%surf%nnod_4_surf,                               &
     &     org_ele_mesh%edge%nnod_4_edge)
!
        if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
        call set_nod_and_ele_infos                                      &
     &     (org_femmesh%mesh%node, org_femmesh%mesh%ele)
      end if
!
!  --  read 2nd mesh for target (if exist)
!
!
      if (my_rank .lt. ndomain_dest) then
        mesh_file_head = dest_mesh_head
        iflag_mesh_file_fmt = ifmt_itp_mesh_file
        call input_mesh(my_rank, new_femmesh%mesh, new_femmesh%group,   &
     &      new_ele_mesh%surf%nnod_4_surf,                              &
     &      new_ele_mesh%edge%nnod_4_edge)
        call allocate_overlaped_ele_type(new_femmesh%mesh%ele)
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
      if (iflag_debug.eq.1) write(*,*) 'init_interpolate_nodal_data'
      call init_interpolate_nodal_data                                  &
     &   (org_femmesh%mesh%node, org_femmesh%mesh%ele,                  &
     &    new_femmesh%mesh%node)
!
      end subroutine s_input_control_interpolate
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_interpolate_udt(nod_fld)
!
      use calypso_mpi
      use t_phys_data
      use set_control_nodal_data
!
      type(phys_data), intent(inout) :: nod_fld
      integer(kind = kint) :: ierr
!
!
      call s_set_control_nodal_data(nod_fld, ierr)
      if (ierr .ne. 0) call calypso_MPI_abort(ierr, e_message)
!
      end subroutine set_ctl_interpolate_udt
!
! ----------------------------------------------------------------------
!
      end module input_control_interpolate
