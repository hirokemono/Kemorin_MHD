!boundary_data_IO_b.f90
!      module boundary_data_IO_b
!
!     Written by H. Matsui on Sep., 2006
!
!       subroutine read_boundary_data_b(input_file_code)
!       subroutine write_boundary_data_b(input_file_code)
!
      module boundary_data_IO_b
!
      use m_precision
!
      use m_read_boundary_data
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_boundary_data_b(input_file_code)
!
      use group_data_IO_b
!
      integer (kind = kint), intent(in) :: input_file_code
!
!   read node group
!
      read(input_file_code) num_bc_dummy
!      write(*,*) 'num_bc_dummy', num_bc_dummy
!
      call allocate_bc_stack_dummy
      call read_group_stack_b(input_file_code, num_bc_dummy,            &
     &    num_nod_bc_dummy, bc_istack_dummy)
!
      call allocate_bc_item_dummy
!
      call read_group_item_b(input_file_code, num_bc_dummy,             &
     &     num_nod_bc_dummy, bc_istack_dummy, bc_name_dummy,            &
     &     bc_item_dummy)
!
!  read element group ( not in use)
!
      read(input_file_code) num_mat_dummy
!      write(*,*) 'num_mat_dummy', num_mat_dummy
!
      call allocate_bc_ele_stack_dummy
      call read_group_stack_b(input_file_code, num_mat_dummy,           &
     &    num_mat_bc_dummy, mat_istack_dummy)
!
      call allocate_bc_ele_item_dummy
!
      call read_group_item_b(input_file_code, num_mat_dummy,            &
     &     num_mat_bc_dummy, mat_istack_dummy, mat_name_dummy,          &
     &     mat_item_dummy)
!
!  read surface group
!
      read(input_file_code) num_surf_dummy
!      write(*,*) 'num_surf_dummy', num_surf_dummy
!
      call allocate_bc_sf_stack_dummy
!
      call read_group_stack_b(input_file_code, num_surf_dummy,          &
     &    num_surf_bc_dummy, surf_istack_dummy)
!
      call allocate_bc_sf_item_dummy
!
      call read_surface_group_item_b(input_file_code, num_surf_dummy,   &
     &    num_surf_bc_dummy, surf_istack_dummy, surf_name_dummy,        &
     &    surf_item_dummy)
!
      end subroutine read_boundary_data_b
!
! ----------------------------------------------------------------------
!
      subroutine write_boundary_data_b(input_file_code)
!
      use group_data_IO_b
!
      integer (kind = kint), intent(in) :: input_file_code
!
!
!   write node group
!
      call write_group_data_b(input_file_code, num_bc_dummy,            &
     &     num_nod_bc_dummy, bc_istack_dummy, bc_name_dummy,            &
     &     bc_item_dummy)
!
      call deallocate_bc_item_dummy
!
!  write element group ( not in use)
!
      call write_group_data_b(input_file_code, num_mat_dummy,           &
     &     num_mat_bc_dummy, mat_istack_dummy, mat_name_dummy,          &
     &     mat_item_dummy)
!
      call deallocate_bc_ele_item_dummy
!
!  write surface group
!
      call write_surface_group_data_b(input_file_code, num_surf_dummy,  &
     &     num_surf_bc_dummy, surf_istack_dummy, surf_name_dummy,       &
     &     surf_item_dummy)
!
      call deallocate_bc_sf_item_dummy
!
      end subroutine write_boundary_data_b
!
! ----------------------------------------------------------------------
!
      end module boundary_data_IO_b
