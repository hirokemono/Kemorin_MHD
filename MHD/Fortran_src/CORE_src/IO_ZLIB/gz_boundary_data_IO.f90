!
!      module gz_boundary_data_IO
!
!     Written by H. Matsui on Sep., 2006
!
!       subroutine read_boundary_data_gz
!       subroutine write_boundary_data_gz
!
      module gz_boundary_data_IO
!
      use m_precision
!
      use m_read_boundary_data
      use skip_gz_comment
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
       subroutine read_boundary_data_gz
!
      use gz_group_data_IO
!
!   read node group
!
      call skip_gz_comment_int(num_bc_dummy)
!      write(*,*) 'num_bc_dummy', num_bc_dummy
!
      call allocate_bc_stack_dummy
!
!      write(*,*) 'read_group_stack_gz', num_bc_dummy
      call read_group_stack_gz(num_bc_dummy, num_nod_bc_dummy,          &
     &    bc_istack_dummy)
      call allocate_bc_item_dummy
!
!      write(*,*) 'read_group_item_gz', num_nod_bc_dummy
      call read_group_item_gz(num_bc_dummy, num_nod_bc_dummy,           &
     &    bc_istack_dummy, bc_name_dummy, bc_item_dummy)
!
!  read element group ( not in use)
!
      call skip_gz_comment_int(num_mat_dummy)
!      write(*,*) 'num_mat_dummy', num_mat_dummy
!
      call allocate_bc_ele_stack_dummy
!
      call read_group_stack_gz(num_mat_dummy, num_mat_bc_dummy,         &
     &    mat_istack_dummy)
      call allocate_bc_ele_item_dummy
!
      call read_group_item_gz(num_mat_dummy, num_mat_bc_dummy,          &
     &    mat_istack_dummy, mat_name_dummy, mat_item_dummy)
!
!  read surface group
!
      call skip_gz_comment_int(num_surf_dummy)
!      write(*,*) 'num_surf_dummy', num_surf_dummy
!
      call allocate_bc_sf_stack_dummy
!
      call read_group_stack_gz(num_surf_dummy, num_surf_bc_dummy,       &
     &    surf_istack_dummy)
      call allocate_bc_sf_item_dummy
!
      call read_surface_group_item_gz(num_surf_dummy,                   &
     &    num_surf_bc_dummy, surf_istack_dummy, surf_name_dummy,        &
     &    surf_item_dummy)
!
      end subroutine read_boundary_data_gz
!
! ----------------------------------------------------------------------
!
      subroutine write_boundary_data_gz
!
      use gz_group_data_IO
!
!   write node group
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4. group information  ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4.1 node group ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_group_data_gz(num_bc_dummy, num_nod_bc_dummy,          &
     &    bc_istack_dummy, bc_name_dummy, bc_item_dummy)
!
      call deallocate_bc_item_dummy
!
!  write element group
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4.2 element group ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_group_data_gz(num_mat_dummy, num_mat_bc_dummy,         &
     &    mat_istack_dummy, mat_name_dummy, mat_item_dummy)
!
      call deallocate_bc_ele_item_dummy
!
!  write surface group
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4.3 surface group ', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_surf_group_data_gz(num_surf_dummy, num_surf_bc_dummy,  &
     &    surf_istack_dummy, surf_name_dummy, surf_item_dummy)
!
      call deallocate_bc_sf_item_dummy
!
      end subroutine write_boundary_data_gz
!
! ----------------------------------------------------------------------
!
      end module gz_boundary_data_IO
