!groups_IO_b.f90
!      module groups_IO_b
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine read_group_data_b(mesh_file_id, group_IO)
!!      subroutine read_surf_grp_data_b(mesh_file_id, surf_grp_IO)
!!
!!      subroutine write_grp_data_b(mesh_file_id, group_IO)
!!      subroutine write_surf_grp_data_b(mesh_file_id, surf_grp_IO)
!!        type(group_data), intent(inout) :: group_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!
      module groups_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_group_data
!
      use group_data_IO_b
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_group_data_b(mesh_file_id, group_IO)
!
!
      integer(kind = kint), intent(in) :: mesh_file_id
      type(group_data), intent(inout) :: group_IO
!
!
      read(mesh_file_id) group_IO%num_grp
!
      call allocate_grp_type_num(group_IO)
!
      if (group_IO%num_grp .gt. 0) then
        call read_group_stack_b(mesh_file_id, group_IO%num_grp,         &
     &      group_IO%num_item, group_IO%istack_grp)
!
        call allocate_grp_type_item(group_IO)
        call read_group_item_b(mesh_file_id, group_IO%num_grp,          &
     &      group_IO%num_item, group_IO%istack_grp,                     &
     &      group_IO%grp_name, group_IO%item_grp)
!
      else
        call allocate_grp_type_item(group_IO)
      end if
!
      end subroutine read_group_data_b
!
!------------------------------------------------------------------
!
      subroutine read_surf_grp_data_b(mesh_file_id, surf_grp_IO)
!
      integer(kind = kint), intent(in) :: mesh_file_id
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
!
      read(mesh_file_id) surf_grp_IO%num_grp
!
      call allocate_sf_grp_type_num(surf_grp_IO)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call read_group_stack_b(mesh_file_id, surf_grp_IO%num_grp,      &
     &      surf_grp_IO%num_item, surf_grp_IO%istack_grp)
!
        call allocate_sf_grp_type_item(surf_grp_IO)
        call read_surface_group_item_b                                  &
     &     (mesh_file_id, surf_grp_IO%num_grp,                          &
     &      surf_grp_IO%num_item, surf_grp_IO%istack_grp,               &
     &      surf_grp_IO%grp_name, surf_grp_IO%item_sf_grp)
!
      else
        call allocate_sf_grp_type_item(surf_grp_IO)
      end if
!
      end subroutine read_surf_grp_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_grp_data_b(mesh_file_id, group_IO)
!
      integer(kind = kint), intent(in) :: mesh_file_id
      type(group_data), intent(inout) :: group_IO
!
!
      call write_group_data_b(mesh_file_id, group_IO%num_grp,           &
     &    group_IO%num_item, group_IO%istack_grp,                       &
     &    group_IO%grp_name, group_IO%item_grp)
!
      call deallocate_grp_type(group_IO)
!
      end subroutine write_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine write_surf_grp_data_b(mesh_file_id, surf_grp_IO)
!
      integer(kind = kint), intent(in) :: mesh_file_id
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
!
      call write_surface_group_data_b                                   &
     &   (mesh_file_id, surf_grp_IO%num_grp,                            &
     &    surf_grp_IO%num_item, surf_grp_IO%istack_grp,                 &
     &    surf_grp_IO%grp_name, surf_grp_IO%item_sf_grp)
!
      call deallocate_sf_grp_type(surf_grp_IO)
!
      end subroutine write_surf_grp_data_b
!
!------------------------------------------------------------------
!
      end module groups_IO_b
