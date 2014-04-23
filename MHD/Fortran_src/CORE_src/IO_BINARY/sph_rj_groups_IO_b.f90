!sph_rj_groups_IO_b.f90
!      module sph_rj_groups_IO_b
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_modes_rj_groups_b(mesh_file_id)
!      subroutine write_modes_rj_groups_b(mesh_file_id)
!
      module sph_rj_groups_IO_b
!
      use m_precision
!
      use m_group_data_sph_specr_IO
      use group_data_IO_b
!
      implicit none
!
      private :: read_rj_radial_grp_data_b, write_rj_radial_grp_data_b
      private :: read_rj_sphere_grp_data_b, write_rj_sphere_grp_data_b
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_modes_rj_groups_b(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call read_rj_radial_grp_data_b(mesh_file_id)
      call read_rj_sphere_grp_data_b(mesh_file_id)
!
      end subroutine read_modes_rj_groups_b
!
!------------------------------------------------------------------
!
      subroutine write_modes_rj_groups_b(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call write_rj_radial_grp_data_b(mesh_file_id)
      call write_rj_sphere_grp_data_b(mesh_file_id)
!
      end subroutine write_modes_rj_groups_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_rj_radial_grp_data_b(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      read(mesh_file_id) num_radial_grp_rj_IO
!
      call allocate_rj_r_grp_IO_stack
!
      if (num_radial_grp_rj_IO .gt. 0) then
        call read_group_stack_b(mesh_file_id, num_radial_grp_rj_IO,     &
     &      ntot_radial_grp_rj_IO, istack_radial_grp_rj_IO)
!
        call allocate_rj_r_grp_IO_item
        call read_group_item_b(mesh_file_id, num_radial_grp_rj_IO,      &
     &      ntot_radial_grp_rj_IO, istack_radial_grp_rj_IO,             &
     &      name_radial_grp_rj_IO, item_radial_grp_rj_IO)
!
      else
        ntot_radial_grp_rj_IO = 0
        call allocate_rj_r_grp_IO_item
      end if
!
      end subroutine read_rj_radial_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine read_rj_sphere_grp_data_b(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      read(mesh_file_id) num_sphere_grp_rj_IO
!
      call allocate_rj_j_grp_IO_stack
!
      if (num_sphere_grp_rj_IO .gt. 0) then
        call read_group_stack_b(mesh_file_id, num_sphere_grp_rj_IO,     &
     &      ntot_sphere_grp_rj_IO, istack_sphere_grp_rj_IO)
!
        call allocate_rj_j_grp_IO_item
        call read_group_item_b(mesh_file_id, num_sphere_grp_rj_IO,      &
     &      ntot_sphere_grp_rj_IO, istack_sphere_grp_rj_IO,             &
     &      name_sphere_grp_rj_IO, item_sphere_grp_rj_IO)
!
      else
        ntot_sphere_grp_rj_IO = 0
        call allocate_rj_j_grp_IO_item
      end if
!
      end subroutine read_rj_sphere_grp_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_rj_radial_grp_data_b(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      call write_group_data_b(mesh_file_id, num_radial_grp_rj_IO,       &
     &    ntot_radial_grp_rj_IO, istack_radial_grp_rj_IO,               &
     &    name_radial_grp_rj_IO, item_radial_grp_rj_IO)
!
      call deallocate_rj_r_grp_IO_item
!
      end subroutine write_rj_radial_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine write_rj_sphere_grp_data_b(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      call write_group_data_b(mesh_file_id, num_sphere_grp_rj_IO,       &
     &    ntot_sphere_grp_rj_IO, istack_sphere_grp_rj_IO,               &
     &    name_sphere_grp_rj_IO, item_sphere_grp_rj_IO)
!
      call deallocate_rj_j_grp_IO_item
!
      end subroutine write_rj_sphere_grp_data_b
!
!------------------------------------------------------------------
!
      end module sph_rj_groups_IO_b
