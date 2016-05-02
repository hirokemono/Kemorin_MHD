!sph_rj_groups_IO_b.f90
!      module sph_rj_groups_IO_b
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_modes_rj_groups_b(mesh_file_id)
!      subroutine read_geom_rtp_groups_b(mesh_file_id)
!      subroutine write_modes_rj_groups_b(mesh_file_id)
!      subroutine write_geom_rtp_groups_b(mesh_file_id)
!
      module sph_rj_groups_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_group_data_sph_specr_IO
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
      subroutine read_modes_rj_groups_b(mesh_file_id)
!
      use m_group_data_sph_specr_IO
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call read_rj_grp_data_b(mesh_file_id, radial_rj_grp_IO)
      call read_rj_grp_data_b(mesh_file_id, sphere_rj_grp_IO)
!
      end subroutine read_modes_rj_groups_b
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_groups_b(mesh_file_id)
!
      use m_group_data_sph_specr_IO
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call read_rj_grp_data_b(mesh_file_id, bc_rtp_grp_IO)
      call read_rj_grp_data_b(mesh_file_id, radial_rtp_grp_IO)
      call read_rj_grp_data_b(mesh_file_id, theta_rtp_grp_IO)
      call read_rj_grp_data_b(mesh_file_id, zonal_rtp_grp_IO)
!
      end subroutine read_geom_rtp_groups_b
!
!------------------------------------------------------------------
!
      subroutine write_modes_rj_groups_b(mesh_file_id)
!
      use m_group_data_sph_specr_IO
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      call write_rj_grp_data_b(mesh_file_id, radial_rj_grp_IO)
      call write_rj_grp_data_b(mesh_file_id, sphere_rj_grp_IO)
!
      end subroutine write_modes_rj_groups_b
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_groups_b(mesh_file_id)
!
      use m_group_data_sph_specr_IO
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      call write_rj_grp_data_b(mesh_file_id, bc_rtp_grp_IO)
      call write_rj_grp_data_b(mesh_file_id, radial_rtp_grp_IO)
      call write_rj_grp_data_b(mesh_file_id, theta_rtp_grp_IO)
      call write_rj_grp_data_b(mesh_file_id, zonal_rtp_grp_IO)
!
      end subroutine write_geom_rtp_groups_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_rj_grp_data_b(mesh_file_id, rj_grp_IO)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: mesh_file_id
      type(group_data), intent(inout) :: rj_grp_IO
!
!
      read(mesh_file_id) rj_grp_IO%num_grp
!
      call allocate_grp_type_num(rj_grp_IO)
!
      if (rj_grp_IO%num_grp .gt. 0) then
        call read_group_stack_b(mesh_file_id, rj_grp_IO%num_grp,        &
     &      rj_grp_IO%num_item, rj_grp_IO%istack_grp)
!
        call allocate_grp_type_item(rj_grp_IO)
        call read_group_item_b(mesh_file_id, rj_grp_IO%num_grp,         &
     &      rj_grp_IO%num_item, rj_grp_IO%istack_grp,                   &
     &      rj_grp_IO%grp_name, rj_grp_IO%item_grp)
!
      else
        rj_grp_IO%num_item = 0
        call allocate_grp_type_item(rj_grp_IO)
      end if
!
      end subroutine read_rj_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine write_rj_grp_data_b(mesh_file_id, rj_grp_IO)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: mesh_file_id
      type(group_data), intent(inout) :: rj_grp_IO
!
!
      call write_group_data_b(mesh_file_id, rj_grp_IO%num_grp,          &
     &    rj_grp_IO%num_item, rj_grp_IO%istack_grp,                     &
     &    rj_grp_IO%grp_name, rj_grp_IO%item_grp)
!
      call deallocate_grp_type(rj_grp_IO)
!
      end subroutine write_rj_grp_data_b
!
!------------------------------------------------------------------
!
      end module sph_rj_groups_IO_b
