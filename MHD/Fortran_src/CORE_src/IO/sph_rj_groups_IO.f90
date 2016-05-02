!>@file  sph_rj_groups_IO.f90
!!       module sph_rj_groups_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Base routines for spectrum group data IO
!!
!!@verbatim
!!      subroutine read_modes_rj_groups(mesh_file_id)
!!      subroutine read_geom_rtp_groups(mesh_file_id)
!!      subroutine write_modes_rj_groups(mesh_file_id)
!!      subroutine write_geom_rtp_groups(mesh_file_id)
!!@endverbatim
!
      module sph_rj_groups_IO
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use stack_array_IO
      use group_data_IO
      use skip_comment_f
!
      implicit none
!
      character(len=255) :: character_4_read = ''
      private :: character_4_read
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_modes_rj_groups(mesh_file_id)
!
      use m_group_data_sph_specr_IO
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      call read_rj_grp_data(mesh_file_id, radial_rj_grp_IO)
      call read_rj_grp_data(mesh_file_id, sphere_rj_grp_IO)
!
      end subroutine read_modes_rj_groups
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_groups(mesh_file_id)
!
      use m_group_data_sph_specr_IO
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      if(iflag_debug .gt. 0) write(*,*) 'read_rtp_node_grp_data'
      call read_rj_grp_data(mesh_file_id, bc_rtp_grp_IO)
!
      if(iflag_debug .gt. 0) write(*,*) 'read_rtp_radial_grp_data'
      call read_rj_grp_data(mesh_file_id, radial_rtp_grp_IO)
!
      if(iflag_debug .gt. 0) write(*,*) 'read_rtp_theta_grp_data'
      call read_rj_grp_data(mesh_file_id, theta_rtp_grp_IO)
!
      if(iflag_debug .gt. 0) write(*,*) 'read_rtp_zonal_grp_data'
      call read_rj_grp_data(mesh_file_id, zonal_rtp_grp_IO)
!
      end subroutine read_geom_rtp_groups
!
!------------------------------------------------------------------
!
      subroutine write_modes_rj_groups(mesh_file_id)
!
      use m_sph_modes_grid_labels
      use m_group_data_sph_specr_IO
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      write(mesh_file_id,'(a)', advance='NO') hd_grphd()
      write(mesh_file_id,'(a)', advance='NO') hd_kgrphd()
      call write_rj_grp_data(mesh_file_id, radial_rj_grp_IO)
      write(mesh_file_id,'(a)', advance='NO') hd_jgrphd()
      call write_rj_grp_data(mesh_file_id, sphere_rj_grp_IO)
!
      end subroutine write_modes_rj_groups
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_groups(mesh_file_id)
!
      use m_sph_modes_grid_labels
      use m_group_data_sph_specr_IO
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      write(mesh_file_id,'(a)', advance='NO') hd_grphd()
!
      write(mesh_file_id,'(a)', advance='NO') hd_ngrphd()
      call write_rj_grp_data(mesh_file_id, bc_rtp_grp_IO)
!
      if(iflag_debug .gt. 0) write(*,'(a)') 'write_rtp_radial_grp_data'
      write(mesh_file_id,'(a)', advance='NO') hd_rgrphd()
      call write_rj_grp_data(mesh_file_id, radial_rtp_grp_IO)
!
      if(iflag_debug .gt. 0) write(*,'(a)') 'write_rtp_theta_grp_data'
      write(mesh_file_id,'(a)', advance='NO') hd_tgrphd()
      call write_rj_grp_data(mesh_file_id, theta_rtp_grp_IO)
!
      if(iflag_debug .gt. 0) write(*,'(a)') 'write_rtp_zonal_grp_data'
      write(mesh_file_id,'(a)', advance='NO') hd_pgrphd()
      call write_rj_grp_data(mesh_file_id, zonal_rtp_grp_IO)
!
      end subroutine write_geom_rtp_groups
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_rj_grp_data(mesh_file_id, rj_grp_IO)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: mesh_file_id
      type(group_data), intent(inout) :: rj_grp_IO
!
!
      call skip_comment(character_4_read, mesh_file_id)
      read(character_4_read,*) rj_grp_IO%num_grp
!
      call allocate_grp_type_num(rj_grp_IO)
!
      if (rj_grp_IO%num_grp .gt. 0) then
        call read_group_stack(mesh_file_id, rj_grp_IO%num_grp,          &
     &      rj_grp_IO%num_item, rj_grp_IO%istack_grp)
!
        call allocate_grp_type_item(rj_grp_IO)
        call read_group_item(mesh_file_id, rj_grp_IO%num_grp,           &
     &      rj_grp_IO%num_item, rj_grp_IO%istack_grp,                   &
     &      rj_grp_IO%grp_name, rj_grp_IO%item_grp)
!
      else
        rj_grp_IO%num_item = 0
        call allocate_grp_type_item(rj_grp_IO)
      end if
!
      end subroutine read_rj_grp_data
!
!------------------------------------------------------------------
!
      subroutine write_rj_grp_data(mesh_file_id, rj_grp_IO)
!
      use m_sph_modes_grid_labels
      use t_group_data
!
      integer(kind = kint), intent(in) :: mesh_file_id
      type(group_data), intent(inout) :: rj_grp_IO
!
!
      call write_group_data(mesh_file_id, rj_grp_IO%num_grp,            &
     &    rj_grp_IO%num_item, rj_grp_IO%istack_grp,                     &
     &    rj_grp_IO%grp_name, rj_grp_IO%item_grp)
!
      call deallocate_grp_type(rj_grp_IO)
!
      end subroutine write_rj_grp_data
!
!------------------------------------------------------------------
!
      end module sph_rj_groups_IO
