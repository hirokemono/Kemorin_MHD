!
!      module mesh_interpolation
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine interpolation_4_mesh_test(org_mesh, dest_mesh)
!
      module mesh_interpolation
!
      use m_precision
!
        implicit none
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine interpolation_4_mesh_test(org_mesh, dest_mesh)
!
      use calypso_mpi
      use m_machine_parameter
      use m_ctl_params_4_gen_table
      use m_interpolated_geometry
      use m_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use interpolate_position
      use m_read_mesh_data
      use t_mesh_data
!
      use check_ineterppolated_mesh
!
      type(mesh_geometry), intent(inout) :: org_mesh
      type(mesh_geometry), intent(in) :: dest_mesh
!
!     return global node from table
!
      if (iflag_debug.eq.1)   write(*,*) 's_interpolate_global_node'
      call s_interpolate_global_node                                    &
     &   (dest_mesh%node%numnod, dest_mesh%nod_comm,                    &
     &    itp1_org, itp1_dest)
!
!     interpolate 2nd mesh from 1st mesh
!
      if (iflag_debug.eq.1)   write(*,*) 's_interpolate_position'
      call s_interpolate_position                                       &
     &   (org_mesh%node, dest_mesh%node%numnod, dest_mesh%nod_comm)
!      if (iflag_debug.eq.1)   write(*,*) 's_interpolate_position_by_N'
!      call s_interpolate_position_by_N                                 &
!     &   (org_mesh%node, dest_mesh%node%numnod, dest_mesh%nod_comm)
!      if (iflag_debug.eq.1)   write(*,*) 's_interpolate_position_by_s'
!      call s_interpolate_position_by_s                                 &
!     &   (org_mesh%node, dest_mesh%node%%numnod, dest_mesh%nod_comm)
!
!
      if (iflag_debug.gt.0)  write(*,*) 's_check_ineterppolated_mesh'
      call s_check_ineterppolated_mesh(dest_mesh%node)
!
      end subroutine interpolation_4_mesh_test
!
! ----------------------------------------------------------------------
!
      end module mesh_interpolation
