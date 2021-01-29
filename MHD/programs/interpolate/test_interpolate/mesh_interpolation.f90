!
!      module mesh_interpolation
!
!     Written by H. Matsui on Sep., 2006
!
!!      subroutine interpolation_4_mesh_test                            &
!!     &          (org_mesh, dest_mesh, itp_table)
!!        type(mesh_geometry), intent(inout) :: org_mesh
!!        type(mesh_geometry), intent(in) :: dest_mesh
!!        type(interpolate_table), intent(in) :: itp_table
!
      module mesh_interpolation
!
      use m_precision
!
      implicit none
!
      integer(kind = kint_gl), allocatable :: inod_global_itp(:)
      real(kind = kreal), allocatable :: xx_interpolate(:,:)
!
      private :: inod_global_itp, xx_interpolate
      private :: allocate_interpolate_geometry
      private :: deallocate_interpolate_geometry
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine interpolation_4_mesh_test                              &
     &          (org_mesh, dest_mesh, itp_table)
!
      use calypso_mpi
      use m_machine_parameter
      use interpolate_position
      use t_interpolate_table
      use t_mesh_data
      use m_array_for_send_recv
!
      use check_ineterppolated_mesh
!
      type(mesh_geometry), intent(inout) :: org_mesh
      type(mesh_geometry), intent(in) :: dest_mesh
      type(interpolate_table), intent(in) :: itp_table
!
!
      call allocate_interpolate_geometry(dest_mesh%node%numnod)
!
!     return global node from table
!
      if (iflag_debug.eq.1)   write(*,*) 's_interpolate_global_node'
      call s_interpolate_global_node(itp_table%iflag_itp_recv,          &
     &    dest_mesh%node%numnod, dest_mesh%nod_comm,                    &
     &    itp_table%tbl_org, itp_table%tbl_dest, inod_global_itp)
!
!     interpolate 2nd mesh from 1st mesh
!
      if (iflag_debug.eq.1)   write(*,*) 's_interpolate_position'
      call s_interpolate_position(org_mesh%node,                        &
     &   dest_mesh%node%numnod, dest_mesh%nod_comm, itp_table,          &
     &   xx_interpolate, vect1)
!      if (iflag_debug.eq.1)   write(*,*) 's_interpolate_position_by_N'
!      call s_interpolate_position_by_N(org_mesh%node,                  &
!     &   dest_mesh%node%numnod, dest_mesh%nod_comm, itp_table,         &
!     &   inod_global_itp, vect1)
!      if (iflag_debug.eq.1)   write(*,*) 's_interpolate_position_by_s'
!      call s_interpolate_position_by_S(org_mesh%node,                  &
!     &   dest_mesh%node%%numnod, dest_mesh%nod_comm, itp_table,        &
!     &   inod_global_itp, vect1)
!
!
      if (iflag_debug.gt.0)  write(*,*) 's_check_ineterppolated_mesh'
      call s_check_ineterppolated_mesh(dest_mesh%node,                  &
     &    inod_global_itp, xx_interpolate)
      call deallocate_interpolate_geometry
!
      end subroutine interpolation_4_mesh_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_interpolate_geometry(numnod)
!
      integer(kind = kint) :: numnod
!
      allocate( inod_global_itp(numnod) )
      allocate( xx_interpolate(numnod,3) )
!
      inod_global_itp = 0
      xx_interpolate = 0.0d0
!
      end subroutine allocate_interpolate_geometry
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_interpolate_geometry
!
      deallocate( inod_global_itp )
      deallocate( xx_interpolate )
!
      end subroutine deallocate_interpolate_geometry
!
! ----------------------------------------------------------------------
!
      end module mesh_interpolation
