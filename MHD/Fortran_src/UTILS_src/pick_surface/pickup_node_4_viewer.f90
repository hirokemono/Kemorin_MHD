!
!      module pickup_node_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine set_node_position_4_viewer(node, inod_ksm, view_mesh)
!!        type(node_data), intent(in) :: node
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!
      module pickup_node_4_viewer
!
      use m_precision
      use t_surface_data
      use t_viewer_mesh
!
      implicit none
!
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_node_position_4_viewer(node, inod_ksm, view_mesh)
!
      use t_mesh_data
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: inod_ksm(node%numnod)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: inum, inod
!
!
!$omp parallel do
      do inum = 1, view_mesh%nnod_viewer
        view_mesh%inod_gl_view(inum) = inum
      end do
!$omp end parallel do
!
      do inod = 1, node%numnod
        inum = inod_ksm(inod)
        if(inum .gt. 0) view_mesh%xx_view(inum,1:3) = node%xx(inod,1:3)
      end do
!
      end subroutine set_node_position_4_viewer
!
!------------------------------------------------------------------
!
      end module pickup_node_4_viewer
