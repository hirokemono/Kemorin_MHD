!
!      module renumber_surface_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!
!!      subroutine set_node_group_stack_viewer(num_pe, inod_sf_stack,   &
!!     &          merged_grp, ngrp_nod_sf, nod_nod_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!!        type(viewer_group_data), intent(inout) :: nod_nod_grp
!
      module renumber_surface_4_viewer
!
      use m_precision
!
      use t_viewer_mesh
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_node_group_stack_viewer(num_pe, inod_sf_stack,     &
     &          merged_grp, ngrp_nod_sf, nod_nod_grp)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: num_pe
      integer(kind = kint), intent(in) :: inod_sf_stack(0:num_pe)
      type(mesh_groups), intent(in) :: merged_grp
      integer(kind = kint), intent(in) :: ngrp_nod_sf
      type(viewer_group_data), intent(inout) :: nod_nod_grp
!
      integer(kind = kint) :: igrp, ip, idx, iref, ist, ied, inum, inod
!
!
      do igrp = 1, ngrp_nod_sf
        do ip = 1, num_pe
          idx = ip + (igrp-1) * num_pe
          iref = inod_sf_stack(ip)
          ist = nod_nod_grp%istack_sf(idx-1) + 1
          ied = merged_grp%nod_grp%istack_grp(igrp)
!
          nod_nod_grp%istack_sf(idx) = nod_nod_grp%istack_sf(idx-1)
          do inum = ist, ied
            inod = abs( nod_nod_grp%item_sf(inum) )
            if ( inod .gt. iref ) exit
            nod_nod_grp%istack_sf(idx:(num_pe*ngrp_nod_sf)) = inum
          end do
        end do
      end do
!
      end subroutine set_node_group_stack_viewer
!
!------------------------------------------------------------------
!
      end module renumber_surface_4_viewer
