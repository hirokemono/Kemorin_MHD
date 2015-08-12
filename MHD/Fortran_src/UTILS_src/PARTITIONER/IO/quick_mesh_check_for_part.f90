!quick_mesh_check_for_part.f90
!
!      module  quick_mesh_check_for_part
!
      module  quick_mesh_check_for_part
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine quick_mesh_chk_4_part(nod_grp, ele_grp, sf_grp)
!
      use m_constants
      use m_error_IDs
      use m_geometry_data
      use t_group_data
!
      use set_nnod_for_ele_by_type
      use check_domain_prop_4_part
      use error_exit_4_part
!
      type(group_data), intent(in) :: nod_grp
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
!
      integer(kind=kint) :: ierr
      integer(kind=kint) ::  is, in, ik
!
!
      if (node1%numnod .le. 0) call ERROR_EXIT(ierr_ele, izero)
      if (ele1%numele .le. 0) call ERROR_EXIT(ierr_ele, izero)
!
      if (nod_grp%num_grp .lt. 0) call ERROR_EXIT(ierr_ngrp, ione)
      if (ele_grp%num_grp .lt. 0) call ERROR_EXIT(ierr_ngrp, itwo)
      if (sf_grp%num_grp .lt.  0) call ERROR_EXIT(ierr_ngrp, ithree)
!
      if (nod_grp%num_grp .gt. 0) then
        do is= 1, nod_grp%istack_grp(nod_grp%num_grp)
          in= nod_grp%item_grp(is)
          if (in .le. 0) call ERROR_EXIT(ierr_grp,ione)
          if (in .gt. node1%numnod) call ERROR_EXIT(ierr_ov_grp,ione)
        end do
      end if
!
      if (ele_grp%num_grp .gt. 0) then
        do is= 1, ele_grp%istack_grp(ele_grp%num_grp)
          in= ele_grp%item_grp(is)
          if (in .le. 0) call ERROR_EXIT(ierr_grp, itwo)
          if (in .gt. ele1%numele) call ERROR_EXIT(ierr_ov_grp, itwo)
        enddo
      endif
!
      if (sf_grp%num_grp.gt.0) then
        do is= 1, sf_grp%istack_grp(sf_grp%num_grp)
          in= sf_grp%item_sf_grp(1,is)
          ik= sf_grp%item_sf_grp(2,is)
          if (in.le.0) call ERROR_EXIT(ierr_grp, ithree)
          if (ik.le.0) call ERROR_EXIT(ierr_grp, ithree)
          if (in.gt.ele1%numele) call ERROR_EXIT(ierr_ov_grp, ithree)
        end do
      endif
!
!C
!C +--------------+
!C | ELEMENT-TYPE |
!C +--------------+
!C
      call set_num_node_for_ele_by_etype(ierr)
      if (ierr.gt.0) call ERROR_EXIT(ierr, izero)
!C
!C-- check local surface ID
      if (sf_grp%num_grp.gt.0) then
        call check_surface_def_in_surf_grp(ele1%numele,                 &
     &      sf_grp%num_item, elmtyp, sf_grp%item_sf_grp)
      end if
!
      end subroutine quick_mesh_chk_4_part
!
!   --------------------------------------------------------------------
!
      end module quick_mesh_check_for_part
