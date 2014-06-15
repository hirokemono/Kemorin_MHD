!link_2nd_group_type.f90
!     module link_2nd_group_type
!
!      Written by H. Matsui on Sep., 2006
!
!      subroutine s_link_2nd_group_type(group)
!         type(mesh_groups), intent(in)  ::   group
!       subroutine link_2nd_nod_group_type(nod_grp)
!         type (group_data), intent(in) :: nod_grp
!       subroutine link_2nd_ele_group_type(ele_grp)
!         type (group_data), intent(in) :: ele_grp
!       subroutine link_2nd_surf_group_type(surf_grp)
!         type (surface_group_data), intent(in) :: surf_grp
!
!      subroutine link_2nd_ele_grp_tbl_type(ele_grp_tbl)
!        type(element_group_table), intent(in) :: ele_grp_tbl
!      subroutine link_2nd_surf_grp_tbl_type(surf_grp_tbl)
!        type(surface_group_table), intent(in) :: surf_grp_tbl
!
      module link_2nd_group_type
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_link_2nd_group_type(group)
!
      use t_mesh_data
!
      type(mesh_groups), intent(in)  ::   group
!
!
      call link_2nd_nod_group_type(group%nod_grp)
      call link_2nd_ele_group_type(group%ele_grp)
      call link_2nd_surf_group_type(group%surf_grp)
!
      end subroutine s_link_2nd_group_type
!
!  ---------------------------------------------------------------------
!
       subroutine link_2nd_nod_group_type(nod_grp)
!
       use m_2nd_group_data
       use t_group_data
!
        type (group_data), intent(in) :: nod_grp
!
!
       num_bc_2nd =     nod_grp%num_grp
       num_nod_bc_2nd = nod_grp%num_item
!
       bc_name_2nd =>   nod_grp%grp_name
       bc_istack_2nd => nod_grp%istack_grp
       bc_item_2nd =>   nod_grp%item_grp
!
       end subroutine link_2nd_nod_group_type
!
!  ---------------------------------------------------------------------
!
       subroutine link_2nd_ele_group_type(ele_grp)
!
       use m_2nd_group_data
       use t_group_data
!
        type (group_data), intent(in) :: ele_grp
!
!
       num_mat_2nd =    ele_grp%num_grp
       num_mat_bc_2nd = ele_grp%num_item
!
       mat_name_2nd =>   ele_grp%grp_name
       mat_istack_2nd => ele_grp%istack_grp
       mat_item_2nd =>   ele_grp%item_grp
!
       end subroutine link_2nd_ele_group_type
!
!  ---------------------------------------------------------------------
!
       subroutine link_2nd_surf_group_type(surf_grp)
!
       use m_2nd_group_data
       use t_group_data
!
        type (surface_group_data), intent(in) :: surf_grp
!
!
       num_surf_2nd =    surf_grp%num_grp
       num_surf_bc_2nd = surf_grp%num_item
!
       surf_name_2nd =>   surf_grp%grp_name
       surf_istack_2nd => surf_grp%istack_grp
       surf_item_2nd =>   surf_grp%item_sf_grp
!
       end subroutine link_2nd_surf_group_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_2nd_ele_grp_tbl_type(ele_grp_tbl)
!
      use t_group_connects
      use m_2nd_group_data
!
      type(element_group_table), intent(in) :: ele_grp_tbl
!
      call link_ele_grp_connect_type(ele_grp_tbl, ele_grp_tbl_2nd)
!
      end subroutine link_2nd_ele_grp_tbl_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_2nd_surf_grp_tbl_type(surf_grp_tbl)
!
      use t_group_connects
      use m_2nd_group_data
!
      type(surface_group_table), intent(in) :: surf_grp_tbl
!
!
      call link_surf_grp_connect_type(surf_grp_tbl, sf_grp_tbl_2nd)
!
      end subroutine link_2nd_surf_grp_tbl_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module link_2nd_group_type
