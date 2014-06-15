!
!      module m_2nd_group_data
!
!      Written by H. Matsui on Mar., 2006
!
!
!      subroutine allocate_2nd_node_group
!      subroutine allocate_2nd_element_group
!      subroutine allocate_2nd_surface_group
!
!      subroutine allocate_2nd_node_grp_num
!      subroutine allocate_2nd_ele_grp_num
!      subroutine allocate_2nd_surf_grp_num
!      subroutine allocate_2nd_node_grp_item
!      subroutine allocate_2nd_ele_grp_item
!      subroutine allocate_2nd_surf_grp_item
!
!      subroutine deallocate_2nd_groups
!      subroutine deallocate_2nd_node_group
!      subroutine deallocate_2nd_element_group
!      subroutine deallocate_2nd_surface_group
!
!      subroutine disconnect_2nd_group
!      subroutine disconnect_2nd_node_group
!      subroutine disconnect_2nd_element_group
!      subroutine disconnect_2nd_surface_group
!
!       subroutine allocate_2nd_node_grp_num_smp
!       subroutine allocate_2nd_ele_grp_num_smp
!       subroutine allocate_2nd_surf_grp_num_smp
!
!       subroutine deallocate_2nd_node_grp_num_smp
!       subroutine deallocate_2nd_ele_grp_num_smp
!       subroutine deallocate_2nd_surf_grp_num_smp
!
      module m_2nd_group_data
!
      use m_precision
      use t_group_data
      use t_group_connects
!
      implicit none
!
!      node group
!
      integer (kind=kint) :: num_bc_2nd
      integer (kind=kint) :: num_nod_bc_2nd
!
      integer (kind=kint), pointer :: bc_istack_2nd(:)
      integer (kind=kint), pointer :: bc_item_2nd(:)
!
      character (len=kchara), pointer :: bc_name_2nd(:)
!
!
      integer( kind=kint )  ::  num_bc_2nd_smp
!     number of element on whole processes
      integer( kind=kint ), pointer :: ibc_smp_2nd_stack(:)
!     number of element on this PE
      integer( kind=kint )  ::  max_bc_2nd_4_smp
!     number of element on whole processes
!
!      element group
!
      integer (kind=kint) :: num_mat_2nd
      integer (kind=kint) :: num_mat_bc_2nd
!
      integer (kind=kint), pointer :: mat_istack_2nd(:)
      integer (kind=kint), pointer :: mat_item_2nd(:)
! 
      character (len=kchara), pointer :: mat_name_2nd(:)
!
      integer( kind=kint )  ::  num_mat_2nd_smp
!     number of element on whole processes
      integer( kind=kint ), pointer :: imat_smp_2nd_stack(:)
!     number of element on this PE
      integer( kind=kint )  ::  max_mat_2nd_4_smp
!     number of element on whole processes
!
!    surface group
!
      integer(kind=kint) :: num_surf_2nd
      integer(kind=kint) :: num_surf_bc_2nd
!
      integer(kind=kint), pointer :: surf_istack_2nd(:)
      integer(kind=kint), pointer :: surf_item_2nd(:,:)
!
      character(len=kchara), pointer :: surf_name_2nd(:)
!
!
      integer( kind=kint )  ::  num_surf_2nd_smp
!     number of element on whole processes
      integer( kind=kint ), pointer :: isurf_grp_2nd_smp_stack(:)
!     number of element on this PE
      integer( kind=kint )  ::  max_sf_grp_2nd_4_smp
!     number of element on whole processes
!
!
      type(element_group_table), save :: ele_grp_tbl_2nd
!
!
      type(element_group_table), save :: ele_grp_tbl_2nd
!
      type(surface_group_table), save :: sf_grp_tbl_2nd
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_2nd_node_group
!
      call allocate_2nd_node_grp_num
      call allocate_2nd_node_grp_item
!
      end subroutine allocate_2nd_node_group
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_2nd_element_group
!
      call allocate_2nd_ele_grp_num
      call allocate_2nd_ele_grp_item
!
      end subroutine allocate_2nd_element_group
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_2nd_surface_group
!
      call allocate_2nd_surf_grp_num
      call allocate_2nd_surf_grp_item
!
      end subroutine allocate_2nd_surface_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_2nd_node_grp_num
!
      allocate(bc_name_2nd(num_bc_2nd))
      allocate(bc_istack_2nd(0:num_bc_2nd))
      bc_istack_2nd = 0
!
      end subroutine allocate_2nd_node_grp_num
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_2nd_ele_grp_num
!
      allocate(mat_name_2nd(num_mat_2nd))
      allocate(mat_istack_2nd(0:num_mat_2nd))
      mat_istack_2nd = 0
!
      end subroutine allocate_2nd_ele_grp_num
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_2nd_surf_grp_num
!
      allocate(surf_name_2nd(num_surf_2nd))
      allocate(surf_istack_2nd(0:num_surf_2nd))
      surf_istack_2nd = 0
!
      end subroutine allocate_2nd_surf_grp_num
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_2nd_node_grp_item
!
      allocate(bc_item_2nd(num_nod_bc_2nd))
      bc_item_2nd = 0
!
      end subroutine allocate_2nd_node_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_2nd_ele_grp_item
!
      allocate(mat_item_2nd(num_mat_bc_2nd))
      mat_item_2nd = 0
!
      end subroutine allocate_2nd_ele_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_2nd_surf_grp_item
!
      allocate(surf_item_2nd(2,num_surf_bc_2nd))
      surf_item_2nd = 0
!
      end subroutine allocate_2nd_surf_grp_item
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_2nd_groups
!
!
      if (num_surf_2nd .gt. 0) then
        call deallocate_2nd_surface_group
      end if
      if (num_mat_2nd .gt. 0) then
        call deallocate_2nd_element_group
      end if
      if (num_bc_2nd .gt. 0) then
        call deallocate_2nd_node_group
      end if
!
      end subroutine deallocate_2nd_groups
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_2nd_node_group
!
      deallocate(bc_name_2nd)
      deallocate(bc_istack_2nd)
      deallocate(bc_item_2nd)
!
      end subroutine deallocate_2nd_node_group
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_2nd_element_group
!
      deallocate(mat_name_2nd)
      deallocate(mat_istack_2nd)
      deallocate(mat_item_2nd)
!
      end subroutine deallocate_2nd_element_group
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_2nd_surface_group
!
      deallocate(surf_name_2nd)
      deallocate(surf_istack_2nd)
      deallocate(surf_item_2nd)
!
      end subroutine deallocate_2nd_surface_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine disconnect_2nd_groups
!
      call disconnect_2nd_surface_group
      call disconnect_2nd_element_group
      call disconnect_2nd_node_group
!
      end subroutine disconnect_2nd_groups
!
!  ---------------------------------------------------------------------
!
      subroutine disconnect_2nd_node_group
!
      nullify(bc_name_2nd)
      nullify(bc_istack_2nd)
      nullify(bc_item_2nd)
!
      end subroutine disconnect_2nd_node_group
!
!  ---------------------------------------------------------------------
!
      subroutine disconnect_2nd_element_group
!
      nullify(mat_name_2nd)
      nullify(mat_istack_2nd)
      nullify(mat_item_2nd)
!
      end subroutine disconnect_2nd_element_group
!
!  ---------------------------------------------------------------------
!
      subroutine disconnect_2nd_surface_group
!
      nullify(surf_name_2nd)
      nullify(surf_istack_2nd)
      nullify(surf_item_2nd)
!
      end subroutine disconnect_2nd_surface_group
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine allocate_2nd_node_grp_num_smp
!
       allocate( ibc_smp_2nd_stack(0:num_bc_2nd_smp))
       ibc_smp_2nd_stack = 0
!
       end subroutine allocate_2nd_node_grp_num_smp
!
!-----------------------------------------------------------------------
!
       subroutine allocate_2nd_ele_grp_num_smp
!
       allocate( imat_smp_2nd_stack(0:num_mat_2nd_smp))
       imat_smp_2nd_stack = 0
!
       end subroutine allocate_2nd_ele_grp_num_smp
!
!-----------------------------------------------------------------------
!
       subroutine allocate_2nd_surf_grp_num_smp
!
       allocate( isurf_grp_2nd_smp_stack(0:num_surf_2nd_smp))
       isurf_grp_2nd_smp_stack = 0
!
       end subroutine allocate_2nd_surf_grp_num_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine deallocate_2nd_node_grp_num_smp
!
       deallocate(ibc_smp_2nd_stack)
!
       end subroutine deallocate_2nd_node_grp_num_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_2nd_ele_grp_num_smp
!
       deallocate(imat_smp_2nd_stack)
!
       end subroutine deallocate_2nd_ele_grp_num_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_2nd_surf_grp_num_smp
!
       deallocate(isurf_grp_2nd_smp_stack)
!
       end subroutine deallocate_2nd_surf_grp_num_smp
!
!-----------------------------------------------------------------------
!
      end module  m_2nd_group_data
