!
!      module set_cutshell_surf_grp
!
!     Written by H. Matsui
!
!      subroutine s_set_new_surface_grp_4_hemi(newmesh, new_sf_grp)
!      subroutine s_set_new_surface_grp(new_sf_grp)
!
      module set_cutshell_surf_grp
!
      use m_precision
!
      use m_surface_group
      use m_cutshell_nod_ele_flag
      use t_mesh_data
      use t_group_data
!
      implicit none
!
      private :: count_new_surf_group,  set_new_surf_group
      private :: count_equator_surface, set_equator_surface
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_new_surface_grp_4_hemi(newmesh, new_sf_grp)
!
      type(mesh_geometry), intent(in) :: newmesh
      type(surface_group_data), intent(inout) :: new_sf_grp
!
!
      new_sf_grp%num_grp =  sf_grp1%num_grp + 1
      call allocate_sf_grp_type_num(new_sf_grp)
!
      call count_new_surf_group(new_sf_grp)
      call count_equator_surface(newmesh%node, newmesh%ele, new_sf_grp)
!
!
      call allocate_sf_grp_type_item(new_sf_grp)
!
      call set_new_surf_group(new_sf_grp)
!
      call set_equator_surface(newmesh%node, newmesh%ele, new_sf_grp)
!
      end subroutine s_set_new_surface_grp_4_hemi
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_new_surface_grp(new_sf_grp)
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
!
      new_sf_grp%num_grp =  sf_grp1%num_grp
      call allocate_sf_grp_type_num(new_sf_grp)
!
      call count_new_surf_group(new_sf_grp)
!
      call allocate_sf_grp_type_item(new_sf_grp)
      call set_new_surf_group(new_sf_grp)
!
      end subroutine s_set_new_surface_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_new_surf_group(new_sf_grp)
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint) :: i, iele, inum
!
!
      new_sf_grp%grp_name(1:sf_grp1%num_grp)                            &
     &     = surf_name(1:sf_grp1%num_grp)
!
      do i = 1, sf_grp1%num_grp
         new_sf_grp%istack_grp(i) = new_sf_grp%istack_grp(i-1)
         do inum = surf_istack(i-1)+1, surf_istack(i)
           iele = surf_item(1,inum)
           if ( mark_new_ele(iele) .ne. 0 ) then
             new_sf_grp%istack_grp(i) = new_sf_grp%istack_grp(i) + 1
           end if
         end do
      end do
      new_sf_grp%num_item = new_sf_grp%istack_grp(sf_grp1%num_grp)
!
      end subroutine count_new_surf_group
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_surf_group(new_sf_grp)
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
     integer(kind = kint) :: iele, inum, i, icou
!
      icou = 0
      do i = 1, sf_grp1%num_grp
         do inum = surf_istack(i-1)+1, surf_istack(i)
           iele = surf_item(1,inum)
           if ( mark_new_ele(iele) .ne. 0 ) then
             icou = icou + 1
             new_sf_grp%item_sf_grp(1,icou) = mark_new_ele(iele)
             new_sf_grp%item_sf_grp(2,icou) = surf_item(2,inum)
           end if
         end do
      end do
!
      end subroutine set_new_surf_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_equator_surface(new_node, new_ele, new_sf_grp)
!
      use m_geometry_constants
      use m_geometry_parameter
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint) :: iele, inum, i, k, inod, isig
!
!
      new_sf_grp%grp_name(new_sf_grp%num_grp) = 'equator_surf'
!
      new_sf_grp%istack_grp(new_sf_grp%num_grp)                         &
     &      = new_sf_grp%istack_grp(sf_grp1%num_grp)
      do iele = 1, new_ele%numele
        do inum = 1, nsurf_4_ele
          isig = 1
          do i = 1, num_linear_sf
            k = node_on_sf_4(i,inum)
            inod = new_ele%ie(iele,k)
            if ( new_node%xx(inod,3) .gt. 1.0d-11 ) isig = 0
          end do
          if (isig .eq. 1) then
            new_sf_grp%istack_grp(new_sf_grp%num_grp)                   &
     &            = new_sf_grp%istack_grp(new_sf_grp%num_grp) + 1
          end if
        end do
      end do
      new_sf_grp%num_item = new_sf_grp%istack_grp(new_sf_grp%num_grp)
!
      end subroutine count_equator_surface
!
!  ---------------------------------------------------------------------
!
      subroutine set_equator_surface(new_node, new_ele, new_sf_grp)
!
      use m_geometry_constants
      use m_geometry_parameter
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint) :: iele, inum, i, k, inod, icou, isig
!
!
      icou = new_sf_grp%istack_grp(sf_grp1%num_grp)
      do iele = 1, new_ele%numele
        do inum = 1, nsurf_4_ele
          isig = 1
          do i = 1, num_linear_sf
            k = node_on_sf_4(i,inum)
            inod = new_ele%ie(iele,k)
            if (new_node%xx(inod,3) .gt. 1.0d-11 ) isig = 0
          end do
          if (isig .eq. 1) then
            icou = icou + 1
            new_sf_grp%item_sf_grp(1,icou) = iele
            new_sf_grp%item_sf_grp(2,icou) = inum
          end if
        end do
      end do
!
      end subroutine set_equator_surface
!
!  ---------------------------------------------------------------------
!
      end module set_cutshell_surf_grp
