!
!      module set_cutshell_node_data
!
!     Written by H. Matsui
!     Modified by H. Matsui on Oct., 2007
!
!      subroutine set_new_node_4_hemi(org_node, new_node)
!      subroutine set_new_node_4_cut_shell(org_node, new_node)
!      subroutine set_new_node_outer_core(org_node, nod_grp, new_node)
!      subroutine set_new_node_hemi_o_core(org_node, nod_grp, new_node)
!
      module set_cutshell_node_data
!
      use m_precision
!
      use m_cutshell_nod_ele_flag
!
      use t_geometry_data
      use t_group_data
!
      implicit none
!
      real(kind = kreal), private :: r_CMB, r_ICB
!
      private :: count_new_position_4_hemi
      private :: count_new_position_cut_shell
      private :: set_new_position_4_hemi
      private :: set_new_position_cut_shell
      private :: count_position_outer_core,   set_position_outer_core
      private :: count_position_h_outer_core, set_position_h_outer_core
      private :: set_boundary_radii
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_node_4_hemi(org_node, new_node)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
!
!
      call count_new_position_4_hemi(org_node, new_node)
!
      call allocate_node_geometry_type(new_node)
      call set_new_position_4_hemi(org_node, new_node)
!
      end subroutine set_new_node_4_hemi
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_node_4_cut_shell(org_node, new_node)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
!
!
      call count_new_position_cut_shell(org_node, new_node)
!
      call allocate_node_geometry_type(new_node)
      call set_new_position_cut_shell(org_node, new_node)
!
      end subroutine set_new_node_4_cut_shell
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_node_outer_core(org_node, nod_grp, new_node)
!
      type(node_data), intent(in) :: org_node
      type(group_data), intent(in) :: nod_grp
      type(node_data), intent(inout) :: new_node
!
!
      call set_boundary_radii(org_node, nod_grp)
!
      call count_position_outer_core(org_node, new_node)
!
      call allocate_node_geometry_type(new_node)
      call set_position_outer_core(org_node, new_node)
!
      end subroutine set_new_node_outer_core
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_node_hemi_o_core(org_node, nod_grp, new_node)
!
      type(node_data), intent(in) :: org_node
      type(group_data), intent(in) :: nod_grp
      type(node_data), intent(inout) :: new_node
!
      call set_boundary_radii(org_node, nod_grp)
!
      call count_position_h_outer_core(org_node, new_node)
!
      call allocate_node_geometry_type(new_node)
      call set_position_h_outer_core(org_node, new_node)
!
      end subroutine set_new_node_hemi_o_core
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_new_position_4_hemi(org_node, new_node)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
      integer(kind = kint) :: inod
!
      new_node%numnod = 0
      do inod = 1, org_node%numnod
        if (org_node%xx(inod,3) .gt. -1.0d-11) then
          new_node%numnod = new_node%numnod + 1
         end if
      end do
      new_node%internal_node = new_node%numnod
!
      end subroutine count_new_position_4_hemi
!
!  ---------------------------------------------------------------------
!
      subroutine count_new_position_cut_shell(org_node, new_node)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
      integer(kind = kint) :: inod
!
      new_node%numnod = 0
      do inod = 1, org_node%numnod
        if (  org_node%xx(inod,1) .lt. 1.0d-11                          &
     &   .or. org_node%xx(inod,2) .lt. 1.0d-11                          &
     &   .or. org_node%xx(inod,3) .lt. 1.0d-11  ) then
          new_node%numnod = new_node%numnod + 1
        end if
      end do
      new_node%internal_node = new_node%numnod
!
      end subroutine count_new_position_cut_shell
!
!  ---------------------------------------------------------------------
!
      subroutine count_position_outer_core(org_node, new_node)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
      integer(kind = kint) :: inod
!
      new_node%numnod = 0
      do inod = 1, org_node%numnod
        if      (org_node%rr(inod) .ge. r_ICB                           &
     &     .and. org_node%rr(inod) .le. r_CMB) then
          new_node%numnod = new_node%numnod + 1
        end if
      end do
      new_node%internal_node = new_node%numnod
!
      end subroutine count_position_outer_core
!
!  ---------------------------------------------------------------------
!
      subroutine count_position_h_outer_core(org_node, new_node)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
      integer(kind = kint) :: inod
!
      new_node%numnod = 0
      do inod = 1, org_node%numnod
        if     (org_node%rr(inod) .ge. r_ICB                            &
     &    .and. org_node%rr(inod) .le. r_CMB                            &
     &    .and. org_node%xx(inod,3) .gt. -1.0d-11) then
          new_node%numnod = new_node%numnod + 1
        end if
      end do
      new_node%internal_node = new_node%numnod
!
      end subroutine count_position_h_outer_core
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_new_position_4_hemi(org_node, new_node)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
      integer(kind = kint) :: inod, icou
!
      icou = 0
      do inod = 1, org_node%numnod
!
        if ( org_node%xx(inod,3) .gt. -1.0d-11 ) then
          icou = icou + 1
          new_node%inod_global(icou) = icou
          new_node%xx(icou,1:3) = org_node%xx(inod,1:3)
!
          mark_new_node(inod) = icou
        end if
!
      end do
!
      end subroutine set_new_position_4_hemi
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_position_cut_shell(org_node, new_node)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
      integer(kind = kint) :: inod, icou
!
      icou = 0
      do inod = 1, org_node%numnod
!
        if (  org_node%xx(inod,1) .lt. 1.0d-11                          &
     &   .or. org_node%xx(inod,2) .lt. 1.0d-11                          &
     &   .or. org_node%xx(inod,3) .lt. 1.0d-11  ) then
          icou = icou + 1
          new_node%inod_global(icou) = icou
          new_node%xx(icou,1:3) = org_node%xx(inod,1:3)
!
          mark_new_node(inod) = icou
        end if
!
      end do
!
      end subroutine set_new_position_cut_shell
!
!  ---------------------------------------------------------------------
!
      subroutine set_position_outer_core(org_node, new_node)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
      integer(kind = kint) :: inod, icou
!
      icou = 0
      do inod = 1, org_node%numnod
!
        if       (org_node%rr(inod) .ge. r_ICB                          &
     &      .and. org_node%rr(inod) .le. r_CMB) then
          icou = icou + 1
          new_node%inod_global(icou) = icou
          new_node%xx(icou,1:3) = org_node%xx(inod,1:3)
!
          mark_new_node(inod) = icou
        end if
!
      end do
!
      end subroutine set_position_outer_core
!
!  ---------------------------------------------------------------------
!
      subroutine set_position_h_outer_core(org_node, new_node)
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
      integer(kind = kint) :: inod, icou
!
      icou = 0
      do inod = 1, org_node%numnod
!
        if     (org_node%rr(inod) .ge. r_ICB                            &
     &    .and. org_node%rr(inod) .le. r_CMB                            &
     &    .and. org_node%xx(inod,3) .gt. -1.0d-11) then
          icou = icou + 1
          new_node%inod_global(icou) = icou
          new_node%xx(icou,1:3) = org_node%xx(inod,1:3)
!
          mark_new_node(inod) = icou
        end if
!
      end do
!
      end subroutine set_position_h_outer_core
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_radii(node, nod_grp)
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      integer(kind = kint) :: i, inod, inum
!
      r_ICB = 1.0d11
      r_CMB = 0.0d0
      do i = 1, nod_grp%num_grp
        if (nod_grp%grp_name(i) .eq. 'ICB') then
          do inum = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
            inod = nod_grp%item_grp(inum)
            r_ICB = min(r_ICB,node%rr(inod))
          end do
        else if (nod_grp%grp_name(i) .eq. 'CMB') then
          do inum = nod_grp%istack_grp(i-1)+1, nod_grp%istack_grp(i)
            inod = nod_grp%item_grp(inum)
            r_CMB = max(r_CMB,node%rr(inod))
          end do
        end if
      end do
!
      end subroutine set_boundary_radii
!
!  ---------------------------------------------------------------------
!
      end module set_cutshell_node_data
