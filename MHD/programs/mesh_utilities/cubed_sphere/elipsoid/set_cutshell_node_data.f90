!
!      module set_cutshell_node_data
!
      module set_cutshell_node_data
!
!     Written by H. Matsui
!     Modified by H. Matsui on Oct., 2007
!
      use m_precision
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_cutshell_nod_ele_flag
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
!      subroutine set_new_node_4_hemi
!      subroutine set_new_node_4_cut_shell
!      subroutine set_new_node_outer_core
!      subroutine set_new_node_hemi_o_core
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_node_4_hemi
!
      call count_new_position_4_hemi
!
      call allocate_2nd_node_position
      call set_new_position_4_hemi
!
      end subroutine set_new_node_4_hemi
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_node_4_cut_shell
!
      call count_new_position_cut_shell
!
      call allocate_2nd_node_position
      call set_new_position_cut_shell
!
      end subroutine set_new_node_4_cut_shell
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_node_outer_core
!
!
      call set_boundary_radii
!
      call count_position_outer_core
!
      call allocate_2nd_node_position
      call set_position_outer_core
!
      end subroutine set_new_node_outer_core
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_node_hemi_o_core
!
      call set_boundary_radii
!
      call count_position_h_outer_core
!
      call allocate_2nd_node_position
      call set_position_h_outer_core
!
      end subroutine set_new_node_hemi_o_core
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_new_position_4_hemi
!
      integer(kind = kint) :: inod
!
      nnod_2nd = 0
      do inod = 1, numnod
        if (xx(inod,3) .gt. -1.0d-11) nnod_2nd = nnod_2nd + 1
      end do
      internal_nod_2nd = nnod_2nd
!
      end subroutine count_new_position_4_hemi
!
!  ---------------------------------------------------------------------
!
      subroutine count_new_position_cut_shell
!
      integer(kind = kint) :: inod
!
      nnod_2nd = 0
      do inod = 1, numnod
        if (  xx(inod,1) .lt. 1.0d-11 .or. xx(inod,2) .lt. 1.0d-11      &
     &   .or. xx(inod,3) .lt. 1.0d-11  ) then
          nnod_2nd = nnod_2nd + 1
        end if
      end do
      internal_nod_2nd = nnod_2nd
!
      end subroutine count_new_position_cut_shell
!
!  ---------------------------------------------------------------------
!
      subroutine count_position_outer_core
!
      integer(kind = kint) :: inod
!
      nnod_2nd = 0
      do inod = 1, numnod
        if ( radius(inod) .ge. r_ICB .and. radius(inod).le. r_CMB) then
          nnod_2nd = nnod_2nd + 1
        end if
      end do
      internal_nod_2nd = nnod_2nd
!
      end subroutine count_position_outer_core
!
!  ---------------------------------------------------------------------
!
      subroutine count_position_h_outer_core
!
      integer(kind = kint) :: inod
!
      nnod_2nd = 0
      do inod = 1, numnod
        if ( radius(inod) .ge. r_ICB .and. radius(inod).le. r_CMB       &
     &    .and. xx(inod,3) .gt. -1.0d-11) then
          nnod_2nd = nnod_2nd + 1
        end if
      end do
      internal_nod_2nd = nnod_2nd
!
      end subroutine count_position_h_outer_core
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_new_position_4_hemi
!
      integer(kind = kint) :: inod, icou
!
      icou = 0
      do inod = 1, numnod
!
        if ( xx(inod,3) .gt. -1.0d-11 ) then
          icou = icou + 1
          globalnodid_2nd(icou) = icou
          xx_2nd(icou,1:3) = xx(inod,1:3)
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
      subroutine set_new_position_cut_shell
!
      integer(kind = kint) :: inod, icou
!
      icou = 0
      do inod = 1, numnod
!
        if (  xx(inod,1) .lt. 1.0d-11 .or. xx(inod,2) .lt. 1.0d-11      &
     &   .or. xx(inod,3) .lt. 1.0d-11  ) then
          icou = icou + 1
          globalnodid_2nd(icou) = icou
          xx_2nd(icou,1:3) = xx(inod,1:3)
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
      subroutine set_position_outer_core
!
      integer(kind = kint) :: inod, icou
!
      icou = 0
      do inod = 1, numnod
!
        if ( radius(inod) .ge. r_ICB .and. radius(inod).le. r_CMB) then
          icou = icou + 1
          globalnodid_2nd(icou) = icou
          xx_2nd(icou,1:3) = xx(inod,1:3)
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
      subroutine set_position_h_outer_core
!
      integer(kind = kint) :: inod, icou
!
      icou = 0
      do inod = 1, numnod
!
        if ( radius(inod) .ge. r_ICB .and. radius(inod).le. r_CMB       &
     &    .and. xx(inod,3) .gt. -1.0d-11) then
          icou = icou + 1
          globalnodid_2nd(icou) = icou
          xx_2nd(icou,1:3) = xx(inod,1:3)
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
      subroutine set_boundary_radii
!
      use m_node_group
!
      integer(kind = kint) :: i, inod, inum
!
      r_ICB = 1.0d11
      r_CMB = 0.0d0
      do i = 1, num_bc
        if (bc_name(i) .eq. 'ICB') then
          do inum = bc_istack(i-1)+1, bc_istack(i)
            inod = bc_item(inum)
            r_ICB = min(r_ICB,radius(inod))
          end do
        else if (bc_name(i) .eq. 'CMB') then
          do inum = bc_istack(i-1)+1, bc_istack(i)
            inod = bc_item(inum)
            r_CMB = max(r_CMB,radius(inod))
          end do
        end if
      end do
!
      end subroutine set_boundary_radii
!
!  ---------------------------------------------------------------------
!
      end module set_cutshell_node_data
