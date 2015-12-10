!convert_group_2_linear.f90
!      module convert_group_2_linear
!
!      Written by H. Matsui on Mar., 2006
!
!      subroutine convert_ele_group_2_linear(num_mat, num_mat_bc,       &
!     &          mat_name, mat_istack, mat_item, num_mat_bc_l,          &
!     &          mat_name_l, mat_istack_l, mat_item_l)
!      subroutine convert_surf_group_2_linear(num_surf, num_surf_bc,    &
!     &          surf_name, surf_istack, surf_item, num_surf_bc_l,      &
!     &          surf_name_l, surf_istack_l, surf_item_l)
!
      module convert_group_2_linear
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
      subroutine convert_ele_group_2_linear(num_mat, num_mat_bc,        &
     &          mat_name, mat_istack, mat_item, num_mat_bc_l,           &
     &          mat_name_l, mat_istack_l, mat_item_l)
!
      integer(kind = kint), intent(in) :: num_mat, num_mat_bc
      integer(kind = kint), intent(in) :: num_mat_bc_l
      integer(kind = kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind = kint), intent(in) :: mat_item(num_mat_bc)
      character (len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(inout) :: mat_istack_l(0:num_mat)
      integer(kind = kint), intent(inout) :: mat_item_l(num_mat_bc_l)
      character (len=kchara), intent(inout) :: mat_name_l(num_mat)
!
      integer(kind = kint) :: inum, iele
      integer(kind = kint) :: inum1, inum2, inum3, inum4
      integer(kind = kint) :: inum5, inum6, inum7, inum8
!
!
      mat_name_l(1:num_mat) = mat_name(1:num_mat)
      mat_istack_l(0:num_mat) = 8*mat_istack(0:num_mat)
!
      do inum = 1, num_mat_bc
!
        iele = mat_item(inum)
!
        inum1 = 8*(inum-1) + 1
        inum2 = 8*(inum-1) + 2
        inum3 = 8*(inum-1) + 3
        inum4 = 8*(inum-1) + 4
        inum5 = 8*(inum-1) + 5
        inum6 = 8*(inum-1) + 6
        inum7 = 8*(inum-1) + 7
        inum8 = 8*(inum-1) + 8
!
        mat_item_l(inum1) = 8*(iele-1) + 1
        mat_item_l(inum2) = 8*(iele-1) + 2
        mat_item_l(inum3) = 8*(iele-1) + 3
        mat_item_l(inum4) = 8*(iele-1) + 4
        mat_item_l(inum5) = 8*(iele-1) + 5
        mat_item_l(inum6) = 8*(iele-1) + 6
        mat_item_l(inum7) = 8*(iele-1) + 7
        mat_item_l(inum8) = 8*(iele-1) + 8
!
      end do
!
      end subroutine convert_ele_group_2_linear
!
!  ---------------------------------------------------------------------
!
      subroutine convert_surf_group_2_linear(num_surf, num_surf_bc,     &
     &          surf_name, surf_istack, surf_item, num_surf_bc_l,       &
     &          surf_name_l, surf_istack_l, surf_item_l)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: num_surf_bc_l
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      integer(kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      character (len=kchara), intent(in) :: surf_name(num_surf)
!
      integer(kind = kint), intent(inout) :: surf_istack_l(0:num_surf)
      integer(kind = kint), intent(inout)                               &
     &                     :: surf_item_l(2,num_surf_bc_l)
      character (len=kchara), intent(inout) :: surf_name_l(num_surf)
!
      integer(kind = kint) :: inum, iele, isf
      integer(kind = kint) :: inum1, inum2, inum3, inum4
!
!
      surf_name_l(1:num_surf) = surf_name(1:num_surf)
      surf_istack_l(0:num_surf) = 4*surf_istack(0:num_surf)
!
      do inum = 1, num_surf_bc
!
        iele = surf_item(1,inum)
        isf =  surf_item(2,inum)
!
        inum1 = 4*(inum-1) + 1
        inum2 = 4*(inum-1) + 2
        inum3 = 4*(inum-1) + 3
        inum4 = 4*(inum-1) + 4
!
        surf_item_l(1,inum1) = 8*(iele-1) + node_on_sf_8(1,isf)
        surf_item_l(1,inum2) = 8*(iele-1) + node_on_sf_8(2,isf)
        surf_item_l(1,inum3) = 8*(iele-1) + node_on_sf_8(3,isf)
        surf_item_l(1,inum4) = 8*(iele-1) + node_on_sf_8(4,isf)
!
        surf_item_l(2,inum1) = isf
        surf_item_l(2,inum2) = isf
        surf_item_l(2,inum3) = isf
        surf_item_l(2,inum4) = isf
!
      end do
!
      end subroutine convert_surf_group_2_linear
!
!  ---------------------------------------------------------------------
!
      end module convert_group_2_linear
