!
!      module set_cutshell_surf_grp
!
      module set_cutshell_surf_grp
!
!     Written by H. Matsui
!
      use m_precision
!
      use m_surface_group
      use m_2nd_group_data
      use m_cutshell_nod_ele_flag
!
      implicit none
!
      private :: count_new_surf_group,  set_new_surf_group
      private :: count_equator_surface, set_equator_surface
!
!      subroutine s_set_new_surface_grp_4_hemi
!      subroutine s_set_new_surface_grp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_new_surface_grp_4_hemi
!
!
      num_surf_2nd =  num_surf + 1
      call allocate_2nd_surf_grp_num
!
      call count_new_surf_group
      call count_equator_surface
!
!
      call allocate_2nd_surf_grp_item
!
      call set_new_surf_group
!
      call set_equator_surface
!
      end subroutine s_set_new_surface_grp_4_hemi
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_new_surface_grp
!
!
      num_surf_2nd =  num_surf
      call allocate_2nd_surf_grp_num
!
      call count_new_surf_group
!
      call allocate_2nd_surf_grp_item
!
      call set_new_surf_group
!
      end subroutine s_set_new_surface_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_new_surf_group
!
      integer(kind = kint) :: i, iele, inum, icou
!
!
      surf_name_2nd(1:num_surf) = surf_name(1:num_surf)
!
      do i = 1, num_surf
         surf_istack_2nd(i) = surf_istack_2nd(i-1)
         do inum = surf_istack(i-1)+1, surf_istack(i)
           iele = surf_item(1,inum)
           if ( mark_new_ele(iele) .ne. 0 ) then
             surf_istack_2nd(i) = surf_istack_2nd(i) + 1
           end if
         end do
      end do
      num_surf_bc_2nd = surf_istack_2nd(num_surf)
!
      end subroutine count_new_surf_group
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_surf_group
!
     integer(kind = kint) :: iele, inum, i, icou
!
      icou = 0
      do i = 1, num_surf
         do inum = surf_istack(i-1)+1, surf_istack(i)
           iele = surf_item(1,inum)
           if ( mark_new_ele(iele) .ne. 0 ) then
             icou = icou + 1
             surf_item_2nd(1,icou) = mark_new_ele(iele)
             surf_item_2nd(2,icou) = surf_item(2,inum)
           end if
         end do
      end do
!
      end subroutine set_new_surf_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_equator_surface
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      integer(kind = kint) :: iele, inum, i, k, inod, isig
!
!
      surf_name_2nd(num_surf_2nd) = 'equator_surf'
!
      surf_istack_2nd(num_surf_2nd) = surf_istack_2nd(num_surf)
      do iele = 1, ele_2nd%numele
        do inum = 1, nsurf_4_ele
          isig = 1
          do i = 1, num_linear_sf
            k = node_on_sf_4(i,inum)
            inod = ele_2nd%ie(iele,k)
            if ( xx_2nd(inod,3) .gt. 1.0d-11 ) isig = 0
          end do
          if (isig .eq. 1) then
            surf_istack_2nd(num_surf_2nd)                               &
     &            = surf_istack_2nd(num_surf_2nd) + 1
          end if
        end do
      end do
      num_surf_bc_2nd = surf_istack_2nd(num_surf_2nd)
!
      end subroutine count_equator_surface
!
!  ---------------------------------------------------------------------
!
      subroutine set_equator_surface
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      integer(kind = kint) :: iele, inum, i, k, inod, icou, isig
!
!
      icou = surf_istack_2nd(num_surf)
      do iele = 1, ele_2nd%numele
        do inum = 1, nsurf_4_ele
          isig = 1
          do i = 1, num_linear_sf
            k = node_on_sf_4(i,inum)
            inod = ele_2nd%ie(iele,k)
            if ( xx_2nd(inod,3) .gt. 1.0d-11 ) isig = 0
          end do
          if (isig .eq. 1) then
            icou = icou + 1
            surf_item_2nd(1,icou) = iele
            surf_item_2nd(2,icou) = inum
          end if
        end do
      end do
!
      end subroutine set_equator_surface
!
!  ---------------------------------------------------------------------
!
      end module set_cutshell_surf_grp
