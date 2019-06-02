!!set_cubed_sph_sf_group_ctl.f90
!!      module set_cubed_sph_sf_group_ctl
!!
!!        programmed by H.Matsui on Apr., 2006
!!
!!      subroutine set_cubed_sph_surface_grp_num(surf_grp_name_ctl,     &
!!     &          if_CMB, if_ICB, if_EXT, csp_surf_grp)
!!      subroutine set_cubed_sph_surface_grp_name(surf_grp_name_ctl,    &
!!     &          if_CMB, if_ICB, if_EXT, csp_surf_grp)
!!      subroutine set_cubed_sph_surface_grp_item                       &
!!     &         (surf_grp_name_ctl, surf_grp_layer_ctl,                &
!!     &          nlayer_ICB, nlayer_CMB, nlayer_EXT,                   &
!!     &          if_CMB, if_ICB, if_EXT, csp_surf_grp)
!!        type(ctl_array_ci), intent(in) :: surf_grp_name_ctl
!!        type(ctl_array_ci), intent(in) :: surf_grp_layer_ctl
!!        type(surface_group_data), intent(inout) :: csp_surf_grp
!
      module set_cubed_sph_sf_group_ctl
!
      use m_precision
      use t_read_control_arrays
      use t_group_data
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_surface_grp_num(surf_grp_name_ctl,       &
     &          if_CMB, if_ICB, if_EXT, csp_surf_grp)
!
      use t_control_array_charaint
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: surf_grp_name_ctl
!
      integer(kind = kint), intent(inout) :: if_CMB, if_ICB, if_EXT
      type(surface_group_data), intent(inout) :: csp_surf_grp
!
      integer(kind = kint) :: j
!
!
      if_CMB = 1
      if_ICB = 1
      if_EXT = 1
      if (surf_grp_name_ctl%icou .gt. 0) then
        do j = 1, surf_grp_name_ctl%num
          if(cmp_no_case(surf_grp_name_ctl%c_tbl(j), 'CMB_surf')        &
     &     .or. cmp_no_case(surf_grp_name_ctl%c_tbl(j), 'CMB')          &
     &     )  if_CMB = 0
          if(cmp_no_case(surf_grp_name_ctl%c_tbl(j), 'ICB_surf')        &
     &     .or. cmp_no_case(surf_grp_name_ctl%c_tbl(j), 'ICB')          &
     &     )  if_ICB = 0
          if(cmp_no_case(surf_grp_name_ctl%c_tbl(j), 'external_surf')   &
     &      .or. cmp_no_case(surf_grp_name_ctl%c_tbl(j), 'external')    &
     &     )  if_EXT = 0
        end do
!
        csp_surf_grp%num_grp                                            &
     &                  = surf_grp_name_ctl%num + if_CMB+if_ICB+if_EXT
      else
        csp_surf_grp%num_grp = if_CMB+if_ICB+if_EXT
      end if
!
      end subroutine set_cubed_sph_surface_grp_num
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_surface_grp_name(surf_grp_name_ctl,      &
     &          if_CMB, if_ICB, if_EXT, csp_surf_grp)
!
      use t_control_array_charaint
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: surf_grp_name_ctl
      integer(kind = kint), intent(in) :: if_CMB, if_ICB, if_EXT
!
      type(surface_group_data), intent(inout) :: csp_surf_grp
!
      integer(kind = kint) :: j, k
!
!
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        csp_surf_grp%nitem_grp(j) = 1
        csp_surf_grp%grp_name(j) = 'ICB_surf'
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        csp_surf_grp%nitem_grp(j) = 1
        csp_surf_grp%grp_name(j) = 'CMB_surf'
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        csp_surf_grp%nitem_grp(j) = 1
        csp_surf_grp%grp_name(j) = 'Infinity_surf'
      end if
!
      if (surf_grp_name_ctl%icou .gt. 0) then
        k = 1 + if_CMB+if_ICB+if_EXT
        csp_surf_grp%nitem_grp(k) = surf_grp_name_ctl%ivec(1)
        csp_surf_grp%grp_name(k) = surf_grp_name_ctl%c_tbl(1)
        do j = 2, surf_grp_name_ctl%num
          k = j + if_CMB+if_ICB+if_EXT
          csp_surf_grp%nitem_grp(k) = surf_grp_name_ctl%ivec(j)         &
     &                               - surf_grp_name_ctl%ivec(j-1)
          csp_surf_grp%grp_name(k) = surf_grp_name_ctl%c_tbl(j)
        end do
      end if
!
      csp_surf_grp%istack_grp(0) = 0
      do k = 1, csp_surf_grp%num_grp
        csp_surf_grp%istack_grp(k) = csp_surf_grp%istack_grp(k-1)       &
     &                              + csp_surf_grp%nitem_grp(k)
      end do
      csp_surf_grp%num_item                                             &
     &      = csp_surf_grp%istack_grp(csp_surf_grp%num_grp)
!
      end subroutine set_cubed_sph_surface_grp_name
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_surface_grp_item                         &
     &         (surf_grp_name_ctl, surf_grp_layer_ctl,                  &
     &          nlayer_ICB, nlayer_CMB, nlayer_EXT,                     &
     &          if_CMB, if_ICB, if_EXT, csp_surf_grp)
!
      use t_control_array_charaint
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: surf_grp_name_ctl
      type(ctl_array_ci), intent(in) :: surf_grp_layer_ctl
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: nlayer_EXT
      integer(kind = kint), intent(in) :: if_CMB, if_ICB, if_EXT
!
      type(surface_group_data), intent(inout) :: csp_surf_grp
!
      integer(kind = kint) :: j, k
!
!
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        csp_surf_grp%item_sf_grp(1,j) = nlayer_ICB
        csp_surf_grp%item_sf_grp(2,j) = 5
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        csp_surf_grp%item_sf_grp(1,j) = nlayer_CMB - 1
        csp_surf_grp%item_sf_grp(2,j) = 6
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        csp_surf_grp%item_sf_grp(1,j) = nlayer_EXT - 1
        csp_surf_grp%item_sf_grp(2,j) = 6
      end if
!
      if (surf_grp_name_ctl%icou .gt. 0) then
        do j = 1, surf_grp_layer_ctl%num
          k = j + csp_surf_grp%istack_grp(if_CMB+if_ICB+if_EXT)
          csp_surf_grp%item_sf_grp(1,k) = surf_grp_layer_ctl%ivec(j)
!
          if (   cmp_no_case(surf_grp_layer_ctl%c_tbl(j), 'in')         &
     &        ) csp_surf_grp%item_sf_grp(2,k) = 5
          if (   cmp_no_case(surf_grp_layer_ctl%c_tbl(j), 'out')        &
     &        ) csp_surf_grp%item_sf_grp(2,k) = 6
        end do
      end if
!
      end subroutine set_cubed_sph_surface_grp_item
!
!   --------------------------------------------------------------------
!
      end module set_cubed_sph_sf_group_ctl
