!
!      module set_cubed_sph_ele_group_ctl
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine set_cubed_sph_element_grp_num(elem_grp_name_ctl,     &
!!     &          if_CMB, if_ICB, if_EXT, csp_ele_grp)
!!      subroutine set_cubed_sph_element_grp_name(elem_grp_name_ctl,    &
!!     &          nlayer_ICB, nlayer_CMB, nlayer_EXT,                   &
!!     &          if_CMB, if_ICB, if_EXT, csp_ele_grp)
!!      subroutine set_cubed_sph_element_grp_item                       &
!!     &         (elem_grp_name_ctl, elem_grp_layer_ctl,                &
!!     &          nlayer_ICB, nlayer_CMB, nlayer_EXT,                   &
!!     &          if_CMB, if_ICB, if_EXT, csp_ele_grp)
!!        type(ctl_array_ci), intent(in) :: elem_grp_name_ctl
!!        type(ctl_array_int), intent(in) :: elem_grp_layer_ctl
!!        type(group_data), intent(inout) :: csp_ele_grp
!
      module set_cubed_sph_ele_group_ctl
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
      subroutine set_cubed_sph_element_grp_num(elem_grp_name_ctl,       &
     &          if_CMB, if_ICB, if_EXT, csp_ele_grp)
!
      use t_control_array_charaint
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: elem_grp_name_ctl
      integer(kind = kint), intent(inout) :: if_CMB, if_ICB, if_EXT
!
      type(group_data), intent(inout) :: csp_ele_grp
!
      integer(kind = kint) :: j
!
!   set element group table
!
      if_CMB = 1
      if_ICB = 1
      if_EXT = 1
      if (elem_grp_name_ctl%icou .gt. 0) then
        do j = 1, elem_grp_name_ctl%num
          if(cmp_no_case(elem_grp_name_ctl%c_tbl(j), 'outer_core')      &
     &       )  if_CMB = 0
          if(cmp_no_case(elem_grp_name_ctl%c_tbl(j), 'inner_core')      &
     &       )  if_ICB = 0
          if(cmp_no_case(elem_grp_name_ctl%c_tbl(j), 'external')        &
     &       )  if_EXT = 0
        end do
!
        csp_ele_grp%num_grp = elem_grp_name_ctl%num                     &
     &                       + if_CMB+if_ICB+if_EXT
      else
        csp_ele_grp%num_grp = if_CMB+if_ICB+if_EXT
      end if
!
      end subroutine set_cubed_sph_element_grp_num
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_element_grp_name(elem_grp_name_ctl,      &
     &          nlayer_ICB, nlayer_CMB, nlayer_EXT,                     &
     &          if_CMB, if_ICB, if_EXT, csp_ele_grp)
!
      use t_control_array_charaint
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: elem_grp_name_ctl
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: nlayer_EXT
      integer(kind = kint), intent(in) :: if_CMB, if_ICB, if_EXT
!
      type(group_data), intent(inout) :: csp_ele_grp
!
      integer(kind = kint) :: i, j, k
!
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        csp_ele_grp%nitem_grp(j) = max(nlayer_ICB,0)
        csp_ele_grp%grp_name(j) = 'inner_core'
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        csp_ele_grp%nitem_grp(j) = max((nlayer_CMB-nlayer_ICB),0)
        csp_ele_grp%grp_name(j) = 'outer_core'
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        csp_ele_grp%nitem_grp(j) = max((nlayer_EXT-nlayer_CMB),0)
        csp_ele_grp%grp_name(j) = 'external'
      end if
!
      if (elem_grp_name_ctl%icou .gt. 0) then
        k = 1 + if_CMB+if_ICB+if_EXT
        csp_ele_grp%nitem_grp(k) = elem_grp_name_ctl%ivec(1)
        csp_ele_grp%grp_name(k) = elem_grp_name_ctl%c_tbl(1)
        do j = 2, elem_grp_name_ctl%num
          k = j + if_CMB+if_ICB+if_EXT
          csp_ele_grp%nitem_grp(k) = elem_grp_name_ctl%ivec(j)          &
     &                              - elem_grp_name_ctl%ivec(j-1)
          csp_ele_grp%grp_name(k) = elem_grp_name_ctl%c_tbl(j)
        end do
      end if
!
      csp_ele_grp%istack_grp(0) = 0
      do j = 1, csp_ele_grp%num_grp
        csp_ele_grp%istack_grp(j) = csp_ele_grp%istack_grp(j-1)         &
     &                             + csp_ele_grp%nitem_grp(j)
      end do
      csp_ele_grp%num_item                                              &
     &      = csp_ele_grp%istack_grp(csp_ele_grp%num_grp)
!
      end subroutine set_cubed_sph_element_grp_name
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_element_grp_item                         &
     &         (elem_grp_name_ctl, elem_grp_layer_ctl,                  &
     &          nlayer_ICB, nlayer_CMB, nlayer_EXT,                     &
     &          if_CMB, if_ICB, if_EXT, csp_ele_grp)
!
      use t_control_array_charaint
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: elem_grp_name_ctl
      type(ctl_array_int), intent(in) :: elem_grp_layer_ctl
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: nlayer_EXT
      integer(kind = kint), intent(in) :: if_CMB, if_ICB, if_EXT
!
      type(group_data), intent(inout) :: csp_ele_grp
!
      integer(kind = kint) :: i, j, k
!
!
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        do k = 1, nlayer_ICB
          i = csp_ele_grp%istack_grp(j-1) + k
          csp_ele_grp%item_grp(i) = k - 1
        end do
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        do k = 1, (nlayer_CMB-nlayer_ICB)
          i = csp_ele_grp%istack_grp(j-1) + k
          csp_ele_grp%item_grp(i) = k + nlayer_ICB- 1
        end do
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        do k = 1, (nlayer_EXT-nlayer_CMB)
          i = csp_ele_grp%istack_grp(j-1) + k
          csp_ele_grp%item_grp(i) = k + nlayer_CMB - 1
        end do
      end if
!
      if (elem_grp_name_ctl%icou .gt. 0) then
        do j = 1, elem_grp_layer_ctl%num
          k = j + csp_ele_grp%istack_grp(if_CMB+if_ICB+if_EXT)
          csp_ele_grp%item_grp(k) = elem_grp_layer_ctl%ivec(j)
        end do
      end if
!
      end subroutine set_cubed_sph_element_grp_item
!
!   --------------------------------------------------------------------
!
      end module set_cubed_sph_ele_group_ctl
