!!set_cubed_sph_nod_group_ctl.f90
!!      module set_cubed_sph_nod_group_ctl
!!
!!        programmed by H.Matsui on Apr., 2006
!!
!!      subroutine set_cubed_sph_node_grp_num                           &
!!     &         (node_grp_name_ctl, node_grp_layer_ctl,                &
!!     &          nlayer_ICB, nlayer_CMB, nlayer_EXT,                   &
!!     &          if_CMB, if_ICB, if_EXT, csp_nod_grp)
!!      subroutine set_cubed_sph_node_grp_name(node_grp_name_ctl,       &
!!     &          if_CMB, if_ICB, if_EXT, csp_nod_grp)
!!      subroutine set_cubed_sph_node_grp_item                          &
!!     &         (node_grp_name_ctl, node_grp_layer_ctl,                &
!!     &          nlayer_ICB, nlayer_CMB, nlayer_EXT,                   &
!!     &          if_CMB, if_ICB, if_EXT, csp_nod_grp)
!!        type(ctl_array_ci), intent(in) :: node_grp_name_ctl
!!        type(ctl_array_int), intent(in) :: node_grp_layer_ctl
!!        type(group_data), intent(inout) :: csp_nod_grp
!
      module set_cubed_sph_nod_group_ctl
!
      use m_precision
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
      subroutine set_cubed_sph_node_grp_num                             &
     &         (node_grp_name_ctl, node_grp_layer_ctl,                  &
     &          nlayer_ICB, nlayer_CMB, nlayer_EXT,                     &
     &          if_CMB, if_ICB, if_EXT, csp_nod_grp)
!
      use t_control_array_integer
      use t_control_array_charaint
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: node_grp_name_ctl
      type(ctl_array_int), intent(in) :: node_grp_layer_ctl
!
      integer(kind = kint), intent(inout) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(inout) :: nlayer_EXT
      integer(kind = kint), intent(inout) :: if_CMB, if_ICB, if_EXT
!
      type(group_data), intent(inout) :: csp_nod_grp
!
      integer(kind = kint) :: j, k
!
!   set node group table
!
      if_CMB = 1
      if_ICB = 1
      if_EXT = 1
      k = 0
      if(node_grp_name_ctl%icou .gt. 0) then
!
        do j = 1, node_grp_name_ctl%num
          if(cmp_no_case(node_grp_name_ctl%c_tbl(j),'CMB')) then
            if_CMB = 0
            k = k + 1
            nlayer_CMB = node_grp_layer_ctl%ivec(k)
          end if
          if(cmp_no_case(node_grp_name_ctl%c_tbl(j),'ICB')) then
            if_ICB = 0
            k = k + 1
            nlayer_ICB = node_grp_layer_ctl%ivec(k)
          end if
          if(cmp_no_case(node_grp_name_ctl%c_tbl(j),'infinity')) then
            if_EXT = 0
            k = k + 1
            nlayer_EXT = node_grp_layer_ctl%ivec(k)
          end if
        end do
        csp_nod_grp%num_grp =  node_grp_name_ctl%num                    &
     &                     + if_CMB+if_ICB+if_EXT
      else
        csp_nod_grp%num_grp =  if_CMB+if_ICB+if_EXT
      end if
!
      end subroutine set_cubed_sph_node_grp_num
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_node_grp_name(node_grp_name_ctl,         &
     &          if_CMB, if_ICB, if_EXT, csp_nod_grp)
!
      use t_control_array_charaint
!
      type(ctl_array_ci), intent(in) :: node_grp_name_ctl
      integer(kind = kint), intent(in) :: if_CMB, if_ICB, if_EXT
!
      type(group_data), intent(inout) :: csp_nod_grp
!
      integer(kind = kint) :: j, k
!
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        csp_nod_grp%nitem_grp(j) = 1
        csp_nod_grp%grp_name(j) = 'ICB'
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        csp_nod_grp%nitem_grp(j) = 1
        csp_nod_grp%grp_name(j) = 'CMB'
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        csp_nod_grp%nitem_grp(j) = 1
        csp_nod_grp%grp_name(j) = 'Infinity'
      end if
!
      if (node_grp_name_ctl%icou .gt. 0) then
        k = 1 + if_CMB+if_ICB+if_EXT
        csp_nod_grp%nitem_grp(k) = node_grp_name_ctl%ivec(1)
        csp_nod_grp%grp_name(k) = node_grp_name_ctl%c_tbl(1)
        do j = 2, node_grp_name_ctl%num
          k = j + if_CMB+if_ICB+if_EXT
          csp_nod_grp%nitem_grp(k) = node_grp_name_ctl%ivec(j)          &
     &                              - node_grp_name_ctl%ivec(j-1)
          csp_nod_grp%grp_name(k) = node_grp_name_ctl%c_tbl(j)
        end do
      end if
!
      csp_nod_grp%istack_grp(0) = 0
      do j = 1, csp_nod_grp%num_grp
        csp_nod_grp%istack_grp(j) = csp_nod_grp%istack_grp(j-1)         &
     &                             + csp_nod_grp%nitem_grp(j)
      end do
      csp_nod_grp%num_item                                              &
     &          = csp_nod_grp%istack_grp(csp_nod_grp%num_grp)
!
      end subroutine set_cubed_sph_node_grp_name
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_node_grp_item                            &
     &         (node_grp_name_ctl, node_grp_layer_ctl,                  &
     &          nlayer_ICB, nlayer_CMB, nlayer_EXT,                     &
     &          if_CMB, if_ICB, if_EXT, csp_nod_grp)
!
      use t_control_array_integer
      use t_control_array_charaint
!
      type(ctl_array_ci), intent(in) :: node_grp_name_ctl
      type(ctl_array_int), intent(in) :: node_grp_layer_ctl
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: nlayer_EXT
      integer(kind = kint), intent(in) :: if_CMB, if_ICB, if_EXT
!
      type(group_data), intent(inout) :: csp_nod_grp
!
      integer(kind = kint) :: j, k
!
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        csp_nod_grp%item_grp(j) = nlayer_ICB
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        csp_nod_grp%item_grp(j) = nlayer_CMB
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        csp_nod_grp%item_grp(j) = nlayer_EXT
      end if
!
      if (node_grp_name_ctl%icou .gt. 0) then
        do j = 1, node_grp_layer_ctl%num
          k = j + csp_nod_grp%istack_grp(if_CMB+if_ICB+if_EXT)
          csp_nod_grp%item_grp(k) = node_grp_layer_ctl%ivec(j)
        end do
      end if
!
      end subroutine set_cubed_sph_node_grp_item
!
!   --------------------------------------------------------------------
!
       end module set_cubed_sph_nod_group_ctl
