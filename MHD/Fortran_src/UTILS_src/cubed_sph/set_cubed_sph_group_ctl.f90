!
!      module set_cubed_sph_group_ctl
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine set_cubed_sph_node_grp_ctl                           &
!!     &         (node_grp_name_ctl, node_grp_layer_ctl)
!!        type(ctl_array_ci), intent(in) :: node_grp_name_ctl
!!        type(ctl_array_int), intent(in) :: node_grp_layer_ctl
!!      subroutine set_cubed_sph_element_grp_ctl                        &
!!     &         (elem_grp_name_ctl, elem_grp_layer_ctl)
!!        type(ctl_array_ci), intent(in) :: elem_grp_name_ctl
!!        type(ctl_array_int), intent(in) :: elem_grp_layer_ctl
!!      subroutine set_cubed_sph_surface_grp_ctl                        &
!!     &         (surf_grp_name_ctl, surf_grp_layer_ctl)
!!        type(ctl_array_ci), intent(in) :: surf_grp_name_ctl
!!        type(ctl_array_ci), intent(in) :: surf_grp_layer_ctl
!
      module set_cubed_sph_group_ctl
!
      use m_precision
      use m_cubed_sph_grp_param
      use t_read_control_arrays
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_node_grp_ctl                             &
     &         (node_grp_name_ctl, node_grp_layer_ctl)
!
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: node_grp_name_ctl
      type(ctl_array_int), intent(in) :: node_grp_layer_ctl
!
      integer(kind = kint) :: j, k
      integer(kind = kint) :: if_CMB, if_ICB, if_EXT
!
!   set node group table
!
      if_CMB = 1
      if_ICB = 1
      if_EXT = 1
      if (node_grp_name_ctl%icou .gt. 0) then
!
        do j = 1, node_grp_name_ctl%num
          if(cmp_no_case(node_grp_name_ctl%c_tbl(j),'CMB')) then
            if_CMB = 0
            k = istack_nod_grp_layer_csp(j-1) + 1
            nlayer_CMB = node_grp_layer_ctl%ivec(k)
          end if
          if(cmp_no_case(node_grp_name_ctl%c_tbl(j),'ICB')) then
            if_ICB = 0
            k = istack_nod_grp_layer_csp(j-1) + 1
            nlayer_ICB = node_grp_layer_ctl%ivec(k)
          end if
          if(cmp_no_case(node_grp_name_ctl%c_tbl(j),'infinity')) then
            if_EXT = 0
            k = istack_nod_grp_layer_csp(j-1) + 1
            nlayer_EXT = node_grp_layer_ctl%ivec(k)
          end if
        end do
        num_node_grp_csp =  node_grp_name_ctl%num                       &
     &                     + if_CMB+if_ICB+if_EXT
        num_nod_layer_csp = node_grp_layer_ctl%num                      &
     &                     + if_CMB+if_ICB+if_EXT
      else
        num_node_grp_csp =  if_CMB+if_ICB+if_EXT
        num_nod_layer_csp = if_CMB+if_ICB+if_EXT
      end if
!
      call allocate_nod_grp_name_csp
      call allocate_nod_grp_layer_csp
!
      istack_nod_grp_layer_csp(0) = 0
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        istack_nod_grp_layer_csp(j) = istack_nod_grp_layer_csp(j-1) + 1
        nod_grp_name_csp(j) =        'ICB'
        id_nod_grp_layer_csp(j) = nlayer_ICB
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        istack_nod_grp_layer_csp(j) = istack_nod_grp_layer_csp(j-1) + 1
        nod_grp_name_csp(j) = 'CMB'
        id_nod_grp_layer_csp(j) = nlayer_CMB
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        istack_nod_grp_layer_csp(j) = istack_nod_grp_layer_csp(j-1) + 1
        nod_grp_name_csp(j) = 'Infinity'
        id_nod_grp_layer_csp(j) = nlayer_EXT
      end if
!
      if (node_grp_name_ctl%icou .gt. 0) then
        k = 1 + if_CMB+if_ICB+if_EXT
        istack_nod_grp_layer_csp(k) = istack_nod_grp_layer_csp(k-1)     &
     &                                 + node_grp_name_ctl%ivec(1)
        nod_grp_name_csp(k) = node_grp_name_ctl%c_tbl(1)
        do j = 2, node_grp_name_ctl%num
          k = j + if_CMB+if_ICB+if_EXT
          istack_nod_grp_layer_csp(k) = istack_nod_grp_layer_csp(k-1)   &
     &                                 + node_grp_name_ctl%ivec(j)      &
     &                                 - node_grp_name_ctl%ivec(j-1)
          nod_grp_name_csp(k) = node_grp_name_ctl%c_tbl(j)
        end do
        do j = 1, node_grp_layer_ctl%num
          k = j + istack_nod_grp_layer_csp(if_CMB+if_ICB+if_EXT)
          id_nod_grp_layer_csp(k) = node_grp_layer_ctl%ivec(j)
        end do
      end if
!
      end subroutine set_cubed_sph_node_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_element_grp_ctl                          &
     &         (elem_grp_name_ctl, elem_grp_layer_ctl)
!
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: elem_grp_name_ctl
      type(ctl_array_int), intent(in) :: elem_grp_layer_ctl
!
!
      integer(kind = kint) :: i, j, k
      integer(kind = kint) :: if_CMB, if_ICB, if_EXT
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
        num_ele_grp_csp = elem_grp_name_ctl%num + if_CMB+if_ICB+if_EXT
      else
        num_ele_grp_csp = if_CMB+if_ICB+if_EXT
      end if
!
      call allocate_ele_grp_name_csp
!
      call set_cubed_sph_element_grp_name(elem_grp_name_ctl,      &
     &          nlayer_ICB, nlayer_CMB, nlayer_EXT,                     &
     &          if_CMB, if_ICB, if_EXT, csp_ele_grp)
!
      call allocate_ele_grp_layer_csp
!
      call set_cubed_sph_element_grp_item                               &
     &   (elem_grp_name_ctl, elem_grp_layer_ctl,                        &
     &    nlayer_ICB, nlayer_CMB, nlayer_EXT,                           &
     &    if_CMB, if_ICB, if_EXT, csp_ele_grp)
!
      end subroutine set_cubed_sph_element_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_surface_grp_ctl                          &
     &         (surf_grp_name_ctl, surf_grp_layer_ctl, csp_surf_grp)
!
      use set_cubed_sph_sf_group_ctl
!
      type(ctl_array_ci), intent(in) :: surf_grp_name_ctl
      type(ctl_array_ci), intent(in) :: surf_grp_layer_ctl
!
      type(surface_group_data), intent(inout) :: csp_surf_grp
!
      integer(kind = kint) :: if_CMB, if_ICB, if_EXT
!
!   set surface group table
!
      call set_cubed_sph_surface_grp_num(surf_grp_name_ctl,             &
     &    if_CMB, if_ICB, if_EXT, csp_surf_grp)
!
      call allocate_surf_grp_name_csp
!
      call set_cubed_sph_surface_grp_name(surf_grp_name_ctl,            &
     &    if_CMB, if_ICB, if_EXT, csp_surf_grp)
!
      call allocate_surf_grp_layer_csp
!
      call set_cubed_sph_surface_grp_item                               &
     &   (surf_grp_name_ctl, surf_grp_layer_ctl,                        &
     &    nlayer_ICB, nlayer_CMB, nlayer_EXT,                           &
     &    if_CMB, if_ICB, if_EXT, csp_surf_grp)
!
      end subroutine set_cubed_sph_surface_grp_ctl
!
!   --------------------------------------------------------------------
!
      end module set_cubed_sph_group_ctl
