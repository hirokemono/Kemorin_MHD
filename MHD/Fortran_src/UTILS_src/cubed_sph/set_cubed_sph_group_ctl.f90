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
      istack_ele_grp_layer_csp(0) = 0
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        istack_ele_grp_layer_csp(j) = istack_ele_grp_layer_csp(j-1)     &
     &                               + max(nlayer_ICB,0)
        ele_grp_name_csp(j) =        'inner_core'
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        istack_ele_grp_layer_csp(j) = istack_ele_grp_layer_csp(j-1)     &
     &                               + max((nlayer_CMB-nlayer_ICB),0)
        ele_grp_name_csp(j) = 'outer_core'
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        istack_ele_grp_layer_csp(j) = istack_ele_grp_layer_csp(j-1)     &
     &                               + max((nlayer_EXT-nlayer_CMB),0)
        ele_grp_name_csp(j) = 'external'
      end if
!
      num_ele_layer_csp = elem_grp_layer_ctl%num                        &
     &                    + istack_ele_grp_layer_csp(j)
      call allocate_ele_grp_layer_csp
!
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        do k = 1, nlayer_ICB
          i = istack_ele_grp_layer_csp(j-1) + k
          id_ele_grp_layer_csp(i) = k - 1
        end do
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        do k = 1, (nlayer_CMB-nlayer_ICB)
          i = istack_ele_grp_layer_csp(j-1) + k
          id_ele_grp_layer_csp(i) = k + nlayer_ICB- 1
        end do
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        do k = 1, (nlayer_EXT-nlayer_CMB)
          i = istack_ele_grp_layer_csp(j-1) + k
          id_ele_grp_layer_csp(i) = k + nlayer_CMB - 1
        end do
      end if
!
!
      if (elem_grp_name_ctl%icou .gt. 0) then
        k = 1 + if_CMB+if_ICB+if_EXT
        istack_ele_grp_layer_csp(k) = istack_ele_grp_layer_csp(k-1)     &
     &                                 + elem_grp_name_ctl%ivec(1)
        ele_grp_name_csp(k) = elem_grp_name_ctl%c_tbl(1)
        do j = 2, elem_grp_name_ctl%num
          k = j + if_CMB+if_ICB+if_EXT
          istack_ele_grp_layer_csp(k) = istack_ele_grp_layer_csp(k-1)   &
     &                                 + elem_grp_name_ctl%ivec(j)      &
     &                                 - elem_grp_name_ctl%ivec(j-1)
          ele_grp_name_csp(k) = elem_grp_name_ctl%c_tbl(j)
        end do
        do j = 1, elem_grp_layer_ctl%num
          k = j + istack_ele_grp_layer_csp(if_CMB+if_ICB+if_EXT)
          id_ele_grp_layer_csp(k) = elem_grp_layer_ctl%ivec(j)
        end do
      end if
!
      end subroutine set_cubed_sph_element_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_surface_grp_ctl                          &
     &         (surf_grp_name_ctl, surf_grp_layer_ctl)
!
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: surf_grp_name_ctl
      type(ctl_array_ci), intent(in) :: surf_grp_layer_ctl
!
      integer(kind = kint) :: j, k
      integer(kind = kint) :: if_CMB, if_ICB, if_EXT
!
!   set surface group table
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
        num_surf_grp_csp                                                &
     &                  = surf_grp_name_ctl%num + if_CMB+if_ICB+if_EXT
        num_surf_layer_csp                                              &
     &                  = surf_grp_layer_ctl%num + if_CMB+if_ICB+if_EXT
      else
        num_surf_grp_csp =   if_CMB+if_ICB+if_EXT
        num_surf_layer_csp = if_CMB+if_ICB+if_EXT
      end if
!
      call allocate_surf_grp_name_csp
      call allocate_surf_grp_layer_csp
!
      istack_surf_grp_layer_csp(0) = 0
      j = 0
      if(if_ICB .gt. 0) then
        j = j + 1
        istack_surf_grp_layer_csp(j) = istack_surf_grp_layer_csp(j-1)+1
        surf_grp_name_csp(j) =        'ICB_surf'
        id_surf_grp_layer_csp(1,j) = nlayer_ICB
        id_surf_grp_layer_csp(2,j) = 5
      end if
      if(if_CMB .gt. 0) then
        j = j + 1
        istack_surf_grp_layer_csp(j) = istack_surf_grp_layer_csp(j-1)+1
        surf_grp_name_csp(j) = 'CMB_surf'
        id_surf_grp_layer_csp(1,j) = nlayer_CMB - 1
        id_surf_grp_layer_csp(2,j) = 6
      end if
      if(if_EXT .gt. 0) then
        j = j + 1
        istack_surf_grp_layer_csp(j) = istack_surf_grp_layer_csp(j-1)+1
        surf_grp_name_csp(j) = 'Infinity_surf'
        id_surf_grp_layer_csp(1,j) = nlayer_EXT - 1
        id_surf_grp_layer_csp(2,j) = 6
      end if
!
      if (surf_grp_name_ctl%icou .gt. 0) then
        k = 1 + if_CMB+if_ICB+if_EXT
        istack_surf_grp_layer_csp(k) = istack_surf_grp_layer_csp(k-1)   &
     &                                 + surf_grp_name_ctl%ivec(1)
        surf_grp_name_csp(k) = surf_grp_name_ctl%c_tbl(1)
        do j = 2, surf_grp_name_ctl%num
          k = j + if_CMB+if_ICB+if_EXT
          istack_surf_grp_layer_csp(k) = istack_surf_grp_layer_csp(k-1) &
     &                                 + surf_grp_name_ctl%ivec(j)      &
     &                                 - surf_grp_name_ctl%ivec(j-1)
          surf_grp_name_csp(k) = surf_grp_name_ctl%c_tbl(j)
        end do
        do j = 1, surf_grp_layer_ctl%num
          k = j + istack_surf_grp_layer_csp(if_CMB+if_ICB+if_EXT)
          id_surf_grp_layer_csp(1,k) = surf_grp_layer_ctl%ivec(j)
!
          if (   cmp_no_case(surf_grp_layer_ctl%c_tbl(j), 'in')         &
     &        ) id_surf_grp_layer_csp(2,k) = 5
          if (   cmp_no_case(surf_grp_layer_ctl%c_tbl(j), 'out')        &
     &        ) id_surf_grp_layer_csp(2,k) = 6
        end do
      end if
!
      end subroutine set_cubed_sph_surface_grp_ctl
!
!   --------------------------------------------------------------------
!
      end module set_cubed_sph_group_ctl
