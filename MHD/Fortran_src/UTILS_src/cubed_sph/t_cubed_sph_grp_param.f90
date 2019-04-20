!t_cubed_sph_grp_param.f90
!      module t_cubed_sph_grp_param
!
!      Written by H. Matsui on Apr., 2006
!
!!      subroutine set_cubed_sph_group_ctl(cubed_sph_c, csph_grp)
!!      subroutine set_empty_cubed_sph_group(csph_grp)
!!      subroutine dealloc_cubed_sph_group(csph_grp)
!!        type(control_data_cubed_sph), intent(in) :: cubed_sph_c
!!        type(cubed_sph_group, intent(inout) :: csph_grp
!
      module t_cubed_sph_grp_param
!
      use m_precision
      use t_group_data
!
      implicit none
!
      type cubed_sph_group
        integer(kind = kint) :: nlayer_ICB =    0
        integer(kind = kint) :: nlayer_CMB =    0
        integer(kind = kint) :: nlayer_EXT =    0
!
        integer(kind = kint) :: nr_icb
        integer(kind = kint) :: nr_cmb
!
        type(group_data) :: csp_nod_grp
        type(group_data) :: csp_ele_grp
        type(surface_group_data) :: csp_surf_grp
      end type cubed_sph_group
!
      private :: set_cubed_sph_node_grp_ctl
      private :: set_cubed_sph_element_grp_ctl
      private :: set_cubed_sph_surface_grp_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_cubed_sph_group_ctl(cubed_sph_c, csph_grp)
!
      use t_control_data_cubed_sph
!
      type(control_data_cubed_sph), intent(in) :: cubed_sph_c
      type(cubed_sph_group), intent(inout) :: csph_grp
!
!   set node group table
      call set_cubed_sph_node_grp_ctl                                   &
     &  (cubed_sph_c%node_grp_name_ctl, cubed_sph_c%node_grp_layer_ctl, &
     &   csph_grp%nlayer_ICB, csph_grp%nlayer_CMB, csph_grp%nlayer_EXT, &
     &   csph_grp%csp_nod_grp)
!
!   set element group table
      call set_cubed_sph_element_grp_ctl                                &
     &  (cubed_sph_c%elem_grp_name_ctl, cubed_sph_c%elem_grp_layer_ctl, &
     &   csph_grp%nlayer_ICB, csph_grp%nlayer_CMB, csph_grp%nlayer_EXT, &
     &   csph_grp%csp_ele_grp)
!
!   set surface group table
      call set_cubed_sph_surface_grp_ctl                                &
     &  (cubed_sph_c%surf_grp_name_ctl, cubed_sph_c%surf_grp_layer_ctl, &
     &   csph_grp%nlayer_ICB, csph_grp%nlayer_CMB, csph_grp%nlayer_EXT, &
     &   csph_grp%csp_surf_grp)
!
      end subroutine set_cubed_sph_group_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine set_empty_cubed_sph_group(csph_grp)
!
      type(cubed_sph_group, intent(inout) :: csph_grp
!
!   set node group table
!
      csph_grp%csp_nod_grp%num_grp =   0
      call alloc_group_num(csph_grp%csp_nod_grp)
      csph_grp%csp_nod_grp%istack_grp = 0
      csph_grp%csp_nod_grp%num_item = 0
      call alloc_group_item(csph_grp%csp_nod_grp)
!
!   set element group table
!
      csph_grp%csp_ele_grp%num_grp =   0
      call alloc_group_num(csph_grp%csp_ele_grp)
      csph_grp%csp_ele_grp%istack_grp = 0
      csph_grp%csp_ele_grp%num_item = 0
      call alloc_group_item(csph_grp%csp_ele_grp)
!
!   set surface group table
!
      csph_grp%csp_surf_grp%num_grp =   0
      call alloc_sf_group_num(csph_grp%csp_surf_grp)
      csph_grp%csp_surf_grp%istack_grp = 0
      csph_grp%csp_surf_grp%num_item = 0
      call alloc_sf_group_item(csph_grp%csp_surf_grp)
!
      end subroutine set_empty_cubed_sph_group
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_cubed_sph_group(csph_grp)
!
      type(cubed_sph_group, intent(inout) :: csph_grp
!
      call dealloc_group(csph_grp%csp_nod_grp)
      call dealloc_group(csph_grp%csp_ele_grp)
      call dealloc_sf_group(csph_grp%csp_surf_grp)
!
      end subroutine dealloc_cubed_sph_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_cubed_sph_node_grp_ctl                             &
     &         (node_grp_name_ctl, node_grp_layer_ctl,                  &
     &          nlayer_ICB, nlayer_CMB, nlayer_EXT, csp_nod_grp)
!
      use set_cubed_sph_nod_group_ctl
!
      type(ctl_array_ci), intent(in) :: node_grp_name_ctl
      type(ctl_array_int), intent(in) :: node_grp_layer_ctl
!
      integer(kind = kint), intent(inout) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(inout) :: nlayer_EXT
      type(group_data), intent(inout) :: csp_nod_grp
!
      integer(kind = kint) :: if_CMB, if_ICB, if_EXT
      integer(kind = kint) :: j, jst, jed
!
!
      call set_cubed_sph_node_grp_num                                   &
     &   (node_grp_name_ctl, node_grp_layer_ctl,                        &
     &    nlayer_ICB, nlayer_CMB, nlayer_EXT,                           &
     &    if_CMB, if_ICB, if_EXT, csp_nod_grp)
!
      call alloc_group_num(csp_nod_grp)
!
      call set_cubed_sph_node_grp_name(node_grp_name_ctl,               &
     &    if_CMB, if_ICB, if_EXT, csp_nod_grp)
!
      call alloc_group_item(csp_nod_grp)
!
      call set_cubed_sph_node_grp_item                                  &
     &   (node_grp_name_ctl, node_grp_layer_ctl,                        &
     &    nlayer_ICB, nlayer_CMB, nlayer_EXT,                           &
     &    if_CMB, if_ICB, if_EXT, csp_nod_grp)
!
      do j = 1, csp_nod_grp%num_grp
        jst = csp_nod_grp%istack_grp(j-1) + 1
        jed = csp_nod_grp%istack_grp(j)
        write(*,*) j, csp_nod_grp%istack_grp(j),                        &
     &      trim(csp_nod_grp%grp_name(j))
        write(*,*) csp_nod_grp%item_grp(jst:jed)
      end do
!
      end subroutine set_cubed_sph_node_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_element_grp_ctl                          &
     &         (elem_grp_name_ctl, elem_grp_layer_ctl,                  &
     &          nlayer_ICB, nlayer_CMB, nlayer_EXT, csp_ele_grp)
!
      use set_cubed_sph_ele_group_ctl
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: nlayer_EXT
      type(ctl_array_ci), intent(in) :: elem_grp_name_ctl
      type(ctl_array_int), intent(in) :: elem_grp_layer_ctl
!
      type(group_data), intent(inout) :: csp_ele_grp
!
      integer(kind = kint) :: if_CMB, if_ICB, if_EXT
      integer(kind = kint) :: j, jst, jed
!
!
      call set_cubed_sph_element_grp_num(elem_grp_name_ctl,             &
     &    if_CMB, if_ICB, if_EXT, csp_ele_grp)
!
      call alloc_group_num(csp_ele_grp)
!
      call set_cubed_sph_element_grp_name(elem_grp_name_ctl,            &
     &    nlayer_ICB, nlayer_CMB, nlayer_EXT,                           &
     &    if_CMB, if_ICB, if_EXT, csp_ele_grp)
!
      call alloc_group_item(csp_ele_grp)
!
      call set_cubed_sph_element_grp_item                               &
     &   (elem_grp_name_ctl, elem_grp_layer_ctl,                        &
     &    nlayer_ICB, nlayer_CMB, nlayer_EXT,                           &
     &    if_CMB, if_ICB, if_EXT, csp_ele_grp)
!
      do j = 1, csp_ele_grp%num_grp
        jst = csp_ele_grp%istack_grp(j-1) + 1
        jed = csp_ele_grp%istack_grp(j)
        write(*,*) j, csp_ele_grp%istack_grp(j),                        &
     &      trim(csp_ele_grp%grp_name(j))
        write(*,*) csp_ele_grp%item_grp(jst:jed)
      end do
!
      end subroutine set_cubed_sph_element_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine set_cubed_sph_surface_grp_ctl                          &
     &         (surf_grp_name_ctl, surf_grp_layer_ctl,                  &
     &          nlayer_ICB, nlayer_CMB, nlayer_EXT, csp_surf_grp)
!
      use set_cubed_sph_sf_group_ctl
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: nlayer_EXT
      type(ctl_array_ci), intent(in) :: surf_grp_name_ctl
      type(ctl_array_ci), intent(in) :: surf_grp_layer_ctl
!
      type(surface_group_data), intent(inout) :: csp_surf_grp
!
      integer(kind = kint) :: if_CMB, if_ICB, if_EXT
      integer(kind = kint) :: j, jst, jed
!
!   set surface group table
!
      call set_cubed_sph_surface_grp_num(surf_grp_name_ctl,             &
     &    if_CMB, if_ICB, if_EXT, csp_surf_grp)
!
      call alloc_sf_group_num(csp_surf_grp)
!
      call set_cubed_sph_surface_grp_name(surf_grp_name_ctl,            &
     &    if_CMB, if_ICB, if_EXT, csp_surf_grp)
!
      call alloc_sf_group_item(csp_surf_grp)
!
      call set_cubed_sph_surface_grp_item                               &
     &   (surf_grp_name_ctl, surf_grp_layer_ctl,                        &
     &    nlayer_ICB, nlayer_CMB, nlayer_EXT,                           &
     &    if_CMB, if_ICB, if_EXT, csp_surf_grp)
!
      do j = 1, csp_surf_grp%num_grp
        jst = csp_surf_grp%istack_grp(j-1) + 1
        jed = csp_surf_grp%istack_grp(j)
        write(*,*) j, csp_surf_grp%istack_grp(j),                       &
     &      trim(csp_surf_grp%grp_name(j))
        write(*,*) csp_surf_grp%item_sf_grp(1,jst:jed)
        write(*,*) csp_surf_grp%item_sf_grp(2,jst:jed)
      end do
!
      end subroutine set_cubed_sph_surface_grp_ctl
!
!   --------------------------------------------------------------------
!
      end module t_cubed_sph_grp_param
