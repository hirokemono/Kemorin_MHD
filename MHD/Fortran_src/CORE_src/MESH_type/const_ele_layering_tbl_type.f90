!const_ele_layering_tbl_type.f90
!      module const_ele_layering_tbl_type
!
!      written by H. Matsui on Nov.,  2006
!      Modified by H. Matsui on Nov., 2009
!
      module const_ele_layering_tbl_type
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      private :: const_layer_list_by_mesh_type
      private :: const_layer_list_by_st_ed_type
      private :: const_layer_list_by_table_type
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_layers_4_dynamic_type(ele_grp, layer_tbl)
!
      use t_group_data
      use t_layering_ele_list
      use set_layer_list_by_table
!
      type(group_data), intent(in) :: ele_grp
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
      if (iflag_layering_type .eq. 1) then
        if (iflag_debug .eq. 1)                                         &
     &    write(*,*) 'const_layer_list_by_st_ed_type'
        call const_layer_list_by_st_ed_type(ele_grp, layer_tbl)
!
      else if (iflag_layering_type .eq. 2) then
        if (iflag_debug .eq. 1)                                         &
     &    write(*,*) 'const_layer_list_by_mesh_type'
        call const_layer_list_by_mesh_type(ele_grp)
      end if
!
      if (iflag_layering_type.eq.0 .or. iflag_layering_type.eq.2) then
        if (iflag_debug .eq. 1)                                         &
     &    write(*,*) 'const_layer_list_by_table_type'
        call const_layer_list_by_table_type(ele_grp, layer_tbl)
      end if
!
      end subroutine const_layers_4_dynamic_type
!
! ----------------------------------------------------------------------
!
      subroutine const_layer_list_by_table_type(ele_grp, layer_tbl)
!
      use t_group_data
      use t_layering_ele_list
      use set_layer_list_by_table
!
      type(group_data), intent(in) :: ele_grp
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
      call set_layerd_group_id(ele_grp%num_grp, ele_grp%grp_name)
!
      layer_tbl%n_layer_d = num_layer_grp
      call alloc_layering_ele_list_type(layer_tbl)
!
!
        if (iflag_debug .eq. 1)                                         &
     &    write(*,*) 'count_ele_layer_by_table'
      call count_ele_layer_by_table(ele_grp%num_grp,                    &
     &    ele_grp%istack_grp, layer_tbl%n_layer_d,                      &
     &   layer_tbl%n_item_layer_d,  layer_tbl%layer_stack)
!
!      call check_layer_stack_type(my_rank, layer_tbl1)
!
      if (iflag_debug .eq. 1) write(*,*) 'allocate_layer_items'
      call alloc_layer_items_type(layer_tbl)
      if (iflag_debug .eq. 1)  write(*,*) 'set_ele_layer_by_table'
      call set_ele_layer_by_table(ele_grp%num_grp, ele_grp%num_item,    &
     &    ele_grp%istack_grp, ele_grp%item_grp, layer_tbl%n_layer_d,    &
     &    layer_tbl%n_item_layer_d, layer_tbl%layer_stack,              &
     &    layer_tbl%item_layer)
!
      call deallocate_layering_ele_grp
!
!   SMP settings
!
      call count_ele_4_dynamic_smp(layer_tbl%n_layer_d,                 &
     &    layer_tbl%layer_stack, layer_tbl%maxlayer_4_smp,              &
     &    layer_tbl%minlayer_4_smp, layer_tbl%min_item_layer_d_smp,     &
     &    layer_tbl%max_item_layer_d_smp, layer_tbl%layer_stack_smp,    &
     &    layer_tbl%istack_item_layer_d_smp)
!
      end subroutine const_layer_list_by_table_type
!
! ----------------------------------------------------------------------
!
      subroutine const_layer_list_by_mesh_type(ele_grp)
!
      use t_group_data
      use set_layer_list_by_table
!
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint) :: ist_grp
!
!
      call count_layering_ele_grp_list(ele_grp%num_grp,                 &
     &    ele_grp%grp_name, ist_grp)
!
      call allocate_layering_ele_grp
      call set_layering_ele_grp_list(ele_grp%num_grp, ele_grp%grp_name, &
     &    ist_grp)
!
      end subroutine const_layer_list_by_mesh_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_layer_list_by_st_ed_type(ele_grp, layer_tbl)
!
      use t_group_data
      use t_layering_ele_list
      use set_layer_list_by_table
      use set_layer_list_by_start_end
!
      type(group_data), intent(in) :: ele_grp
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
      call set_num_dynamic_layer_by_start(ele_grp%num_grp,              &
     &    ele_grp%grp_name, ele_grp%istack_grp, layer_tbl%n_layer_d)
!
      call allocate_layer_ele_start(layer_tbl%n_layer_d)
      call alloc_layering_ele_list_type(layer_tbl)
!
      call set_start_ele_4_dynamic(ele_grp%num_grp, ele_grp%num_item,   &
     &    ele_grp%grp_name, ele_grp%istack_grp, ele_grp%item_grp,       &
     &    layer_tbl%n_layer_d)
!
      call count_ele_4_dynamic_by_start(layer_tbl%n_layer_d,            &
     &    layer_tbl%n_item_layer_d, layer_tbl%layer_stack)
!
      call alloc_layer_items_type(layer_tbl)
      call set_ele_4_dynamic_by_start(layer_tbl%n_layer_d,              &
     &    layer_tbl%n_item_layer_d, layer_tbl%layer_stack,              &
     &    layer_tbl%item_layer)
!
      call deallocate_layer_ele_start
!
!   SMP settings
!
      call count_ele_4_dynamic_smp(layer_tbl%n_layer_d,                 &
     &    layer_tbl%layer_stack, layer_tbl%maxlayer_4_smp,              &
     &    layer_tbl%minlayer_4_smp, layer_tbl%min_item_layer_d_smp,     &
     &    layer_tbl%max_item_layer_d_smp, layer_tbl%layer_stack_smp,    &
     &    layer_tbl%istack_item_layer_d_smp)
!
      end subroutine const_layer_list_by_st_ed_type
!
! ----------------------------------------------------------------------
!
      end module const_ele_layering_tbl_type
