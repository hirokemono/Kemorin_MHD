!>@file   const_ele_layering_table.f90
!!@brief  module const_ele_layering_table
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2006
!!@n    modified in Nov., 2009
!!@n    modified in Aug., 2015
!
!>@brief Set element layering list for dynamic mscheme
!!
!!@verbatim
!!      subroutine const_layers_4_dynamic(ele_grp, layer_tbl)
!!        type(group_data), intent(in) :: ele_grp
!!        type(layering_tbl), intent(inout) :: layer_tbl
!!@endverbatim
!
      module const_ele_layering_table
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      private :: const_layer_list_by_mesh_file
      private :: const_layer_list_by_start_end
      private :: const_layer_list_by_table
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_layers_4_dynamic(ele_grp, layer_tbl)
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
     &    write(*,*) 'const_layer_list_by_start_end'
        call const_layer_list_by_start_end(ele_grp, layer_tbl)
!
      else if (iflag_layering_type .eq. 2) then
        if (iflag_debug .eq. 1)                                         &
     &    write(*,*) 'const_layer_list_by_mesh_file'
        call const_layer_list_by_mesh_file(ele_grp)
      end if
!
      if (iflag_debug.eq.1) call check_layering_ele_grp
!
      if (iflag_layering_type.eq.0 .or. iflag_layering_type.eq.2) then
        if (iflag_debug .eq. 1)                                         &
     &    write(*,*) 'const_layer_list_by_table'
        call const_layer_list_by_table(ele_grp, layer_tbl)
      end if
!
      end subroutine const_layers_4_dynamic
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_layer_list_by_table(ele_grp, layer_tbl)
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
      layer_tbl%e_grp%num_grp = num_layer_grp
      call alloc_layering_ele_list_type(layer_tbl)
!
!
        if (iflag_debug .eq. 1)                                         &
     &    write(*,*) 'count_ele_layer_by_table'
      call count_ele_layer_by_table(ele_grp%num_grp,                    &
     &    ele_grp%istack_grp, layer_tbl%e_grp%num_grp,                  &
     &    layer_tbl%e_grp%num_item, layer_tbl%e_grp%istack_grp)
!
!      call check_layer_stack_type(my_rank, layer_tbl1)
!
      if (iflag_debug .eq. 1) write(*,*) 'allocate_layer_items'
      call alloc_layer_items_type(layer_tbl)
      if (iflag_debug .eq. 1)  write(*,*) 'set_ele_layer_by_table'
      call set_ele_layer_by_table(ele_grp%num_grp, ele_grp%num_item,    &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,            &
     &    layer_tbl%e_grp%istack_grp, layer_tbl%e_grp%item_grp)
!
      call deallocate_layering_ele_grp
!
!   SMP settings
!
      call count_ele_4_dynamic_smp(layer_tbl%e_grp%num_grp,             &
     &    layer_tbl%e_grp%istack_grp, layer_tbl%e_grp%max_grp_smp,      &
     &    layer_tbl%minlayer_4_smp, layer_tbl%min_item_layer_d_smp,     &
     &    layer_tbl%max_item_layer_d_smp, layer_tbl%e_grp%istack_grp_smp, &
     &    layer_tbl%istack_item_layer_d_smp)
!
      end subroutine const_layer_list_by_table
!
! ----------------------------------------------------------------------
!
      subroutine const_layer_list_by_mesh_file(ele_grp)
!
      use t_group_data
      use set_layer_list_by_table
!
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint) :: ist_grp
!
!
      call count_layering_ele_grp_list                                  &
     &   (ele_grp%num_grp, ele_grp%grp_name, ist_grp)
!
      call allocate_layering_ele_grp
      call set_layering_ele_grp_list(ele_grp%num_grp, ele_grp%grp_name, &
     &    ist_grp)
!
      end subroutine const_layer_list_by_mesh_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_layer_list_by_start_end(ele_grp, layer_tbl)
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
     &    ele_grp%grp_name, ele_grp%istack_grp,                         &
     &    layer_tbl%e_grp%num_grp)
!
      call allocate_layer_ele_start(layer_tbl%e_grp%num_grp)
      call alloc_layering_ele_list_type(layer_tbl)
!
      call set_start_ele_4_dynamic(ele_grp%num_grp, ele_grp%num_item,   &
     &    ele_grp%grp_name, ele_grp%istack_grp, ele_grp%item_grp,       &
     &    layer_tbl%e_grp%num_grp)
!
      call count_ele_4_dynamic_by_start(layer_tbl%e_grp%num_grp,        &
     &    layer_tbl%e_grp%num_item, layer_tbl%e_grp%istack_grp)
!
      call alloc_layer_items_type(layer_tbl)
      call set_ele_4_dynamic_by_start(layer_tbl%e_grp%num_grp,          &
     &    layer_tbl%e_grp%num_item, layer_tbl%e_grp%istack_grp,         &
     &    layer_tbl%e_grp%item_grp)
!
      call deallocate_layer_ele_start
!
!   SMP settings
!
      call count_ele_4_dynamic_smp(layer_tbl%e_grp%num_grp,             &
     &    layer_tbl%e_grp%istack_grp, layer_tbl%e_grp%max_grp_smp,      &
     &    layer_tbl%minlayer_4_smp, layer_tbl%min_item_layer_d_smp,     &
     &    layer_tbl%max_item_layer_d_smp, layer_tbl%e_grp%istack_grp_smp, &
     &    layer_tbl%istack_item_layer_d_smp)
!
      end subroutine const_layer_list_by_start_end
!
! ----------------------------------------------------------------------
!
      end module const_ele_layering_table
