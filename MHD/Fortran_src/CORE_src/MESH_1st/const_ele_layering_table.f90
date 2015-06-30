!const_ele_layering_table.f90
!      module const_ele_layering_table
!
!      written by H. Matsui on Nov.,  2006
!      Modified by H. Matsui on Nov., 2009
!
!      subroutine const_layers_4_dynamic
!      subroutine deallocate_layering_ele_list
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
      subroutine const_layers_4_dynamic
!
      use set_layer_list_by_table
!
      integer(kind = kint) :: i
!
!
      if (iflag_layering_type .eq. 1) then
        if (iflag_debug .eq. 1)                                         &
     &    write(*,*) 'const_layer_list_by_start_end'
        call const_layer_list_by_start_end
!
      else if (iflag_layering_type .eq. 2) then
        if (iflag_debug .eq. 1)                                         &
     &    write(*,*) 'const_layer_list_by_mesh_file'
        call const_layer_list_by_mesh_file
      end if
!
      if (iflag_debug.eq.1) then
        write(*,*) 'igrp_stack_each_layer'
        write(*,'(8i16)') igrp_stack_each_layer(0:num_layer_grp)
        write(*,*) 'i, dynamic_layer_grp_name(i)', ntotal_layer_grp
        do i = 1, ntotal_layer_grp
          write(*,*) i, ': ', trim(dynamic_layer_grp_name(i))
        end do
      end if
!
      if (iflag_layering_type.eq.0 .or. iflag_layering_type.eq.2) then
        if (iflag_debug .eq. 1)                                         &
     &    write(*,*) 'const_layer_list_by_table'
        call const_layer_list_by_table
      end if
!
      end subroutine const_layers_4_dynamic
!
! ----------------------------------------------------------------------
!
      subroutine const_layer_list_by_table
!
      use m_element_group
      use m_layering_ele_list
      use set_layer_list_by_table
!
!
      call set_layerd_group_id(num_mat, mat_name)
!
      layer_tbl1%n_layer_d = num_layer_grp
      call alloc_layering_ele_list_type(layer_tbl1)
!
!
        if (iflag_debug .eq. 1)                                         &
     &    write(*,*) 'count_ele_layer_by_table'
      call count_ele_layer_by_table(num_mat, mat_istack,                &
     &    layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,              &
     &    layer_tbl1%layer_stack)
!
!      call check_layer_stack_type(my_rank, layer_tbl1)
!
      if (iflag_debug .eq. 1) write(*,*) 'alloc_layer_items_type'
      call alloc_layer_items_type(layer_tbl1)
      if (iflag_debug .eq. 1)  write(*,*) 'set_ele_layer_by_table'
      call set_ele_layer_by_table                                       &
     &   (num_mat, num_mat_bc, mat_istack, mat_item,                    &
     &    layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,              &
     &    layer_tbl1%layer_stack, layer_tbl1%item_layer)
!
      call deallocate_layering_ele_grp
!
!   SMP settings
!
      call count_ele_4_dynamic_smp                                      &
     &         (layer_tbl1%n_layer_d, layer_tbl1%layer_stack,           &
     &          layer_tbl1%maxlayer_4_smp, layer_tbl1%minlayer_4_smp,   &
     &          layer_tbl1%min_item_layer_d_smp,                        &
     &          layer_tbl1%max_item_layer_d_smp,                        &
     &          layer_tbl1%layer_stack_smp,                             &
     &          layer_tbl1%istack_item_layer_d_smp)
!
      end subroutine const_layer_list_by_table
!
! ----------------------------------------------------------------------
!
      subroutine const_layer_list_by_mesh_file
!
      use m_element_group
      use set_layer_list_by_table
!
      integer(kind = kint) :: ist_grp
!
!
      call count_layering_ele_grp_list(num_mat, mat_name, ist_grp)
!
      call allocate_layering_ele_grp
      call set_layering_ele_grp_list(num_mat, mat_name, ist_grp)
!
      end subroutine const_layer_list_by_mesh_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_layer_list_by_start_end
!
      use m_element_group
      use m_layering_ele_list
      use set_layer_list_by_table
      use set_layer_list_by_start_end
!
!
      call set_num_dynamic_layer_by_start(num_mat, mat_name,            &
     &    mat_istack, layer_tbl1%n_layer_d)
!
      call allocate_layer_ele_start(layer_tbl1%n_layer_d)
      call alloc_layering_ele_list_type(layer_tbl1)
!
      call set_start_ele_4_dynamic(num_mat, num_mat_bc,                 &
     &    mat_name, mat_istack, mat_item, layer_tbl1%n_layer_d)
!
      call count_ele_4_dynamic_by_start                                 &
     &   (layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,              &
     &    layer_tbl1%layer_stack)
!
      call alloc_layer_items_type(layer_tbl1)
      call set_ele_4_dynamic_by_start                                   &
     &   (layer_tbl1%n_layer_d, layer_tbl1%n_item_layer_d,              &
     &    layer_tbl1%layer_stack, layer_tbl1%item_layer)
!
      call deallocate_layer_ele_start
!
!   SMP settings
!
      call count_ele_4_dynamic_smp                                      &
     &         (layer_tbl1%n_layer_d, layer_tbl1%layer_stack,           &
     &          layer_tbl1%maxlayer_4_smp, layer_tbl1%minlayer_4_smp,   &
     &          layer_tbl1%min_item_layer_d_smp,                        &
     &          layer_tbl1%max_item_layer_d_smp,                        &
     &          layer_tbl1%layer_stack_smp,                             &
     &          layer_tbl1%istack_item_layer_d_smp)
!
      end subroutine const_layer_list_by_start_end
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_layering_ele_list
!
      use m_layering_ele_list
!
      call dealloc_layering_ele_list_type(layer_tbl1)
!
      end subroutine deallocate_layering_ele_list
!
! ----------------------------------------------------------------------
!
      end module const_ele_layering_table
