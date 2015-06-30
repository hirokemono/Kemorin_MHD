!>@file  t_layering_ele_list.f90
!!       module t_layering_ele_list
!!
!!@author H. Matsui
!!@date   Programmed in Nov., 2009
!
!> @brief Structure of grouping of elements
!!
!!@verbatim
!!      subroutine alloc_layering_ele_list_type(layer_tbl)
!!      subroutine alloc_layer_items_type(layer_tbl)
!!      subroutine alloc_layering_volumes_type(layer_tbl)
!!
!!      subroutine dealloc_layering_ele_list_type(layer_tbl)
!!      subroutine dealloc_layering_volumes_type(layer_tbl)
!!
!!      subroutine check_layer_stack_type(my_rank, layer_tbl)
!!@endverbatim
!
      module t_layering_ele_list
!
      use m_precision
!
      implicit none
!
!   layering element table
!
!> Structure of grouping of elements
      type layering_tbl
!>      number of layers
        integer (kind = kint) :: n_layer_d
!>      total number of elements layer table
        integer (kind = kint) :: n_item_layer_d
!
!>      starting address for each layer
        integer (kind = kint), pointer :: layer_stack(:)
!>      element ID for layer table
        integer (kind = kint), pointer :: item_layer(:)
!
!>      starting address for each layer and SMP
        integer (kind = kint), pointer :: layer_stack_smp(:)
!>      minimum number of each layer with SMP
        integer(kind = kint) :: minlayer_4_smp
!>      maximum number of each layer with SMP
        integer(kind = kint) :: maxlayer_4_smp
!
!>      starting address for each layer for SMP
        integer (kind = kint), pointer :: istack_item_layer_d_smp(:)
!>      minimum number of each layer for SMP
        integer(kind = kint) :: min_item_layer_d_smp
!>      maximum number of each layer for SMP
        integer(kind = kint) :: max_item_layer_d_smp
!
!   volumes of layering area
!
!>      volumes for each layer
        real(kind = kreal), pointer :: volumes_layer(:)
!>       1 / volumes_layer
        real(kind = kreal), pointer :: a_vol_layer(:)
!
!>      volumes for all layer
        real(kind = kreal) :: vol_total_layer(1)
!>       1 / vol_total_layer
        real(kind = kreal) :: a_vol_total_layer(1)
      end type layering_tbl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_layering_ele_list_type(layer_tbl)
!
      use m_machine_parameter
!
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
      allocate (layer_tbl%layer_stack(0:layer_tbl%n_layer_d))
      allocate (layer_tbl%layer_stack_smp(0:layer_tbl%n_layer_d*np_smp))
      allocate (layer_tbl%istack_item_layer_d_smp(0:np_smp) )
!
      if (layer_tbl%n_layer_d .gt. 0) then
        layer_tbl%layer_stack      = 0
        layer_tbl%layer_stack_smp  = 0
        layer_tbl%istack_item_layer_d_smp = 0
      end if
!
      end subroutine alloc_layering_ele_list_type
!
! ----------------------------------------------------------------------
!
      subroutine alloc_layer_items_type(layer_tbl)
!
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
      allocate (layer_tbl%item_layer(layer_tbl%n_item_layer_d))
      if (layer_tbl%n_item_layer_d .gt. 0) layer_tbl%item_layer = 0
!
      end subroutine alloc_layer_items_type
!
! ----------------------------------------------------------------------
!
      subroutine alloc_layering_volumes_type(layer_tbl)
!
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
       allocate( layer_tbl%volumes_layer(layer_tbl%n_layer_d) )
       allocate( layer_tbl%a_vol_layer(layer_tbl%n_layer_d) )
!
       if(layer_tbl%n_layer_d .gt. 0) then
         layer_tbl%volumes_layer = 0.0d0
         layer_tbl%a_vol_layer  =  0.0d0
       end if
!
      end subroutine alloc_layering_volumes_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_layering_ele_list_type(layer_tbl)
!
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
      deallocate (layer_tbl%item_layer)
      deallocate (layer_tbl%layer_stack, layer_tbl%layer_stack_smp)
      deallocate (layer_tbl%istack_item_layer_d_smp )
!
      end subroutine dealloc_layering_ele_list_type
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_layering_volumes_type(layer_tbl)
!
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
       deallocate( layer_tbl%volumes_layer, layer_tbl%a_vol_layer)
!
      end subroutine dealloc_layering_volumes_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_layer_stack_type(my_rank, layer_tbl)
!
      integer(kind = kint), intent(in) :: my_rank
      type(layering_tbl), intent(in) :: layer_tbl
!
!
      integer(kind = kint) :: j
!
      write(50+my_rank,*) 'j, layer_tbl%layer_stack(j)'
      do j = 0, layer_tbl%n_layer_d
        write(50+my_rank,*) j, layer_tbl%layer_stack(j)
      end do
!
      end subroutine check_layer_stack_type
!
! ----------------------------------------------------------------------
!
      end module t_layering_ele_list
