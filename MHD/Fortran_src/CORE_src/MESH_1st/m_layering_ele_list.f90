!
!      module m_layering_ele_list
!
!      written by Kemorin on Nov., 2009
!
!      subroutine allocate_layering_ele_list
!      subroutine allocate_layer_items
!      subroutine allocate_layering_volumes
!
!      subroutine deallocate_layering_ele_list
!      subroutine deallocate_layering_volumes
!
!      subroutine check_layer_stack(my_rank)
!
      module m_layering_ele_list
!
      use m_precision
!
      implicit none
!
!   layering element table
!
!>      number of layers
      integer (kind = kint) :: n_layer_d
!>      total number of elements layer table
      integer (kind = kint) :: n_item_layer_d
!
!>      starting address for each layer
      integer (kind = kint), allocatable :: layer_stack(:)
!>      element ID for layer table
      integer (kind = kint), allocatable :: item_layer(:)
!
!>      starting address for each layer with SMP
      integer (kind = kint), allocatable :: layer_stack_smp(:)
!>      maximum number of each layer with SMP
      integer(kind = kint) :: minlayer_4_smp
!>      minimum number of each layer with SMP
      integer(kind = kint) :: maxlayer_4_smp
!
!>      starting address for each layer for SMP
      integer (kind = kint), allocatable :: istack_item_layer_d_smp(:)
!>      minimum number of each layer for SMP
      integer(kind = kint) :: min_item_layer_d_smp
!>      maximum number of each layer for SMP
      integer(kind = kint) :: max_item_layer_d_smp
!
!   volumes of layering area
!
!>      volumes for each layer
      real(kind = kreal), allocatable :: volumes_layer(:)
!>       1 / volumes_layer
      real(kind = kreal), allocatable :: a_vol_layer(:)
!
!>      volumes for all layer
      real(kind = kreal) :: vol_total_layer(1)
!>       1 / vol_total_layer
      real(kind = kreal) :: a_vol_total_layer(1)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_layering_ele_list
!
      use m_machine_parameter
!
      allocate (layer_stack(0:n_layer_d))
      allocate (layer_stack_smp(0:n_layer_d*np_smp))
      allocate (istack_item_layer_d_smp(0:np_smp) )
!
      if (n_layer_d .gt. 0) then
        layer_stack      = 0
        layer_stack_smp  = 0
        istack_item_layer_d_smp = 0
      end if
!
      end subroutine allocate_layering_ele_list
!
! ----------------------------------------------------------------------
!
      subroutine allocate_layer_items
!
      allocate (item_layer(n_item_layer_d))
      if (n_item_layer_d .gt. 0) item_layer = 0
!
      end subroutine allocate_layer_items
!
! ----------------------------------------------------------------------
!
      subroutine allocate_layering_volumes
!
       allocate( volumes_layer(n_layer_d) )
       allocate( a_vol_layer(n_layer_d) )
!
       if(n_layer_d .gt. 0) then
         volumes_layer = 0.0d0
         a_vol_layer  =  0.0d0
       end if
!
      end subroutine allocate_layering_volumes
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_layering_ele_list
!
      deallocate (item_layer)
      deallocate (layer_stack, layer_stack_smp)
      deallocate (istack_item_layer_d_smp )
!
      end subroutine deallocate_layering_ele_list
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_layering_volumes
!
       deallocate( volumes_layer, a_vol_layer)
!
      end subroutine deallocate_layering_volumes
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_layer_stack(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: j
!
      write(50+my_rank,*) 'j, layer_stack(j)'
      do j = 0, n_layer_d
        write(50+my_rank,*) j, layer_stack(j)
      end do
!
      end subroutine check_layer_stack
!
! ----------------------------------------------------------------------
!
      end module m_layering_ele_list
