!set_layer_list_by_start_end.f90
!      module set_layer_list_by_start_end
!
!      written by Kemorin on Nov., 2009
!
!      subroutine allocate_layer_ele_start(n_layer_d)
!      subroutine deallocate_layer_ele_start
!
!      subroutine set_num_dynamic_layer_by_start(num_mat, mat_name,     &
!     &          mat_istack, n_layer_d)
!      subroutine set_start_ele_4_dynamic(num_mat, num_mat_bc,          &
!     &          mat_name, mat_istack, mat_item, n_layer_d)
!
!      subroutine count_ele_4_dynamic_by_start(n_layer_d,               &
!     &          n_item_layer_d, layer_stack)
!      subroutine set_ele_4_dynamic_by_start(n_layer_d,                 &
!     &          n_item_layer_d, layer_stack, item_layer)
!
!      subroutine check_layer_start(id_rank, n_layer_d)
!
      module set_layer_list_by_start_end
!
      use m_precision
!
      implicit none
!
      character(len=kchara), parameter :: st_egrp_name = 'layer_start'
      character(len=kchara), parameter :: ed_egrp_name = 'layer_end'
!
      integer (kind = kint), allocatable :: item_layer_start(:)
      integer (kind = kint), allocatable :: item_layer_end(:)
!
      private :: st_egrp_name, ed_egrp_name
      private :: item_layer_start, item_layer_end
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_layer_ele_start(n_layer_d)
!
      integer (kind=kint), intent(in) :: n_layer_d
!
!
      allocate (item_layer_start(n_layer_d))
      allocate (item_layer_end(n_layer_d))
!
      item_layer_start = 0
      item_layer_end   = 0
!
      end subroutine allocate_layer_ele_start
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_layer_ele_start
!
!
      deallocate (item_layer_start, item_layer_end)
!
      end subroutine deallocate_layer_ele_start
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_num_dynamic_layer_by_start(num_mat, mat_name,      &
     &          mat_istack, n_layer_d)
!
      integer (kind=kint), intent(in) :: num_mat
      integer (kind=kint), intent(in) :: mat_istack(0:num_mat)
      character (len=kchara), intent(in) :: mat_name(num_mat)
!
      integer (kind=kint), intent(inout) :: n_layer_d
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_mat
        if ( mat_name(i) .eq. st_egrp_name) then
          n_layer_d = mat_istack(i) - mat_istack(i-1)
          exit
        end if
      end do
!
      end subroutine set_num_dynamic_layer_by_start
!
! ----------------------------------------------------------------------
!
      subroutine set_start_ele_4_dynamic(num_mat, num_mat_bc,           &
     &          mat_name, mat_istack, mat_item, n_layer_d)
!
      integer (kind=kint), intent(in) :: num_mat, num_mat_bc
      integer (kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer (kind=kint), intent(in) :: mat_item(num_mat_bc)
      character (len=kchara), intent(in) :: mat_name(num_mat)
!
      integer (kind=kint), intent(in) :: n_layer_d
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, num_mat
        if ( mat_name(i) .eq. st_egrp_name) then
          do j = 1, n_layer_d
            item_layer_start(j) = mat_item(j+mat_istack(i-1))
          end do
        end if
        if ( mat_name(i) .eq. ed_egrp_name) then
          do j = 1, n_layer_d
            item_layer_end(j) = mat_item(j+mat_istack(i-1))
          end do
        end if
      end do
!
      end subroutine set_start_ele_4_dynamic
!
! ----------------------------------------------------------------------
!
      subroutine count_ele_4_dynamic_by_start(n_layer_d,                &
     &          n_item_layer_d, layer_stack)
!
      integer (kind=kint), intent(in) :: n_layer_d
      integer (kind=kint), intent(inout) :: n_item_layer_d
      integer (kind=kint), intent(inout) :: layer_stack(0:n_layer_d)
!
      integer(kind = kint) :: j, num
!
!
      do j = 1, n_layer_d
        num = item_layer_end(j) - item_layer_start(j) + 1
        layer_stack(j) = layer_stack(j-1) + num
      end do
      n_item_layer_d = layer_stack(n_layer_d)
!
      end subroutine count_ele_4_dynamic_by_start
!
! ----------------------------------------------------------------------
!
      subroutine set_ele_4_dynamic_by_start(n_layer_d,                  &
     &          n_item_layer_d, layer_stack, item_layer)
!
      integer (kind=kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind=kint), intent(in) :: layer_stack(0:n_layer_d)
!
      integer (kind=kint), intent(inout) :: item_layer(n_item_layer_d)
!
      integer(kind = kint) :: j, i, num
!
!
      do j = 1, n_layer_d
        num = layer_stack(j) - layer_stack(j-1)
        do i = 1, num
          item_layer(i) = item_layer_start(j) + i - 1
        end do
      end do
!
      end subroutine set_ele_4_dynamic_by_start
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_layer_start(id_rank, n_layer_d)
!
      integer, intent(in) :: id_rank
      integer (kind=kint), intent(in) :: n_layer_d
!
      integer(kind = kint) :: j
!
!
      write(50+id_rank,*) 'j, item_layer_start(j), item_layer_end(j)'
      do j = 1, n_layer_d
        write(50+id_rank,*) j, item_layer_start(j), item_layer_end(j)
      end do
!
      end subroutine check_layer_start
!
! ----------------------------------------------------------------------
!
      end module set_layer_list_by_start_end
