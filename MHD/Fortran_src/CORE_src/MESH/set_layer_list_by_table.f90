!set_layer_list_by_table.f90
!      module set_layer_list_by_table
!
!      written by Kemorin on Nov., 2009
!
!       subroutine allocate_layering_ele_grp
!       subroutine deallocate_layering_ele_grp
!
!      subroutine set_layerd_group_id(num_mat, mat_name)
!      subroutine count_ele_layer_by_table(num_mat, mat_istack,         &
!     &          n_layer_d, n_item_layer_d, layer_stack)
!      subroutine set_ele_layer_by_table(num_mat, num_mat_bc,           &
!     &          mat_istack, mat_item, n_layer_d, n_item_layer_d,       &
!     &          layer_stack, item_layer)
!
!      subroutine count_layering_ele_grp_list(num_mat, mat_name,        &
!     &          ist_grp)
!      subroutine set_layering_ele_grp_list(num_mat, mat_name, ist_grp)
!
!      subroutine count_ele_4_dynamic_smp(n_layer_d, layer_stack,       &
!     &          maxlayer_4_smp, minlayer_4_smp,                        &
!     &          min_item_layer_d_smp, max_item_layer_d_smp,            &
!     &          layer_stack_smp, istack_item_layer_d_smp)
!
      module set_layer_list_by_table
!
      use m_precision
!
      implicit none
!
!
      integer (kind=kint) :: iflag_layering_type = 0
!
!   start element group name
!
      integer (kind=kint) :: num_layering_grp
      integer (kind=kint) :: num_fluid_layering_grp
      character (len=kchara) :: start_layering_grp_name
      character (len=kchara) :: start_fluid_layering_grp_name
!
!   element list table
!
      integer (kind=kint) :: num_layer_grp = 0
      integer (kind=kint) :: ntotal_layer_grp = 0
      integer (kind=kint), allocatable :: igrp_stack_each_layer(:)
      integer (kind=kint), allocatable :: id_layer_ele_grp(:)
      character (len=kchara), allocatable :: dynamic_layer_grp_name(:)
!
      private :: id_layer_ele_grp
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine allocate_layering_ele_grp
!
       allocate(dynamic_layer_grp_name(ntotal_layer_grp))
       allocate(igrp_stack_each_layer(0:num_layer_grp))
       allocate(id_layer_ele_grp(ntotal_layer_grp))
!
       igrp_stack_each_layer = 0
       if(ntotal_layer_grp .gt. 0) id_layer_ele_grp = 0
!
       end subroutine allocate_layering_ele_grp
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_layering_ele_grp
!
       deallocate(dynamic_layer_grp_name)
       deallocate(igrp_stack_each_layer)
       deallocate(id_layer_ele_grp)
!
       end subroutine deallocate_layering_ele_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_layerd_group_id(num_mat, mat_name)
!
      integer (kind=kint), intent(in) :: num_mat
      character (len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint) :: i, j
!
!
      do j = 1, ntotal_layer_grp
        do i = 1, num_mat
          if (mat_name(i) .eq. dynamic_layer_grp_name(j) ) then
            id_layer_ele_grp(j) = i
            exit
          end if
        end do
      end do
!
      end subroutine set_layerd_group_id
!
! ----------------------------------------------------------------------
!
      subroutine count_ele_layer_by_table(num_mat, mat_istack,          &
     &          n_layer_d, n_item_layer_d, layer_stack)
!
      integer (kind=kint), intent(in) :: num_mat, n_layer_d
      integer (kind=kint), intent(in) :: mat_istack(0:num_mat)
!
      integer (kind=kint), intent(inout) :: n_item_layer_d
      integer (kind=kint), intent(inout) :: layer_stack(0:n_layer_d)
!
      integer(kind = kint) :: i, j, jnum, jst, jed
!
!
      layer_stack(0) = 0
      do j = 1, num_layer_grp
        jst = igrp_stack_each_layer(j-1) + 1
        jed = igrp_stack_each_layer(j)
        layer_stack(j) = layer_stack(j-1)
        do jnum = jst, jed
          i = id_layer_ele_grp(jnum)
          layer_stack(j) = layer_stack(j)                               &
     &                    + mat_istack(i) - mat_istack(i-1)
        end do
      end do
      n_item_layer_d = layer_stack(num_layer_grp)
!
      end subroutine count_ele_layer_by_table
!
! ----------------------------------------------------------------------
!
      subroutine set_ele_layer_by_table(num_mat, num_mat_bc,            &
     &          mat_istack, mat_item, n_layer_d, n_item_layer_d,        &
     &          layer_stack, item_layer)
!
      integer (kind=kint), intent(in) :: num_mat, num_mat_bc
      integer (kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer (kind=kint), intent(in) :: mat_item(num_mat_bc)
!
      integer (kind=kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind=kint), intent(in) :: layer_stack(0:n_layer_d)
!
      integer (kind=kint), intent(inout) :: item_layer(n_item_layer_d)
!
!
      integer(kind = kint) :: i, j, jst, jed, jnum, ist, ied
      integer(kind = kint) :: inum, icou
!
!
      do j = 1, num_layer_grp
        jst = igrp_stack_each_layer(j-1) + 1
        jed = igrp_stack_each_layer(j)
        icou = layer_stack(j-1)
        do jnum = jst, jed
          i = id_layer_ele_grp(jnum)
          ist = mat_istack(i-1) + 1
          ied = mat_istack(i)
          do inum = ist, ied
            icou = icou + 1
            item_layer(icou) = mat_item(inum)
          end do
        end do
      end do
!
      end subroutine set_ele_layer_by_table
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_layering_ele_grp_list(num_mat, mat_name,         &
     &          ist_grp)
!
      integer (kind=kint), intent(in) :: num_mat
      character (len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(inout) :: ist_grp
      integer(kind = kint) :: i
!
!
      do i = 1, num_mat
        if ( mat_name(i) .eq. start_layering_grp_name) then
          ist_grp = i
          exit
        end if
        ist_grp = i
      end do
!
      if ( (ist_grp+num_layering_grp-1) .gt. num_mat) then
        ntotal_layer_grp = num_mat - ist_grp + 1
      else
        ntotal_layer_grp = num_layering_grp
      end if
      num_layer_grp = ntotal_layer_grp
!
      end subroutine count_layering_ele_grp_list
!
! ----------------------------------------------------------------------
!
      subroutine set_layering_ele_grp_list(num_mat, mat_name, ist_grp)
!
      use m_machine_parameter
!
      integer (kind=kint), intent(in) :: num_mat
      character (len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: ist_grp
      integer(kind = kint) :: i
!
!
      igrp_stack_each_layer(0) = 0
      do i = 1, num_layer_grp
        igrp_stack_each_layer(i) = i
        dynamic_layer_grp_name(i) = mat_name(ist_grp+i-1)
      end do
!
      end subroutine set_layering_ele_grp_list
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_ele_4_dynamic_smp(n_layer_d, layer_stack,        &
     &          maxlayer_4_smp, minlayer_4_smp,                         &
     &          min_item_layer_d_smp, max_item_layer_d_smp,             &
     &          layer_stack_smp, istack_item_layer_d_smp)
!
      use m_constants
      use m_machine_parameter
      use cal_minmax_and_stacks
!
      integer (kind=kint), intent(in) :: n_layer_d
      integer (kind=kint), intent(in) :: layer_stack(0:n_layer_d)
!
      integer (kind=kint), intent(inout) :: maxlayer_4_smp
      integer (kind=kint), intent(inout) :: minlayer_4_smp
      integer (kind=kint), intent(inout) :: max_item_layer_d_smp
      integer (kind=kint), intent(inout) :: min_item_layer_d_smp
      integer (kind=kint), intent(inout)                                &
     &       :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind=kint), intent(inout)                                &
     &       :: istack_item_layer_d_smp(0:np_smp)
!
      integer(kind = kint) :: j, id_smp, ist, ied, imax, num
!
!
      maxlayer_4_smp = 0
      minlayer_4_smp = layer_stack(n_layer_d)
      do j = 1, n_layer_d
        id_smp = np_smp*(j-1)
        ist = layer_stack(j-1) + 1
        ied = layer_stack(j  )
        call count_number_4_smp(np_smp, ist, ied,                       &
     &       layer_stack_smp(id_smp), imax )
        num = layer_stack_smp(np_smp*j)
        maxlayer_4_smp = max(maxlayer_4_smp,imax)
        minlayer_4_smp = min(minlayer_4_smp,num)
      end do
!
      call count_number_4_smp(np_smp, ione, n_layer_d,                  &
     &    istack_item_layer_d_smp, max_item_layer_d_smp)
!
      min_item_layer_d_smp = istack_item_layer_d_smp(np_smp)            &
     &                      - istack_item_layer_d_smp(np_smp-1)
!
      end subroutine count_ele_4_dynamic_smp
!
! ----------------------------------------------------------------------
!
      end module set_layer_list_by_table
