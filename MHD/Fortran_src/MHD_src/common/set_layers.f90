!
!     module set_layers
!.......................................................................
!
!       subroutines for start and end element number of each layer
!       and set node lists for each layer
!
!        programmed by H.Matsui on ???, 2002
!        modified by H.Matsui on March, 2006
!        modified by H.Matsui on Feb., 2008
!        modified by H.Matsui on Dec., 2008
!
!!      subroutine count_node_4_layer(numnod, internal_node,            &
!!     &          iele_start, iele_end, numele, nnod_4_ele, ie,         &
!!     &          mat_node_flag, numnod_field, internal_node_field)
!!      subroutine count_ele_4_layer(numele, num_field, num_layer,      &
!!     &          layer_name, num_mat, mat_istack, mat_name)
!!
!!      subroutine set_node_4_layer                                     &
!!     &         (numnod, numnod_field, iele_start, iele_end,           &
!!     &          numele, nnod_4_ele, ie, mat_node_flag, inod_field)
!
!
      module set_layers
!
      use m_precision
!
      implicit none
!
! ---------------------------------------------------------------------
!
      contains
!
! ---------------------------------------------------------------------
!
      subroutine count_node_4_layer(numnod, internal_node,              &
     &          iele_start, iele_end, numele, nnod_4_ele, ie,           &
     &          mat_node_flag, numnod_field, internal_node_field)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: iele_start, iele_end
!
      integer(kind = kint), intent(inout) :: numnod_field
      integer(kind = kint), intent(inout) :: internal_node_field
      integer (kind=kint), intent(inout) :: mat_node_flag(numnod)
!
      integer (kind = kint) :: inod, iele, j1
!
!
      mat_node_flag = 0
      numnod_field = 0
      internal_node_field = 0
!
      do iele = iele_start, iele_end
        do j1 = 1, nnod_4_ele
          inod = ie(iele,j1)
          mat_node_flag(inod) = 1
        end do
      end do
!
      do inod = 1, internal_node
        if ( mat_node_flag(inod) .eq. 1) then
          internal_node_field = internal_node_field +1
        end if
      end do
!
      do inod = 1, numnod
        if ( mat_node_flag(inod) .eq. 1) then
          numnod_field = numnod_field +1
        end if
      end do
!
      end subroutine count_node_4_layer
!
! ---------------------------------------------------------------------
!
      subroutine count_ele_4_layer(numele, num_field, num_layer,        &
     &          layer_name, num_mat, mat_istack, mat_name)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: num_layer
      character(len=kchara), intent(in) :: layer_name(num_layer)
!
      integer (kind = kint), intent(in) :: num_mat
      integer (kind = kint), intent(in) :: mat_istack(0:num_mat)
      character (len=kchara), intent(in) :: mat_name(num_mat)
      integer (kind = kint), intent(inout) :: num_field
!
      integer (kind = kint) :: j, j2
!
!
      num_field = 0
!
      if (num_layer.eq.0) return
!
      if (layer_name(1) .eq. 'all'                                      &
     &      .or. layer_name(1) .eq. 'ALL' ) then
        num_field = numele
!
      else if (layer_name(1) .eq. 'none'                                &
     &      .or. layer_name(1) .eq. 'NONE' ) then
        num_field = 0
!
      else
        do j = 1, num_mat
          do j2 = 1, num_layer
            if ( mat_name(j) .eq. layer_name(j2) ) then
              num_field = num_field + mat_istack(j) - mat_istack(j-1)
            end if
          end do
        end do
!
      end if
!
      end subroutine count_ele_4_layer
!
! ---------------------------------------------------------------------
!
      subroutine set_node_4_layer                                       &
     &         (numnod, numnod_field, iele_start, iele_end,             &
     &          numele, nnod_4_ele, ie, mat_node_flag, inod_field)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: numnod_field
      integer(kind = kint), intent(in) :: iele_start, iele_end
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(inout) :: inod_field(numnod_field)
      integer (kind=kint), intent(inout) :: mat_node_flag(numnod)
!
      integer (kind = kint) :: inod, iele, j1
!
!
      if (numnod_field.eq.0) return
!
      mat_node_flag = 0
!
      do iele = iele_start, iele_end
        do j1 = 1, nnod_4_ele
          inod = ie(iele,j1)
          mat_node_flag(inod) = 1
        end do
      end do
!
      j1 = 0
      do inod = 1, numnod
        if (mat_node_flag(inod) .eq. 1 ) then
          j1 = j1+1
          inod_field(j1) = inod
        end if
      end do
!
      end subroutine set_node_4_layer
!
! ---------------------------------------------------------------------
!
      end module set_layers
