!set_comm_nod_4_cube.f90
!     module set_comm_nod_4_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine count_node_id(sl_rng, inod)
!!      subroutine set_im_node                                          &
!!     &         (sl_rng, loc_id, ntot_import, item_import, inod)
!!      subroutine set_ex_node                                          &
!!     &         (sl_rng, loc_id, ntot_export, item_export, inod)
!!        type(slleve_range), intent(in) :: sl_rng
!!        type(local_node_id_cube), intent(in) :: loc_id
!
      module set_comm_nod_4_cube
!
      use m_precision
!
      use m_comm_data_cube_kemo
      use t_sleeve_cube
      use t_local_node_id_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_node_id(sl_rng, inod)
!
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(inout)  :: inod
!
!
      inod = inod + (sl_rng%ie - sl_rng%is + 1)                         &
     &             * (sl_rng%je - sl_rng%js + 1)                        &
     &             * (sl_rng%ke - sl_rng%ks + 1)
!
      end subroutine count_node_id
!
! ----------------------------------------------------------------------
!
      subroutine set_im_node                                            &
     &         (sl_rng, loc_id, ntot_import, item_import, inod)
!
      type(slleve_range), intent(in) :: sl_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer(kind = kint), intent(in) :: ntot_import
!
      integer(kind = kint), intent(inout) :: item_import(ntot_import)
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind = kint) :: i, j, k
!
!
      do k = sl_rng%ks, sl_rng%ke
        do j = sl_rng%js, sl_rng%je
          do i = sl_rng%is, sl_rng%ie

            inod = inod + 1
            item_import(inod) =  loc_id%node_id_lc(i,j,k)

          enddo
        enddo
      enddo
!
      end subroutine set_im_node
!
! ----------------------------------------------------------------------
!
      subroutine set_ex_node                                            &
     &         (sl_rng, loc_id, ntot_export, item_export, inod)
!
!
      type(slleve_range), intent(in) :: sl_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer(kind = kint), intent(in) :: ntot_export
!
      integer(kind = kint), intent(inout) :: item_export(ntot_export)
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind = kint) :: i, j, k
!
!
      do k = sl_rng%ks, sl_rng%ke
        do j = sl_rng%js, sl_rng%je
          do i = sl_rng%is, sl_rng%ie
            inod = inod + 1
            item_export(inod) =  loc_id%node_id_lc(i,j,k)
          enddo
        enddo
      enddo
!
      end subroutine set_ex_node
!!
! ----------------------------------------------------------------------
!
      end module set_comm_nod_4_cube
