!>@file   set_idx_list_quad_mat.f90
!!@brief  module set_idx_list_quad_mat
!!
!!@author H. Matsui and H. Okuda
!!@date   programmed by H.Matsui and H. Okuda in 2002
!!@n      modified by H. Matsui in Oct., 2005
!!@n      modified by H. Matsui in Nov., 2013
!
!>@brief   Matrix assemble for structure
!!
!!@verbatim
!!      subroutine set_index_list_4_mat_etr
!!      subroutine set_index_list_4_mat_fl
!!      subroutine set_index_list_4_mat_cd
!!      subroutine set_index_list_4_mat_ins
!!      subroutine set_index_list_4_mat_whole(nnod_1ele,                &
!!     &          djds_tbl, idx_4_djds_mat)
!!      subroutine set_index_list_4_DJDS_mat(nnod_1ele,                 &
!!     &          iele_start, iele_end, djds_tbl, idx_4_djds_mat)
!!
!!@endverbatim
!
      module set_idx_list_quad_mat
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_data
      use m_sorted_node
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_etr
!
      use m_solver_djds_MHD
!
!
      call set_index_list_4_mat_whole                                   &
     &   (ele1%nnod_4_ele, DJDS_entire, idx_4_mat)
!
      end subroutine set_index_list_4_mat_etr
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_fl
!
      use m_geometry_data_MHD
      use m_sorted_node_MHD
      use m_solver_djds_MHD
!
!
      call set_index_list_4_DJDS_mat(ele1%nnod_4_ele,                   &
     &    iele_fl_start, iele_fl_end, DJDS_fluid, idx_4_fl_mat)
!
      end subroutine set_index_list_4_mat_fl
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_cd
!
      use m_geometry_data_MHD
      use m_sorted_node_MHD
      use m_solver_djds_MHD
!
!
!      call set_index_list_4_DJDS_mat(ele1%nnod_4_ele,                  &
!     &    iele_cd_start, iele_cd_end, DJDS_conduct, idx_4_cd_mat)
      call set_index_list_4_DJDS_mat(ele1%nnod_4_ele,                   &
     &    iele_cd_start, iele_cd_end, DJDS_entire, idx_4_cd_mat_full)
!
      end subroutine set_index_list_4_mat_cd
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_ins
!
      use m_geometry_data_MHD
      use m_sorted_node_MHD
      use m_solver_djds_MHD
!
!
      call set_index_list_4_DJDS_mat(ele1%nnod_4_ele,                   &
     &    iele_ins_start, iele_ins_end, DJDS_insulator, idx_4_ins_mat)
!
      end subroutine set_index_list_4_mat_ins
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_whole(nnod_1ele,                  &
     &          djds_tbl, idx_4_djds_mat)
!
      use t_solver_djds
      use set_idx_4_mat_type
!
      integer(kind = kint), intent(in) :: nnod_1ele
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      integer(kind = kint), intent(inout)                               &
     &                     :: idx_4_djds_mat(num_sort_smp,nnod_1ele)
!
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in, mat_num, k2
!
!
!$omp parallel private(k2,iproc,inum,inn,ist,ied,in,iele,iconn,mat_num)
      do k2 = 1, nnod_1ele
!
!$omp do
        do iproc = 1, np_smp
          do inum = 1, inod_ele_max
!
            inn = inum + inod_ele_max*(iproc-1)
            ist = nod_stack_smp(inn-1)+1
            ied = nod_stack_smp(inn)
!
            do in = ist, ied
              iele = iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
!
              call set_off_diag_type(node1%numnod, node1%internal_node, &
     &            djds_tbl, ele1%ie(iele,iconn), ele1%ie(iele,k2),      &
     &            mat_num)
              idx_4_djds_mat(in,k2) = mat_num
            end do
          end do
        end do
!$omp end do nowait
!
      end do
!$omp end parallel
!
      end subroutine set_index_list_4_mat_whole
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_DJDS_mat(nnod_1ele,                   &
     &          iele_start, iele_end, djds_tbl, idx_4_djds_mat)
!
      use t_solver_djds
      use set_idx_4_mat_type
!
      integer(kind = kint), intent(in) :: nnod_1ele
      integer(kind = kint), intent(in) :: iele_start, iele_end
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      integer(kind = kint), intent(inout)                               &
     &                     :: idx_4_djds_mat(num_sort_smp,nnod_1ele)
!
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in, k2, mat_num
!
!
!$omp parallel private(k2,iproc,inum,inn,ist,ied,in,iele,iconn,mat_num)
      do k2 = 1, nnod_1ele
!
!$omp do
        do iproc = 1, np_smp
          do inum = 1, inod_ele_max
            inn = inum + inod_ele_max*(iproc-1)
            ist = nod_stack_smp(inn-1)+1
            ied = nod_stack_smp(inn)
!
            do in = ist, ied
              iele = iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
!
              if(iele.ge.iele_start .and. iele.le.iele_end) then
                call set_off_diag_type                                  &
     &             (node1%numnod, node1%internal_node,                  &
     &              djds_tbl, ie(iele,iconn), ie(iele,k2), mat_num)
                idx_4_djds_mat(in,k2) = mat_num
              else
                idx_4_djds_mat(in,k2) = 0
              end if
            end do
          end do
        end do
!$omp end do nowait

      end do
!$omp end parallel
!
      end subroutine set_index_list_4_DJDS_mat
!
!-----------------------------------------------------------------------
!
      end module set_idx_list_quad_mat
