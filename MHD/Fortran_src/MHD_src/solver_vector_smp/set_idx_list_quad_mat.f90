!
!      module set_idx_list_quad_mat
!
!      programmed by H.Matsui and H.Okuda on 2002
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine set_index_list_4_mat_etr
!      subroutine set_index_list_4_mat_fl
!      subroutine set_index_list_4_mat_cd
!      subroutine set_index_list_4_mat_ins
!
      module set_idx_list_quad_mat
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
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
      use calypso_mpi
      use m_solver_djds_MHD
      use set_idx_4_mat_type
!
      integer(kind = kint) :: mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
!$omp parallel private(k2,iproc,inum,inn,ist,ied,                       &
!$omp&                 in,iele,iconn,mat_num)
      do k2 = 1, nnod_4_ele
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
              call set_off_diag_type(numnod, internal_node,             &
     &            DJDS_entire, ie(iele,iconn), ie(iele,k2), mat_num)
              idx_4_mat(in,k2) = mat_num
            end do
          end do
        end do
!$omp end do nowait
!
      end do
!$omp end parallel
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
      use set_idx_4_mat_type
!
      integer(kind = kint) :: mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
!$omp parallel private(k2,iproc,inum,inn,ist,ied,                       &
!$omp&                 in,iele,iconn,mat_num)
      do k2 = 1, nnod_4_ele
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
              if (iele.ge.iele_fl_start .and. iele.le.iele_fl_end) then
                call set_off_diag_type(numnod, internal_node,           &
     &              DJDS_fluid, ie(iele,iconn), ie(iele,k2), mat_num)
                idx_4_fl_mat(in,k2) = mat_num
              else
                idx_4_fl_mat(in,k2) = 0
              end if
!
            end do
          end do
        end do
!$omp end do nowait
!
      end do
!$omp end parallel
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
      use set_idx_4_mat_type
!
      integer(kind = kint) :: mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
!$omp parallel private(k2,iproc,inum,inn,ist,ied,                       &
!$omp&                 in,iele,iconn,mat_num)
      do k2 = 1, nnod_4_ele
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
              if (iele.ge.iele_cd_start .and. iele.le.iele_cd_end) then
!                call set_off_diag_type(numnod, internal_node,          &
!     &            DJDS_conduct, ie(iele,iconn), ie(iele,k2), mat_num)
!                idx_4_cd_mat(in,k2) = mat_num
!
                call set_off_diag_type(numnod, internal_node,           &
     &              DJDS_entire, ie(iele,iconn), ie(iele,k2), mat_num)
                idx_4_cd_mat_full(in,k2) = mat_num
               else
!                 idx_4_cd_mat(in,k2) = 0
                 idx_4_cd_mat_full(in,k2) = 0
               end if
!
             end do
           end do
        end do
!$omp end do nowait
!
      end do
!$omp end parallel
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
      use set_idx_4_mat_type
!
      integer(kind = kint) :: mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
!$omp parallel private(k2,iproc,inum,inn,ist,ied,                      &
!$omp&                 in,iele,iconn,mat_num)
      do k2 = 1, nnod_4_ele
!
!$omp do
        do iproc = 1, np_smp
          do inum = 1, inod_ele_max

            inn = inum + inod_ele_max*(iproc-1)
            ist = nod_stack_smp(inn-1)+1
            ied = nod_stack_smp(inn)

            do in = ist, ied
              iele = iele_sort_smp(in)
              iconn = iconn_sort_smp(in)

              if (iele.ge.iele_ins_start                                &
     &          .and. iele.le.iele_ins_end) then

                call set_off_diag_type(numnod, internal_node,           &
     &              DJDS_insulator, ie(iele,iconn), ie(iele,k2),        &
     &              mat_num)
                idx_4_ins_mat(in,k2) = mat_num

              else
                idx_4_ins_mat(in,k2) = 0
              end if

            end do
          end do
        end do
!$omp end do nowait
!
      end do
!$omp end parallel
!
      end subroutine set_index_list_4_mat_ins
!
!-----------------------------------------------------------------------
!
      end module set_idx_list_quad_mat
