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
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_sorted_node
!
      use set_off_diagonals
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
      use m_parallel_var_dof
!
      integer(kind = kint) :: nod1, nod2, mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
!$omp parallel private(k2,iproc,inum,inn,ist,ied,                       &
!$omp&                 in,iele,iconn,nod1,nod2,mat_num)
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
!
              iele = iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
              nod1 = ie(iele,iconn)
              nod2 = ie(iele,k2)
!
!             write(*,*) my_rank, nod1, nod2
              call set_off_diag( nod1, nod2, mat_num )
              idx_4_mat(in,k2) = mat_num
!
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
!
      integer(kind = kint) :: nod1, nod2, mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
!$omp parallel private(k2,iproc,inum,inn,ist,ied,                       &
!$omp&                 in,iele,iconn,nod1,nod2,mat_num)
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
!
              iele = iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
              nod1 = ie(iele,iconn)
              nod2 = ie(iele,k2)
!
              if (iele.ge.iele_fl_start .and. iele.le.iele_fl_end) then
                call set_off_diag_fluid( nod1, nod2, mat_num )
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
!
      integer(kind = kint) :: nod1, nod2, mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
!$omp parallel private(k2,iproc,inum,inn,ist,ied,                       &
!$omp&                 in,iele,iconn,nod1,nod2,mat_num)
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
!
              iele = iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
              nod1 = ie(iele,iconn)
              nod2 = ie(iele,k2)
!
              if (iele.ge.iele_cd_start .and. iele.le.iele_cd_end) then
!
!               call set_off_diag_conduct( nod1, nod2, mat_num )
!               idx_4_cd_mat(in,k2) = mat_num
!
                call set_off_diag( nod1, nod2, mat_num ) 
                idx_4_cd_mat_full(in,k2) = mat_num
!
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
!
      integer(kind = kint) :: nod1, nod2, mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
!$omp parallel private(k2,iproc,inum,inn,ist,ied,                      &
!$omp&                 in,iele,iconn,nod1,nod2,mat_num)
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
              nod1 = ie(iele,iconn)
              nod2 = ie(iele,k2)

              if (iele.ge.iele_ins_start                                &
     &          .and. iele.le.iele_ins_end) then

                call set_off_diag_insulate( nod1, nod2, mat_num )
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
