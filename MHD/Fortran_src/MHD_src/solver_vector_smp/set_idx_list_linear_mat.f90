!
!      module set_idx_list_linear_mat
!
!      programmed by H.Matsui and H.Okuda on 2002
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine set_index_list_4_mat_etr_l
!      subroutine set_index_list_4_mat_fl_l
!      subroutine set_index_list_4_mat_cd_l
!      subroutine set_index_list_4_mat_ins_l
!
      module set_idx_list_linear_mat
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
      subroutine set_index_list_4_mat_etr_l
!
      use m_solver_djds_MHD
      use set_idx_4_mat_type
!
      integer(kind = kint) :: mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
      if (nnod_4_ele.ne.num_t_linear) then
!
!$omp parallel private(k2,iproc,inum,inn,ist,ied,                       &
!$omp&                 in,iele,iconn,mat_num)
        do k2 = 1, num_t_linear
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
                call set_off_diag_type(numnod, internal_node,           &
     &              DJDS_linear, ie(iele,iconn), ie(iele,k2),           &
     &              mat_num)
                idx_4_l_mat(in,k2) = mat_num
!
              end do
            end do
          end do
!$omp end do nowait
!
        end do
!$omp end parallel
!
      else
!
        idx_4_l_mat = idx_4_mat
!
      end if
!
      end subroutine set_index_list_4_mat_etr_l
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_fl_l
!
      use m_geometry_data_MHD
      use m_sorted_node_MHD
      use m_solver_djds_MHD
      use set_idx_list_quad_mat
!
!
      if (nnod_4_ele.ne.num_t_linear) then
        call set_index_list_4_DJDS_mat(num_t_linear,                    &
     &      iele_cd_start, iele_cd_end, DJDS_fl_l, idx_4_fll_mat)
      else
        idx_4_fll_mat = idx_4_fl_mat
      end if
!
      end subroutine set_index_list_4_mat_fl_l
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_cd_l
!
      use m_geometry_data_MHD
      use m_sorted_node_MHD
      use m_solver_djds_MHD
      use set_idx_list_quad_mat
!
      integer(kind = kint) :: mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
      if (nnod_4_ele.ne.num_t_linear) then
        call set_index_list_4_DJDS_mat(num_t_linear,                    &
     &      iele_cd_start, iele_cd_end, DJDS_cd_l, idx_4_cdl_mat)
        call set_index_list_4_DJDS_mat(num_t_linear,                    &
     &      iele_cd_start, iele_cd_end, DJDS_linear,                    &
     &      idx_4_cdl_mat_full)
      else
        idx_4_cdl_mat      = idx_4_cd_mat
        idx_4_cdl_mat_full = idx_4_cd_mat_full
      end if
!
      end subroutine set_index_list_4_mat_cd_l
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_ins_l
!
      use m_geometry_data_MHD
      use m_sorted_node_MHD
      use m_solver_djds_MHD
      use set_idx_list_quad_mat
!
!
      if (nnod_4_ele.ne.num_t_linear) then
        call set_index_list_4_DJDS_mat(num_t_linear,                    &
     &      iele_ins_start, iele_ins_end, DJDS_ins_l, idx_4_insl_mat)
      else
        idx_4_insl_mat = idx_4_ins_mat
      end if

      end subroutine set_index_list_4_mat_ins_l
!
!-----------------------------------------------------------------------
!
      end module set_idx_list_linear_mat
