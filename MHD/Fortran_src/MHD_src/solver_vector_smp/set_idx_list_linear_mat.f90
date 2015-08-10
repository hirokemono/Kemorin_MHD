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
      use set_idx_list_quad_mat
!
!
      if (nnod_4_ele.ne.num_t_linear) then
        call set_index_list_4_mat_whole                                 &
     &     (nnod_4_ele, DJDS_linear, idx_4_l_mat)
      else
        idx_4_l_mat = idx_4_mat
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
