!
!     module set_off_diagonals
!
!      Written by Hiroaki Matsui on Oct., 2006
!
!      subroutine set_off_diag_conduct (nod1, nod2, mat_num)
!      subroutine set_off_diag_insulate (nod1, nod2, mat_num)
!
!      subroutine set_off_diag_linear_fl (nod1, nod2, mat_num)
!      subroutine set_off_diag_linear_cd (nod1, nod2, mat_num)
!      subroutine set_off_diag_linear_ins (nod1, nod2, mat_num)
!
      module set_off_diagonals
!
      use m_precision
!
      use m_geometry_parameter
      use m_machine_parameter
!
      use set_DJDS_off_diagonal
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_off_diag_conduct (nod1, nod2, mat_num)
!
      use m_solver_djds_MHD
      use set_idx_4_mat_type
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call set_off_diag_type(numnod, internal_node, DJDS_conduct,       &
     &   nod1, nod2, mat_num)
!
      end subroutine set_off_diag_conduct
!
!-----------------------------------------------------------------------
!
      subroutine set_off_diag_insulate (nod1, nod2, mat_num)
!
      use m_solver_djds_MHD
      use set_idx_4_mat_type
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call set_off_diag_type(numnod, internal_node, DJDS_insulator,     &
     &   nod1, nod2, mat_num)
!
      end subroutine set_off_diag_insulate
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_off_diag_linear_fl (nod1, nod2, mat_num)
!
      use m_solver_djds_MHD
      use set_idx_4_mat_type
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call set_off_diag_type(numnod, internal_node, DJDS_fl_l,          &
     &   nod1, nod2, mat_num)
!
      end subroutine set_off_diag_linear_fl
!
!-----------------------------------------------------------------------
!
      subroutine set_off_diag_linear_cd (nod1, nod2, mat_num)
!
      use m_solver_djds_MHD
      use set_idx_4_mat_type
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call set_off_diag_type(numnod, internal_node, DJDS_cd_l,          &
     &   nod1, nod2, mat_num)
!
      end subroutine set_off_diag_linear_cd
!
!-----------------------------------------------------------------------
!
      subroutine set_off_diag_linear_ins (nod1, nod2, mat_num)
!
      use m_solver_djds_MHD
      use set_idx_4_mat_type
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call set_off_diag_type(numnod, internal_node, DJDS_ins_l,         &
     &   nod1, nod2, mat_num)
!
      end subroutine set_off_diag_linear_ins
!
!-----------------------------------------------------------------------
!
      end module set_off_diagonals
