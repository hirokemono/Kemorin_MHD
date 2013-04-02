!
!     module set_DJDS_off_diag
!
!      Written by Hiroaki Matsui on Oct., 2006
!
!      subroutine s_set_DJDS_off_diag (nod1, nod2, mat_num)
!
      module set_DJDS_off_diag
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
      subroutine s_set_DJDS_off_diag (nod1, nod2, mat_num)
!
      use m_solver_djds
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call s_set_DJDS_off_diagonal (internal_node, numnod, np_smp,      &
     &    NLmax, NUmax, itotal_l, itotal_u,                             &
     &    npLX1, npUX1, NHYP, STACKmc, NLmaxHYP, NUmaxHYP,              &
     &    OLDtoNEW, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,                   &
     &    indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,             &
     &    PEon, COLORon, nod1, nod2, mat_num)
!
      end subroutine s_set_DJDS_off_diag
!
!-----------------------------------------------------------------------
!
      end module set_DJDS_off_diag
