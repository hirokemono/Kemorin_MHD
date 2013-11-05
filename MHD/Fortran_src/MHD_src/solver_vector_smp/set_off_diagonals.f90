!
!     module set_off_diagonals
!
!      Written by Hiroaki Matsui on Oct., 2006
!
!      subroutine set_off_diag_fluid (nod1, nod2, mat_num)
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
      subroutine set_off_diag_fluid (nod1, nod2, mat_num)
!
      use m_solver_djds_fluid
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call s_set_DJDS_off_diagonal (internal_node, numnod, np_smp,      &
     &    NLmax, NUmax, itotal_fl_l, itotal_fl_u,                       &
     &    npLX1, npUX1, NHYP, STACKmc, NLmaxHYP, NUmaxHYP,              &
     &    OLDtoNEW, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,                   &
     &    indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,             &
     &    PEon, COLORon, nod1, nod2, mat_num)
!
      end subroutine set_off_diag_fluid
!
!-----------------------------------------------------------------------
!
      subroutine set_off_diag_conduct (nod1, nod2, mat_num)
!
      use m_solver_djds_conduct
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call s_set_DJDS_off_diagonal (internal_node, numnod, np_smp,      &
     &    NLmax, NUmax, itotal_cd_l, itotal_cd_u,                       &
     &    npLX1, npUX1, NHYP, STACKmc, NLmaxHYP, NUmaxHYP,              &
     &    OLDtoNEW, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,                   &
     &    indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,             &
     &    PEon, COLORon, nod1, nod2, mat_num)
!
      end subroutine set_off_diag_conduct
!
!-----------------------------------------------------------------------
!
      subroutine set_off_diag_insulate (nod1, nod2, mat_num)
!
      use m_solver_djds_insulate
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call s_set_DJDS_off_diagonal (internal_node, numnod, np_smp,      &
     &    NLmax, NUmax, itotal_ins_l, itotal_ins_u,                     &
     &    npLX1, npUX1, NHYP, STACKmc, NLmaxHYP, NUmaxHYP,              &
     &    OLDtoNEW, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,                   &
     &    indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,             &
     &    PEon, COLORon, nod1, nod2, mat_num)
!
      end subroutine set_off_diag_insulate
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_off_diag_linear_fl (nod1, nod2, mat_num)
!
      use m_solver_djds_linear_fl
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call s_set_DJDS_off_diagonal (internal_node, numnod, np_smp,      &
     &    NLmax1, NUmax1, itotal1_fl_l, itotal1_fl_u,                   &
     &    npLX1_1, npUX1_1, NHYP1, STACKmc1, NLmaxHYP1, NUmaxHYP1,      &
     &    OLDtoNEW1, OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U,                &
     &    indexDJDS1_L, indexDJDS1_U, itemDJDS1_L, itemDJDS1_U,         &
     &    PEon1, COLORon1, nod1, nod2, mat_num)
!
      end subroutine set_off_diag_linear_fl
!
!-----------------------------------------------------------------------
!
      subroutine set_off_diag_linear_cd (nod1, nod2, mat_num)
!
      use m_solver_djds_linear_cd
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call s_set_DJDS_off_diagonal (internal_node, numnod, np_smp,      &
     &    NLmax1, NUmax1, itotal1_cd_l, itotal1_cd_u,                   &
     &    npLX1_1, npUX1_1, NHYP1, STACKmc1, NLmaxHYP1, NUmaxHYP1,      &
     &    OLDtoNEW1, OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U,                &
     &    indexDJDS1_L, indexDJDS1_U, itemDJDS1_L, itemDJDS1_U,         &
     &    PEon1, COLORon1, nod1, nod2, mat_num)
!
      end subroutine set_off_diag_linear_cd
!
!-----------------------------------------------------------------------
!
      subroutine set_off_diag_linear_ins (nod1, nod2, mat_num)
!
      use m_solver_djds_linear_ins
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call s_set_DJDS_off_diagonal (internal_node, numnod, np_smp,      &
     &    NLmax1, NUmax1, itotal1_ins_l, itotal1_ins_u,                 &
     &    npLX1_1, npUX1_1, NHYP1, STACKmc1, NLmaxHYP1, NUmaxHYP1,      &
     &    OLDtoNEW1, OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U,                &
     &    indexDJDS1_L, indexDJDS1_U, itemDJDS1_L, itemDJDS1_U,         &
     &    PEon1, COLORon1, nod1, nod2, mat_num)
!
      end subroutine set_off_diag_linear_ins
!
!-----------------------------------------------------------------------
!
      end module set_off_diagonals
