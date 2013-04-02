!DJDS_const_solver_list_cd.f90
!     module DJDS_const_solver_list_cd
!
!        programmed by H.Matsui and H.Okuda
!        modified by H. Matsui on Nov. 2003
!        modified by H. Matsui on Aug., 2007
!
!      subroutine reordering_djds_smp_cd
!
      module DJDS_const_solver_list_cd
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine reordering_djds_smp_cd
!
      use m_parallel_var_dof
      use m_iccg_parameter
      use m_geometry_parameter
      use m_machine_parameter
      use m_matrix_work
      use m_colored_connect
      use m_solver_djds_conduct
!
      use ordering_MC_RCM
      use DJDS_hyperplane
      use DJDS_ordering
      use DJDS_total_nondiag
      use DJDS_nodiag_item
!
!C +----------------+
!C | reordering RCM |
!C +----------------+
!C===
!
      call allocate_4_RCM_cd
      call count_rcm(NHYP, OLDtoNEW, NEWtoOLD, NLmax, NUmax)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!
!C-- Off-Diagonal Component Number
!
      call allocate_number_4_djds_cd
      call allocate_work_num_4_djds(numnod)
!
      call count_hyperplane(np_smp, numnod, internal_node,              &
     &      NHYP, IVECT, npLX1, npUX1, NLmax, NUmax,                    &
     &      NLmaxHYP, NUmaxHYP, itotal_cd_l, itotal_cd_u)
!
      call deallocate_4_IVECmc
!
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
      call allocate_lists_4_DJDS_cd
      call allocate_work_4_djds(numnod, NHYP, npLX1, npUX1)
!
      call set_djds_ordering(np_smp, numnod, internal_node,             &
     &      inter_smp_stack, NHYP, IVECT, STACKmcG, STACKmc, PEon,      &
     &      npLX1, npUX1, NLmax, NUmax, NLmaxHYP, NUmaxHYP,             &
     &      indexDJDS_L, indexDJDS_U, NEWtoOLD_DJDS_L, NEWtoOLD_DJDS_U, &
     &      OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U)

!C
!C== change connectivity
!
      call deallocate_iW_ordering
      call allocate_conn_work_4_djds(internal_node, NLmax, NUmax)
!
      call set_itotal_djds(np_smp, internal_node, NHYP, npLX1, npUX1,   &
     &      NLmax, NUmax, NLmaxHYP, NUmaxHYP, itotal_cd_l, itotal_cd_u, &
     &      indexDJDS_L, indexDJDS_U)
!
      call deallocate_mc_connect
!C
!C== ADDRESS
!
      call allocate_address_4_DJDS_cd
      call set_item_djds(np_smp, numnod, internal_node,                 &
     &      NHYP, IVECT, COLORon, STACKmc,                              &
     &      NLmax, NUmax, npLX1, npUX1, NLmaxHYP, NUmaxHYP,             &
     &      itotal_cd_l, itotal_cd_u, indexDJDS_L, indexDJDS_U,         &
     &      itemDJDS_L, itemDJDS_U, OLDtoNEW, NEWtoOLD,                 &
     &      NEWtoOLD_DJDS_L, NEWtoOLD_DJDS_U,                           &
     &      OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, LtoU)
!
      call deallocate_work_4_djds
      call check_DJDS_ordering_info(my_rank)
!
      end subroutine reordering_djds_smp_cd
!
!-----------------------------------------------------------------------
!
      end module DJDS_const_solver_list_cd
