!
!     module DJDS_const_solver_list
!
!     Written by H. Matsui
!     Modifies by H. Matsui on May., 2007
!
!      subroutine reordering_djds_smp
!
      module DJDS_const_solver_list
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine reordering_djds_smp
!
      use m_iccg_parameter
      use calypso_mpi
      use m_geometry_parameter
      use m_machine_parameter
      use m_matrix_work
      use m_colored_connect
      use m_solver_djds
!
      use ordering_MC_RCM
      use DJDS_hyperplane
      use DJDS_ordering
      use DJDS_total_nondiag
      use DJDS_nodiag_item
!
      implicit none
!
!
!C +----------------+
!C | reordering RCM |
!C +----------------+
!C===
!
      call allocate_4_RCM(numnod)
!
!      count_rcm
!       (output:: NHYP, OLDtoNEW, NEWtoOLD, NLmax, NUmax)
!
      call count_rcm(NHYP, OLDtoNEW, NEWtoOLD, NLmax, NUmax)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!
!C-- Off-Diagonal Component Number
!
      call allocate_number_4_djds
!
      call allocate_work_num_4_djds(numnod)
!
!   count_hyperplane
!    ( output :: itotal_l, itotal_u, NLmax, NUmax, npLX1, npUX1
!                IVECT, NLmaxHYP, NUmaxHYP)
!
      call count_hyperplane(np_smp, numnod, internal_node,              &
     &      NHYP, IVECT, npLX1, npUX1, NLmax, NUmax,                    &
     &      NLmaxHYP, NUmaxHYP, itotal_l, itotal_u)
!
      call deallocate_4_IVECmc
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
      call allocate_lists_4_DJDS(np_smp, numnod)
!
      call allocate_work_4_djds(numnod, NHYP, npLX1, npUX1)
!
!  set_djds_ordering
!    ( output:: STACKmc, PEon, indexDJDS_L, indexDJDS_U,
!               NEWtoOLD_DJDS_L, NEWtoOLD_DJDS_U,
!               OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U)
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
!
      call allocate_conn_work_4_djds(internal_node, NLmax, NUmax)
!
!  set_itotal_djds
!    ( output:: itotal_l, itotal_u) 
!
      call set_itotal_djds(np_smp, internal_node, NHYP, npLX1, npUX1,   &
     &      NLmax, NUmax, NLmaxHYP, NUmaxHYP, itotal_l, itotal_u,       &
     &      indexDJDS_L, indexDJDS_U)
!
!
      call deallocate_mc_connect
!C
!C== ADDRESS
!
      call allocate_address_4_DJDS
!
!  set_item_djds
!  (output:: COLORon, itemDJDS_L, itemDJDS_U, OLDtoNEW, NEWtoOLD, LtoU)
!
      call set_item_djds(np_smp, numnod, internal_node,                 &
     &      NHYP, IVECT, COLORon, STACKmc,                              &
     &      NLmax, NUmax, npLX1, npUX1, NLmaxHYP, NUmaxHYP,             &
     &      itotal_l, itotal_u, indexDJDS_L, indexDJDS_U,               &
     &      itemDJDS_L, itemDJDS_U, OLDtoNEW, NEWtoOLD,                 &
     &      NEWtoOLD_DJDS_L, NEWtoOLD_DJDS_U,                           &
     &      OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, LtoU)


      call deallocate_work_4_djds
!
!      call check_DJDS_ordering_info(my_rank, numnod)
!
      end subroutine reordering_djds_smp
!
!  ---------------------------------------------------------------------
!
      end module DJDS_const_solver_list
