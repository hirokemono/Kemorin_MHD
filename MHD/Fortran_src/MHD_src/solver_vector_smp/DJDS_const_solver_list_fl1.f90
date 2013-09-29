!DJDS_const_solver_list_fl1.f90
!     module DJDS_const_solver_list_fl1
!
!        programmed by H.Matsui and H.Okuda
!        modified by H. Matsui on Nov. 2003
!        modified by H. Matsui on Aug., 2007
!
!      subroutine reordering_djds_smp_l_fl
!
      module DJDS_const_solver_list_fl1
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
      subroutine reordering_djds_smp_l_fl
!
      use m_iccg_parameter
      use calypso_mpi
      use m_geometry_parameter
      use m_machine_parameter
      use m_matrix_work
      use m_colored_connect
      use m_solver_djds_linear_fl
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
       call allocate_4_RCM_fl_l
!
       call count_rcm(NHYP1, OLDtoNEW1, NEWtoOLD1, NLmax1, NUmax1)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!C
!
!C-- Off-Diagonal Component Number
!
      call allocate_number_4_djds_fl_l
!
      call allocate_work_num_4_djds(numnod)
!
      call count_hyperplane(np_smp, numnod, internal_node,              &
     &      NHYP1, IVECT1, npLX1_1, npUX1_1, NLmax1, NUmax1,            &
     &      NLmaxHYP1, NUmaxHYP1, itotal1_fl_l, itotal1_fl_u)

!
      call deallocate_4_IVECmc
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
      call allocate_lists_4_DJDS_fl_l
!
      call allocate_work_4_djds(numnod, NHYP1, npLX1_1, npUX1_1)
!
      call set_djds_ordering(np_smp, numnod, internal_node,             &
     &      inter_smp_stack, NHYP1, IVECT1, STACKmcG1, STACKmc1, PEon1, &
     &      npLX1_1, npUX1_1, NLmax1, NUmax1, NLmaxHYP1, NUmaxHYP1,     &
     &      indexDJDS1_L, indexDJDS1_U, NEWtoOLD_DJDS1_L,               &
     &      NEWtoOLD_DJDS1_U, OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U)
!C
!C== change connectivity
!
      call deallocate_iW_ordering
!
      call allocate_conn_work_4_djds(internal_node, NLmax1, NUmax1)
!
!
      call set_itotal_djds(np_smp, internal_node, NHYP1,                &
     &      npLX1_1, npUX1_1, NLmax1, NUmax1, NLmaxHYP1, NUmaxHYP1,     &
     &      itotal1_fl_l, itotal1_fl_u, indexDJDS1_L, indexDJDS1_U)
!
      call deallocate_mc_connect
!C
!C== ADDRESS
!
      call allocate_address_4_DJDS_fl_l

      call set_item_djds(np_smp, numnod, internal_node,                 &
     &      NHYP1, IVECT1, COLORon1, STACKmc1,                          &
     &      NLmax1, NUmax1, npLX1_1, npUX1_1, NLmaxHYP1, NUmaxHYP1,     &
     &      itotal1_fl_l, itotal1_fl_u, indexDJDS1_L, indexDJDS1_U,     &
     &      itemDJDS1_L, itemDJDS1_U, OLDtoNEW1, NEWtoOLD1,             &
     &      NEWtoOLD_DJDS1_L, NEWtoOLD_DJDS1_U,                         &
     &      OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U, LtoU1)


      call deallocate_work_4_djds
!
!      call check_DJDS_ordering_info_l(my_rank)
!
      end subroutine reordering_djds_smp_l_fl
!
!-----------------------------------------------------------------------
!
      end module DJDS_const_solver_list_fl1
