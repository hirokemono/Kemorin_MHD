!link_djds_tbl_MHD_4_MG.f90
!      module link_djds_tbl_MHD_4_MG
!
!      Written by Hiroaki Matsui on Dec., 2008
!
!      subroutine s_link_djds_tbl_MHD_4_MG( djds_tbl, djds_tbl_fl,      &
!     &          djds_tbl_l, djds_tbl_fll)
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_fl
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_cd
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_ins
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_l
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_fll
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_cdl
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_insl
!
!      subroutine  link_first_djds_fl_tbl_4_MG(djds_tbl)
!      subroutine  link_first_djds_cd_tbl_4_MG(djds_tbl)
!      subroutine  link_first_djds_ins_tbl_4_MG(djds_tbl)
!
!      subroutine  link_first_djds_fl_l_tbl_4_MG(djds_tbl)
!      subroutine  link_first_djds_cd_l_tbl_4_MG(djds_tbl)
!      subroutine  link_first_djds_ins_l_tbl_4_MG(djds_tbl)
!
      module link_djds_tbl_MHD_4_MG
!
      use m_precision
!
      use m_control_parameter
      use m_machine_parameter
      use m_geometry_parameter
      use t_solver_djds
!
      implicit none
!
      private ::  link_first_djds_fl_tbl_4_MG
      private ::  link_first_djds_cd_tbl_4_MG
      private ::  link_first_djds_ins_tbl_4_MG
!
      private ::  link_first_djds_fl_l_tbl_4_MG
      private ::  link_first_djds_cd_l_tbl_4_MG
      private ::  link_first_djds_ins_l_tbl_4_MG
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_link_djds_tbl_MHD_4_MG( djds_tbl, djds_tbl_fl,       &
     &          djds_tbl_l, djds_tbl_fll)
!
      use m_solver_djds_MHD
!
      type(DJDS_ordering_table),  intent(inout) :: djds_tbl
      type(DJDS_ordering_table),  intent(inout) :: djds_tbl_fl
      type(DJDS_ordering_table),  intent(inout) :: djds_tbl_l
      type(DJDS_ordering_table),  intent(inout) :: djds_tbl_fll
!
!
      write(*,*) 'link_djds_connect_structs entire domain'
      call link_djds_connect_structs(DJDS_entire, djds_tbl)
!
      write(*,*) 'link_first_djds_fl_tbl_4_MG'
      call  link_first_djds_fl_tbl_4_MG(djds_tbl_fl)
!
!        call  link_first_djds_cd_tbl_4_MG(djds_tbl_cd)
!        call  link_first_djds_ins_tbl_4_MG(djds_tbl_ins)
!
      write(*,*) 'link_djds_connect_structs linear'
      call  link_djds_connect_structs(DJDS_linear, djds_tbl_l)
      write(*,*) 'link_first_djds_fl_l_tbl_4_MG'
      call  link_first_djds_fl_l_tbl_4_MG(djds_tbl_fll)
!
!        call  link_first_djds_cd_l_tbl_4_MG(djds_tbl_cdl)
!        call  link_first_djds_ins_l_tbl_4_MG(djds_tbl_insl)
!
      end subroutine s_link_djds_tbl_MHD_4_MG
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine  link_first_djds_fl_tbl_4_MG(djds_tbl)
!
      use m_comm_table_4_MHD
      use m_solver_djds_fluid
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      djds_tbl%itotal_l = itotal_fl_l
      djds_tbl%itotal_u = itotal_fl_u
      djds_tbl%NHYP =     NHYP
      djds_tbl%NLmax =    NLmax
      djds_tbl%NUmax =    NUmax
      djds_tbl%npLX1 =    NLmax * np_smp
      djds_tbl%npUX1 =    NUmax * np_smp
!
      djds_tbl%IVECT =>           IVECT
      djds_tbl%NEWtoOLD =>        NEWtoOLD
      djds_tbl%OLDtoNEW =>        OLDtoNEW
      djds_tbl%NEWtoOLD_DJDS_U => NEWtoOLD_DJDS_U
      djds_tbl%OLDtoNEW_DJDS_L => OLDtoNEW_DJDS_L
      djds_tbl%OLDtoNEW_DJDS_U => OLDtoNEW_DJDS_U
      djds_tbl%LtoU =>            LtoU
      djds_tbl%indexDJDS_L =>     indexDJDS_L
      djds_tbl%indexDJDS_U =>     indexDJDS_U
      djds_tbl%itemDJDS_L =>      itemDJDS_L
      djds_tbl%itemDJDS_U =>      itemDJDS_U
      djds_tbl%NLmaxHYP =>        NLmaxHYP
      djds_tbl%NUmaxHYP =>        NUmaxHYP
      djds_tbl%STACKmcG =>        STACKmcG
      djds_tbl%STACKmc =>         STACKmc
      djds_tbl%COLORon =>         COLORon
      djds_tbl%PEon =>            PEon
      djds_tbl%NOD_EXPORT_NEW =>  NOD_EXPORT_NEW_fl
!
      end subroutine  link_first_djds_fl_tbl_4_MG
!
!-----------------------------------------------------------------------
!
      subroutine  link_first_djds_cd_tbl_4_MG(djds_tbl)
!
      use m_nod_comm_table
      use m_comm_table_4_MHD
      use m_solver_djds_conduct
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      djds_tbl%itotal_l = itotal_cd_l
      djds_tbl%itotal_u = itotal_cd_u
      djds_tbl%NHYP =     NHYP
      djds_tbl%NLmax =    NLmax
      djds_tbl%NUmax =    NUmax
      djds_tbl%npLX1 =    NLmax * np_smp
      djds_tbl%npUX1 =    NUmax * np_smp
!
      djds_tbl%IVECT =>           IVECT
      djds_tbl%NEWtoOLD =>        NEWtoOLD
      djds_tbl%OLDtoNEW =>        OLDtoNEW
      djds_tbl%NEWtoOLD_DJDS_U => NEWtoOLD_DJDS_U
      djds_tbl%OLDtoNEW_DJDS_L => OLDtoNEW_DJDS_L
      djds_tbl%OLDtoNEW_DJDS_U => OLDtoNEW_DJDS_U
      djds_tbl%LtoU =>            LtoU
      djds_tbl%indexDJDS_L =>     indexDJDS_L
      djds_tbl%indexDJDS_U =>     indexDJDS_U
      djds_tbl%itemDJDS_L =>      itemDJDS_L
      djds_tbl%itemDJDS_U =>      itemDJDS_U
      djds_tbl%NLmaxHYP =>        NLmaxHYP
      djds_tbl%NUmaxHYP =>        NUmaxHYP
      djds_tbl%STACKmcG =>        STACKmcG
      djds_tbl%STACKmc =>         STACKmc
      djds_tbl%COLORon =>         COLORon
      djds_tbl%PEon =>            PEon
      djds_tbl%NOD_EXPORT_NEW =>  NOD_EXPORT_NEW_cd
!
      end subroutine  link_first_djds_cd_tbl_4_MG
!
!-----------------------------------------------------------------------
!
      subroutine  link_first_djds_ins_tbl_4_MG(djds_tbl)
!
      use m_nod_comm_table
      use m_comm_table_4_MHD
      use m_solver_djds_insulate
!
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      djds_tbl%itotal_l = itotal_ins_l
      djds_tbl%itotal_u = itotal_ins_u
      djds_tbl%NHYP =     NHYP
      djds_tbl%NLmax =    NLmax
      djds_tbl%NUmax =    NUmax
      djds_tbl%npLX1 =    NLmax * np_smp
      djds_tbl%npUX1 =    NUmax * np_smp
!
      djds_tbl%IVECT =>           IVECT
      djds_tbl%NEWtoOLD =>        NEWtoOLD
      djds_tbl%OLDtoNEW =>        OLDtoNEW
      djds_tbl%NEWtoOLD_DJDS_U => NEWtoOLD_DJDS_U
      djds_tbl%OLDtoNEW_DJDS_L => OLDtoNEW_DJDS_L
      djds_tbl%OLDtoNEW_DJDS_U => OLDtoNEW_DJDS_U
      djds_tbl%LtoU =>            LtoU
      djds_tbl%indexDJDS_L =>     indexDJDS_L
      djds_tbl%indexDJDS_U =>     indexDJDS_U
      djds_tbl%itemDJDS_L =>      itemDJDS_L
      djds_tbl%itemDJDS_U =>      itemDJDS_U
      djds_tbl%NLmaxHYP =>        NLmaxHYP
      djds_tbl%NUmaxHYP =>        NUmaxHYP
      djds_tbl%STACKmcG =>        STACKmcG
      djds_tbl%STACKmc =>         STACKmc
      djds_tbl%COLORon =>         COLORon
      djds_tbl%PEon =>            PEon
      djds_tbl%NOD_EXPORT_NEW =>  NOD_EXPORT_NEW_ins
!
      end subroutine  link_first_djds_ins_tbl_4_MG
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine  link_first_djds_fl_l_tbl_4_MG(djds_tbl)
!
      use m_comm_table_4_MHD
      use m_solver_djds_linear_fl
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      djds_tbl%itotal_l = itotal1_fl_l
      djds_tbl%itotal_u = itotal1_fl_u
      djds_tbl%NHYP =     NHYP1
      djds_tbl%NLmax =    NLmax1
      djds_tbl%NUmax =    NUmax1
      djds_tbl%npLX1 =    NLmax1 * np_smp
      djds_tbl%npUX1 =    NUmax1 * np_smp
!
      djds_tbl%IVECT =>           IVECT1
      djds_tbl%NEWtoOLD =>        NEWtoOLD1
      djds_tbl%OLDtoNEW =>        OLDtoNEW1
      djds_tbl%NEWtoOLD_DJDS_U => NEWtoOLD_DJDS1_U
      djds_tbl%OLDtoNEW_DJDS_L => OLDtoNEW_DJDS1_L
      djds_tbl%OLDtoNEW_DJDS_U => OLDtoNEW_DJDS1_U
      djds_tbl%LtoU =>            LtoU1
      djds_tbl%indexDJDS_L =>     indexDJDS1_L
      djds_tbl%indexDJDS_U =>     indexDJDS1_U
      djds_tbl%itemDJDS_L =>      itemDJDS1_L
      djds_tbl%itemDJDS_U =>      itemDJDS1_U
      djds_tbl%NLmaxHYP =>        NLmaxHYP1
      djds_tbl%NUmaxHYP =>        NUmaxHYP1
      djds_tbl%STACKmcG =>        STACKmcG1
      djds_tbl%STACKmc =>         STACKmc1
      djds_tbl%COLORon =>         COLORon1
      djds_tbl%PEon =>            PEon1
      djds_tbl%NOD_EXPORT_NEW =>  NOD_EXPORT_NEW_fl1
!
      end subroutine  link_first_djds_fl_l_tbl_4_MG
!
!-----------------------------------------------------------------------
!
      subroutine  link_first_djds_cd_l_tbl_4_MG(djds_tbl)
!
      use m_nod_comm_table
      use m_comm_table_4_MHD
      use m_solver_djds_linear_cd
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      djds_tbl%itotal_l = itotal1_cd_l
      djds_tbl%itotal_u = itotal1_cd_u
      djds_tbl%NHYP =     NHYP1
      djds_tbl%NLmax =    NLmax1
      djds_tbl%NUmax =    NUmax1
      djds_tbl%npLX1 =    NLmax1 * np_smp
      djds_tbl%npUX1 =    NUmax1 * np_smp
!
      djds_tbl%IVECT =>           IVECT1
      djds_tbl%NEWtoOLD =>        NEWtoOLD1
      djds_tbl%OLDtoNEW =>        OLDtoNEW1
      djds_tbl%NEWtoOLD_DJDS_U => NEWtoOLD_DJDS1_U
      djds_tbl%OLDtoNEW_DJDS_L => OLDtoNEW_DJDS1_L
      djds_tbl%OLDtoNEW_DJDS_U => OLDtoNEW_DJDS1_U
      djds_tbl%LtoU =>            LtoU1
      djds_tbl%indexDJDS_L =>     indexDJDS1_L
      djds_tbl%indexDJDS_U =>     indexDJDS1_U
      djds_tbl%itemDJDS_L =>      itemDJDS1_L
      djds_tbl%itemDJDS_U =>      itemDJDS1_U
      djds_tbl%NLmaxHYP =>        NLmaxHYP1
      djds_tbl%NUmaxHYP =>        NUmaxHYP1
      djds_tbl%STACKmcG =>        STACKmcG1
      djds_tbl%STACKmc =>         STACKmc1
      djds_tbl%COLORon =>         COLORon1
      djds_tbl%PEon =>            PEon1
      djds_tbl%NOD_EXPORT_NEW =>  NOD_EXPORT_NEW_cd1
!
      end subroutine  link_first_djds_cd_l_tbl_4_MG
!
!-----------------------------------------------------------------------
!
      subroutine  link_first_djds_ins_l_tbl_4_MG(djds_tbl)
!
      use m_nod_comm_table
      use m_comm_table_4_MHD
      use m_solver_djds_linear_ins
!
       type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      djds_tbl%itotal_l = itotal1_ins_l
      djds_tbl%itotal_u = itotal1_ins_u
      djds_tbl%NHYP =     NHYP1
      djds_tbl%NLmax =    NLmax1
      djds_tbl%NUmax =    NUmax1
      djds_tbl%npLX1 =    NLmax1 * np_smp
      djds_tbl%npUX1 =    NUmax1 * np_smp
!
      djds_tbl%IVECT =>           IVECT1
      djds_tbl%NEWtoOLD =>        NEWtoOLD1
      djds_tbl%OLDtoNEW =>        OLDtoNEW1
      djds_tbl%NEWtoOLD_DJDS_U => NEWtoOLD_DJDS1_U
      djds_tbl%OLDtoNEW_DJDS_L => OLDtoNEW_DJDS1_L
      djds_tbl%OLDtoNEW_DJDS_U => OLDtoNEW_DJDS1_U
      djds_tbl%LtoU =>            LtoU1
      djds_tbl%indexDJDS_L =>     indexDJDS1_L
      djds_tbl%indexDJDS_U =>     indexDJDS1_U
      djds_tbl%itemDJDS_L =>      itemDJDS1_L
      djds_tbl%itemDJDS_U =>      itemDJDS1_U
      djds_tbl%NLmaxHYP =>        NLmaxHYP1
      djds_tbl%NUmaxHYP =>        NUmaxHYP1
      djds_tbl%STACKmcG =>        STACKmcG1
      djds_tbl%STACKmc =>         STACKmc1
      djds_tbl%COLORon =>         COLORon1
      djds_tbl%PEon =>            PEon1
      djds_tbl%NOD_EXPORT_NEW =>  NOD_EXPORT_NEW_ins1
!
      end subroutine  link_first_djds_ins_l_tbl_4_MG
!
!-----------------------------------------------------------------------
!
      end module  link_djds_tbl_MHD_4_MG
