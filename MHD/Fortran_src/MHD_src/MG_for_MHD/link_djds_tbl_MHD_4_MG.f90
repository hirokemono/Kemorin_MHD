!link_djds_tbl_MHD_4_MG.f90
!      module link_djds_tbl_MHD_4_MG
!
!      Written by Hiroaki Matsui on Dec., 2008
!
!      subroutine s_link_djds_tbl_MHD_4_MG(MG0_djds_tbl,                &
!     &          MG0_djds_tbl_fl, MG0_djds_tbl_l, MG0_djds_tbl_fll)
!        type(DJDS_ordering_table),  intent(inout) :: MG0_djds_tbl
!        type(DJDS_ordering_table),  intent(inout) :: MG0_djds_tbl_fl
!        type(DJDS_ordering_table),  intent(inout) :: MG0_djds_tbl_cd
!        type(DJDS_ordering_table),  intent(inout) :: MG0_djds_tbl_ins
!        type(DJDS_ordering_table),  intent(inout) :: MG0_djds_tbl_l
!        type(DJDS_ordering_table),  intent(inout) :: MG0_djds_tbl_fll
!        type(DJDS_ordering_table),  intent(inout) :: MG0_djds_tbl_cdl
!        type(DJDS_ordering_table),  intent(inout) :: MG0_djds_tbl_insl
!
      module link_djds_tbl_MHD_4_MG
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
      subroutine s_link_djds_tbl_MHD_4_MG(MG0_djds_tbl,                 &
     &          MG0_djds_tbl_fl,  MG0_djds_tbl_l, MG0_djds_tbl_fll)
!
      use t_solver_djds
      use m_solver_djds_MHD
!
      type(DJDS_ordering_table),  intent(inout) :: MG0_djds_tbl
      type(DJDS_ordering_table),  intent(inout) :: MG0_djds_tbl_fl
      type(DJDS_ordering_table),  intent(inout) :: MG0_djds_tbl_l
      type(DJDS_ordering_table),  intent(inout) :: MG0_djds_tbl_fll
!
!
      write(*,*) 'link_djds_connect_structs entire domain'
      call link_djds_connect_structs(DJDS_entire, MG0_djds_tbl)
!
      write(*,*) 'link_djds_connect_structs fluid'
      call link_djds_connect_structs(DJDS_fluid, MG0_djds_tbl_fl)
!
!      call link_djds_connect_structs(DJDS_conduct,   MG0_djds_tbl_cd)
!      call link_djds_connect_structs(DJDS_insulator, MG0_djds_tbl_ins)
!
      write(*,*) 'link_djds_connect_structs linear'
      call  link_djds_connect_structs(DJDS_linear, MG0_djds_tbl_l)
      write(*,*) 'link_djds_connect_structs linear fluid'
      call  link_djds_connect_structs(DJDS_fl_l, MG0_djds_tbl_fll)
!
!      call link_djds_connect_structs(DJDS_cd_l,  MG0_djds_tbl_cdl)
!      call link_djds_connect_structs(DJDS_ins_l, MG0_djds_tbl_insl)
!
      end subroutine s_link_djds_tbl_MHD_4_MG
!
!-----------------------------------------------------------------------
!
      end module  link_djds_tbl_MHD_4_MG
