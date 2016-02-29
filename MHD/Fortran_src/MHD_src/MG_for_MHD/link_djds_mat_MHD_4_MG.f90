!>@file   link_djds_mat_MHD_4_MG.f90
!!@brief  module link_djds_mat_MHD_4_MG
!!
!!@author H. Matsui
!!@date Programmed  H. Matsui in  Dec., 2010
!
!> @brief Connect primary matrix data for MGCG
!!
!!@verbatim
!!      subroutine s_link_djds_mat_MHD_4_MG                             &
!!     &      (MG0_mat_temp, MG0_mat_light, MG0_mat_press, MG0_mat_magp)
!!        type(DJDS_MATRIX),  intent(inout) :: MG0_mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: MG0_mat_light
!!        type(DJDS_MATRIX),  intent(inout) :: MG0_mat_press
!!        type(DJDS_MATRIX),  intent(inout) :: MG0_mat_magp
!!@endverbatim
!
      module link_djds_mat_MHD_4_MG
!
      use m_precision
!
      use m_machine_parameter
      use t_solver_djds
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_link_djds_mat_MHD_4_MG(MG0_mat_temp, MG0_mat_light)
!
      use m_control_parameter
      use m_solver_djds_MHD
!
      type(DJDS_MATRIX),  intent(inout) :: MG0_mat_temp
      type(DJDS_MATRIX),  intent(inout) :: MG0_mat_light
!
!
!   set boundary conditions for matrix
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_matrix_structs T'
        call link_djds_matrix_structs(Tmat_DJDS, MG0_mat_temp)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_matrix_structs C'
        call link_djds_matrix_structs(Cmat_DJDS, MG0_mat_light)
      end if
!
      end subroutine s_link_djds_mat_MHD_4_MG
!
!-----------------------------------------------------------------------
!
      end module  link_djds_mat_MHD_4_MG

