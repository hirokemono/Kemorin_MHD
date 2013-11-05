!>@file   link_djds_mat_MHD_4_MG.f90
!!@brief  module link_djds_mat_MHD_4_MG
!!
!!@author H. Matsui
!!@date Programmed  H. Matsui in  Dec., 2010
!
!> @brief Connect primary matrix data for MGCG
!!
!!@verbatim
!!      subroutine s_link_djds_mat_MHD_4_MG(MG0_mat_velo, MG0_mat_magne,&
!!     &       MG0_mat_temp, MG0_mat_light, MG0_mat_press, MG0_mat_magp)
!!        type(DJDS_MATRIX),  intent(inout) :: MG0_mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: MG0_mat_magne
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
      use m_geometry_parameter
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
      subroutine s_link_djds_mat_MHD_4_MG(MG0_mat_velo, MG0_mat_magne,  &
     &       MG0_mat_temp, MG0_mat_light, MG0_mat_press, MG0_mat_magp)
!
      use m_control_parameter
      use m_velo_matrix
      use m_magne_matrix
      use m_temp_matrix
      use m_light_element_matrix
!
      type(DJDS_MATRIX),  intent(inout) :: MG0_mat_velo
      type(DJDS_MATRIX),  intent(inout) :: MG0_mat_magne
      type(DJDS_MATRIX),  intent(inout) :: MG0_mat_temp
      type(DJDS_MATRIX),  intent(inout) :: MG0_mat_light
      type(DJDS_MATRIX),  intent(inout) :: MG0_mat_press
      type(DJDS_MATRIX),  intent(inout) :: MG0_mat_magp
!
!
!   set boundary conditions for matrix
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_matrix_structs P'
        call link_djds_matrix_structs(Pmat_DJDS, MG0_mat_press)
      end if
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_matrix_structs V'
        call link_djds_matrix_structs(Vmat_DJDS, MG0_mat_velo)
      end if
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
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &     .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_matrix_structs F'
        call link_djds_matrix_structs(Fmat_DJDS, MG0_mat_magp)
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson                    &
     &  .or. iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_matrix_structs B'
        call link_djds_matrix_structs(Bmat_DJDS, MG0_mat_magne)
      end if
!
      end subroutine s_link_djds_mat_MHD_4_MG
!
!-----------------------------------------------------------------------
!
      end module  link_djds_mat_MHD_4_MG

