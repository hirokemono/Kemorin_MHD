!>@file   link_djds_mat_MHD_4_MG.f90
!!@brief  module link_djds_mat_MHD_4_MG
!!
!!@author H. Matsui
!!@date Programmed  H. Matsui in  Dec., 2010
!
!> @brief Connect primary matrix data for MGCG
!!
!!@verbatim
!!      subroutine s_link_djds_mat_MHD_4_MG(MG0_mat_velo, mat_magne     &
!!     &          MG0_mat_temp, mat_d_scalar, MG0_mat_press, mat_magp)
!!        type(DJDS_MATRIX),  intent(inout) :: MG0_mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: MG0_mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
!!        type(DJDS_MATRIX),  intent(inout) :: MG0_mat_press
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
!!
!!      subroutine link_djds_mat_magne_4_MG(mat_magne)
!!        type(DJDS_MATRIX), intent(inout) :: mat_magne
!!      subroutine link_djds_mat_magp_4_MG(mat_magp)
!!        type(DJDS_MATRIX), intent(inout) :: mat_magp
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
      private :: link_djds_mat_magne_4_MG
      private :: link_djds_mat_magp_4_MG
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_link_djds_mat_MHD_4_MG(MG0_mat_velo, mat_magne,      &
     &          MG0_mat_temp, MG0_mat_light, MG0_mat_press, mat_magp)
!
      use m_control_parameter
      use m_velo_matrix
      use m_temp_matrix
      use m_light_element_matrix
!
      type(DJDS_MATRIX),  intent(inout) :: MG0_mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: MG0_mat_temp
      type(DJDS_MATRIX),  intent(inout) :: MG0_mat_light
      type(DJDS_MATRIX),  intent(inout) :: MG0_mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
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
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_mat_magp_4_MG'
        call link_djds_mat_magp_4_MG(mat_magp)
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_mat_magne_4_MG'
        call link_djds_mat_magne_4_MG(mat_magne)
      end if
!
      if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_mat_magne_4_MG'
        call link_djds_mat_magne_4_MG(mat_magne)
      end if
!
!
      end subroutine s_link_djds_mat_MHD_4_MG
!
!-----------------------------------------------------------------------
!
      subroutine link_djds_mat_magne_4_MG(mat_magne)
!
      use m_magne_matrix
!
      type(DJDS_MATRIX), intent(inout) :: mat_magne
!
!
       mat_magne%num_diag =      numnod
       mat_magne%internal_diag = internal_node
       mat_magne%istart_diag =   im_mag_d
       mat_magne%istart_l =      im_mag_l
       mat_magne%istart_u =      im_mag_u
       mat_magne%num_non0 =      num_mag_comp
!
       mat_magne%aiccg =>  aiccg_magne
       mat_magne%D =>      aiccg_magne(im_mag_d:im_mag_l-1)
       mat_magne%AL =>     aiccg_magne(im_mag_l:im_mag_u-1)
       mat_magne%AU =>     aiccg_magne(im_mag_u:num_mag_comp)
       mat_magne%ALUG_L => ALUG_magne_L
       mat_magne%ALUG_U => ALUG_magne_U
!
      end subroutine link_djds_mat_magne_4_MG
!
!-----------------------------------------------------------------------
!
      subroutine link_djds_mat_magp_4_MG(mat_magp)
!
      use m_mag_potential_matrix
!
      type(DJDS_MATRIX), intent(inout) :: mat_magp
!
!
       mat_magp%num_diag =      numnod
       mat_magp%internal_diag = internal_node
       mat_magp%istart_diag =   im_mp_d
       mat_magp%istart_l =      im_mp_l
       mat_magp%istart_u =      im_mp_u
       mat_magp%num_non0 =      num_mp_comp
!
       mat_magp%aiccg =>  aiccg_mag_p
       mat_magp%D =>      aiccg_mag_p(im_mp_d:im_mp_l-1)
       mat_magp%AL =>     aiccg_mag_p(im_mp_l:im_mp_u-1)
       mat_magp%AU =>     aiccg_mag_p(im_mp_u:num_mp_comp)
       mat_magp%ALUG_L => ALUG_mag_p_l
       mat_magp%ALUG_U => ALUG_mag_p_u
!
      end subroutine link_djds_mat_magp_4_MG
!
!-----------------------------------------------------------------------
!
      end module  link_djds_mat_MHD_4_MG

