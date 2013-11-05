!>@file   link_djds_mat_MHD_4_MG.f90
!!@brief  module link_djds_mat_MHD_4_MG
!!
!!@author H. Matsui
!!@date Programmed  H. Matsui in  Dec., 2010
!
!> @brief Connect primary matrix data for MGCG
!!
!!@verbatim
!!      subroutine s_link_djds_mat_MHD_4_MG(mat_velo, mat_magne         &
!!     &           mat_temp, mat_d_scalar, mat_press, mat_magp)
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
!!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
!!
!!      subroutine link_djds_mat_velo_4_MG(mat_velo)
!!        type(DJDS_MATRIX), intent(inout) :: mat_velo
!!      subroutine link_djds_mat_magne_4_MG(mat_magne)
!!        type(DJDS_MATRIX), intent(inout) :: mat_magne
!!      subroutine link_djds_mat_press_4_MG(mat_press)
!!        type(DJDS_MATRIX), intent(inout) :: mat_press
!!      subroutine link_djds_mat_magp_4_MG(mat_magp)
!!        type(DJDS_MATRIX), intent(inout) :: mat_magp
!!      subroutine link_djds_mat_temp_4_MG(mat_temp)
!!        type(DJDS_MATRIX), intent(inout) :: mat_temp
!!      subroutine link_djds_mat_dscalar_4_MG(mat_d_scalar)
!!        type(DJDS_MATRIX), intent(inout) :: mat_d_scalar
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
      private :: link_djds_mat_velo_4_MG
      private :: link_djds_mat_magne_4_MG
      private :: link_djds_mat_press_4_MG
      private :: link_djds_mat_magp_4_MG
      private :: link_djds_mat_temp_4_MG
      private :: link_djds_mat_dscalar_4_MG
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_link_djds_mat_MHD_4_MG(mat_velo, mat_magne,          &
     &           mat_temp, mat_d_scalar, mat_press, mat_magp)
!
      use m_control_parameter
!
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
!   set boundary conditions for matrix
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_mat_press_4_MG'
        call link_djds_mat_press_4_MG(mat_press)
      end if
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_mat_velo_4_MG'
        call link_djds_mat_velo_4_MG(mat_velo)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_mat_temp_4_MG'
        call link_djds_mat_temp_4_MG(mat_temp)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        if(iflag_debug .gt. 0) write(*,*) 'link_djds_mat_dscalar_4_MG'
        call link_djds_mat_dscalar_4_MG(mat_d_scalar)
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
      subroutine link_djds_mat_velo_4_MG(mat_velo)
!
      use m_velo_matrix
!
      type(DJDS_MATRIX), intent(inout) :: mat_velo
!
!
       mat_velo%num_diag =      numnod
       mat_velo%internal_diag = internal_node
       mat_velo%istart_diag =   im_velo_d
       mat_velo%istart_l =      im_velo_l
       mat_velo%istart_u =      im_velo_u
       mat_velo%num_non0 =      num_velo_comp
!
       mat_velo%aiccg =>  aiccg_velo
       mat_velo%D =>      aiccg_velo(im_velo_d:im_velo_l-1)
       mat_velo%AL =>     aiccg_velo(im_velo_l:im_velo_u-1)
       mat_velo%AU =>     aiccg_velo(im_velo_u:num_velo_comp)
       mat_velo%ALUG_L => ALUG_velo_L
       mat_velo%ALUG_U => ALUG_velo_U
!
      end subroutine link_djds_mat_velo_4_MG
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
      subroutine link_djds_mat_press_4_MG(mat_press)
!
      use m_press_matrix
!
      type(DJDS_MATRIX), intent(inout) :: mat_press
!
!
       mat_press%num_diag =      numnod
       mat_press%internal_diag = internal_node
       mat_press%istart_diag =   im_press_d
       mat_press%istart_l =      im_press_l
       mat_press%istart_u =      im_press_u
       mat_press%num_non0 =      num_press_comp
!
       mat_press%aiccg =>  aiccg_press
       mat_press%D =>      aiccg_press(im_press_d:im_press_l-1)
       mat_press%AL =>     aiccg_press(im_press_l:im_press_u-1)
       mat_press%AU =>     aiccg_press(im_press_u:num_press_comp)
       mat_press%ALUG_L => ALUG_press_l
       mat_press%ALUG_U => ALUG_press_u
!
      end subroutine link_djds_mat_press_4_MG
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
      subroutine link_djds_mat_temp_4_MG(mat_temp)
!
      use m_temp_matrix
!
      type(DJDS_MATRIX), intent(inout) :: mat_temp
!
!
       mat_temp%num_diag =      numnod
       mat_temp%internal_diag = internal_node
       mat_temp%istart_diag =   im_temp_d
       mat_temp%istart_l =      im_temp_l
       mat_temp%istart_u =      im_temp_u
       mat_temp%num_non0 =      num_temp_comp
!
       mat_temp%aiccg =>  aiccg_temp
       mat_temp%D =>      aiccg_temp(im_temp_d:im_temp_l-1)
       mat_temp%AL =>     aiccg_temp(im_temp_l:im_temp_u-1)
       mat_temp%AU =>     aiccg_temp(im_temp_u:num_temp_comp)
       mat_temp%ALUG_L => ALUG_temp_l
       mat_temp%ALUG_U => ALUG_temp_u
!
      end subroutine link_djds_mat_temp_4_MG
!
!-----------------------------------------------------------------------
!
      subroutine link_djds_mat_dscalar_4_MG(mat_d_scalar)
!
      use m_light_element_matrix
!
      type(DJDS_MATRIX), intent(inout) :: mat_d_scalar
!
!
       mat_d_scalar%num_diag =      numnod
       mat_d_scalar%internal_diag = internal_node
       mat_d_scalar%istart_diag =   im_cps_d
       mat_d_scalar%istart_l =      im_cps_l
       mat_d_scalar%istart_u =      im_cps_u
       mat_d_scalar%num_non0 =      num_composit_comp
!
       mat_d_scalar%aiccg =>  aiccg_composit
       mat_d_scalar%D =>    aiccg_composit(im_cps_d:im_cps_l-1)
       mat_d_scalar%AL =>   aiccg_composit(im_cps_l:im_cps_u-1)
       mat_d_scalar%AU =>   aiccg_composit(im_cps_u:num_composit_comp)
       mat_d_scalar%ALUG_L => ALUG_composit_l
       mat_d_scalar%ALUG_U => ALUG_composit_u
!
      end subroutine link_djds_mat_dscalar_4_MG
!
!-----------------------------------------------------------------------
!
      end module  link_djds_mat_MHD_4_MG

