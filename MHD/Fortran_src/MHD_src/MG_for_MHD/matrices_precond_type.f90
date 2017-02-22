!matrices_precond_type.f90
!     module matrices_precond_type
!
!        programmed by H.Matsui and H.Okuda
!        modified by H. Matsui on Aug., 2007
!
!!      subroutine s_matrices_precond_type                              &
!!     &         (PRECOND_MG, evo_B, evo_A, evo_T, evo_C, fl_prop,      &
!!     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,     &
!!     &          mat_velo, mat_magne, mat_temp, mat_light,             &
!!     &          mat_press, mat_magp)
!!        character(len=kchara),  intent(in) :: PRECOND_MG
!!        type(time_evolution_params), intent(in) :: evo_B, evo_A
!!        type(time_evolution_params), intent(in) :: evo_T, evo_C
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_light
!!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
      module matrices_precond_type
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
      subroutine s_matrices_precond_type                                &
     &         (PRECOND_MG, evo_B, evo_A, evo_T, evo_C, fl_prop,        &
     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,       &
     &          mat_velo, mat_magne, mat_temp, mat_light,               &
     &          mat_press, mat_magp)
!
      use m_iccg_parameter
      use m_machine_parameter
      use t_time_stepping_parameter
      use t_physical_property
      use t_solver_djds
!
      use solver_DJDS11_struct
      use solver_DJDS33_struct
!
      character(len=kchara),  intent(in) :: PRECOND_MG
      type(time_evolution_params), intent(in) :: evo_B, evo_A
      type(time_evolution_params), intent(in) :: evo_T, evo_C
      type(fluid_property), intent(in) :: fl_prop
      type(DJDS_ordering_table),  intent(in) :: djds_tbl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!C
!C +-----------------+
!C | preconditioning |
!C +-----------------+
!C===
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        call precond_DJDS11_struct(np_smp, djds_tbl_fl_l, mat_press,    &
     &     PRECOND_MG, sigma_diag)
      end if
!
      if (fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
        call precond_DJDS33_struct(np_smp, djds_tbl_fl, mat_velo,       &
     &      PRECOND_MG, sigma_diag)
      end if
!
      if (evo_T%iflag_scheme .ge. id_Crank_nicolson) then
        call precond_DJDS11_struct(np_smp, djds_tbl_fl, mat_temp,       &
     &     PRECOND_MG, sigma_diag)
      end if
!
      if (evo_C%iflag_scheme .ge. id_Crank_nicolson) then
        call precond_DJDS11_struct(np_smp, djds_tbl_fl, mat_light,      &
     &     PRECOND_MG, sigma_diag)
      end if
!
      if    (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution             &
     &  .or. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call precond_DJDS11_struct(np_smp, djds_tbl_l, mat_magp,        &
     &     PRECOND_MG, sigma_diag)
      end if
!
      if    (cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson           &
     &  .or. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call precond_DJDS33_struct(np_smp, djds_tbl, mat_magne,         &
     &      PRECOND_MG, sigma_diag)
      end if
!
      end subroutine s_matrices_precond_type
!
!-----------------------------------------------------------------------
!
      end module matrices_precond_type
