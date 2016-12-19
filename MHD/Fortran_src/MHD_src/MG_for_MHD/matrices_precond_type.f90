!matrices_precond_type.f90
!     module matrices_precond_type
!
!        programmed by H.Matsui and H.Okuda
!        modified by H. Matsui on Aug., 2007
!
!      subroutine s_matrices_precond_type(PRECOND_MG,                   &
!     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,      &
!     &          mat_velo, mat_magne, mat_temp, mat_light,              &
!     &          mat_press, mat_magp)
!        character(len=kchara),  intent(in) :: PRECOND_MG
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!        type(DJDS_MATRIX),  intent(inout) :: mat_light
!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
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
      subroutine s_matrices_precond_type(PRECOND_MG,                    &
     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,       &
     &          mat_velo, mat_magne, mat_temp, mat_light,               &
     &          mat_press, mat_magp)
!
      use m_control_parameter
      use m_iccg_parameter
      use m_machine_parameter
      use t_solver_djds
!
      use solver_DJDS11_struct
      use solver_DJDS33_struct
!
      character(len=kchara),  intent(in) :: PRECOND_MG
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
      if (evo_velo%iflag_scheme .gt. id_no_evolution) then
        call precond_DJDS11_struct(np_smp, djds_tbl_fl_l, mat_press,    &
     &     PRECOND_MG, sigma_diag)
      end if
!
      if (evo_velo%iflag_scheme .ge. id_Crank_nicolson) then
        call precond_DJDS33_struct(np_smp, djds_tbl_fl, mat_velo,       &
     &      PRECOND_MG, sigma_diag)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call precond_DJDS11_struct(np_smp, djds_tbl_fl, mat_temp,       &
     &     PRECOND_MG, sigma_diag)
      end if
!
      if (evo_comp%iflag_scheme .ge. id_Crank_nicolson) then
        call precond_DJDS11_struct(np_smp, djds_tbl_fl, mat_light,      &
     &     PRECOND_MG, sigma_diag)
      end if
!
      if (evo_vect_p%iflag_scheme .gt. id_no_evolution                  &
     &      .or. evo_magne%iflag_scheme .gt. id_no_evolution) then
        call precond_DJDS11_struct(np_smp, djds_tbl_l, mat_magp,        &
     &     PRECOND_MG, sigma_diag)
      end if
!
      if (evo_vect_p%iflag_scheme .ge. id_Crank_nicolson                &
     &        .or. evo_magne%iflag_scheme .gt. id_no_evolution) then
        call precond_DJDS33_struct(np_smp, djds_tbl, mat_magne,         &
     &      PRECOND_MG, sigma_diag)
      end if
!
      end subroutine s_matrices_precond_type
!
!-----------------------------------------------------------------------
!
      end module matrices_precond_type
