!matrices_precond_type.f90
!     module matrices_precond_type
!
!        programmed by H.Matsui and H.Okuda
!        modified by H. Matsui on Aug., 2007
!
!      subroutine s_matrices_precond_type(PRECOND_MG,                   &
!     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,      &
!     &         mat_velo, mat_magne, mat_temp, mat_d_scalar,            &
!     &          mat_press, mat_magp)
!        character(len=kchara),  intent(in) :: PRECOND_MG
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!        type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
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
     &          mat_velo, mat_magne, mat_temp, mat_d_scalar,            &
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
      type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!C
!C +-----------------+
!C | preconditioning |
!C +-----------------+
!C===
!
      if ( iflag_t_evo_4_velo .ge. 1 ) then
        call precond_DJDS11_struct(np_smp, djds_tbl_fl_l, mat_press,    &
     &     PRECOND_MG, sigma_diag)
      end if
!
      if ( iflag_t_evo_4_velo .ge. 3 ) then
        call precond_DJDS33_struct(np_smp, djds_tbl_fl, mat_velo,       &
     &      PRECOND_MG, sigma_diag)
      end if
!
      if ( iflag_t_evo_4_temp.ge.3 ) then
        call precond_DJDS11_struct(np_smp, djds_tbl_fl, mat_temp,       &
     &     PRECOND_MG, sigma_diag)
      end if
!
      if ( iflag_t_evo_4_composit.ge.3 ) then
        call precond_DJDS11_struct(np_smp, djds_tbl_fl, mat_d_scalar,   &
     &     PRECOND_MG, sigma_diag)
      end if
!
      if (iflag_t_evo_4_vect_p.ge.1 .or. iflag_t_evo_4_magne.ge.1) then
        call precond_DJDS11_struct(np_smp, djds_tbl_l, mat_magp,        &
     &     PRECOND_MG, sigma_diag)
      end if
!
      if (iflag_t_evo_4_vect_p.ge.1 .or. iflag_t_evo_4_magne.ge.1) then
        call precond_DJDS33_struct(np_smp, djds_tbl, mat_magne,         &
     &      PRECOND_MG, sigma_diag)
      end if
!
      end subroutine s_matrices_precond_type
!
!-----------------------------------------------------------------------
!
      end module matrices_precond_type
