!>@file   precond_djds_MHD.f90
!!@brief  module precond_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in June, 2005
!
!>     Preconditiong of DJDS solver for MHD dynamo
!!
!!@verbatim
!!      subroutine init_MGCG_MHD(node, fl_prop, cd_prop)
!!      subroutine matrix_precondition(PRECOND_11, PRECOND_33,          &
!!     &          sigma_diag, fl_prop, cd_prop, ht_prop, cp_prop,       &
!!     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,     &
!!     &          mat_velo, mat_magne, mat_temp, mat_light,             &
!!     &          mat_press, mat_magp)
!!        character(len=kchara),  intent(in) :: PRECOND_11, PRECOND_33
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
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
!!@endverbatim
!
      module precond_djds_MHD
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_physical_property
      use t_solver_djds_MHD
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_MGCG_MHD(node, fl_prop, cd_prop)
!
      use m_iccg_parameter
      use solver_MGCG_MHD
!
      type(node_data), intent(in) :: node
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
!
!
      call init_MGCG11_MHD(node, method_4_solver, precond_4_solver)
!
      if(     fl_prop%iflag_scheme .ge. id_Crank_nicolson               &
     &   .or. cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson          &
     &   .or. cd_prop%iflag_Bevo_scheme .ge. id_Crank_nicolson) then
        call init_MGCG33_MHD(node, method_4_velo, precond_4_crank)
      end if
!
      end subroutine init_MGCG_MHD
!
! ----------------------------------------------------------------------
!
      subroutine matrix_precondition(PRECOND_11, PRECOND_33,            &
     &          sigma_diag, fl_prop, cd_prop, ht_prop, cp_prop,         &
     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,       &
     &          mat_velo, mat_magne, mat_temp, mat_light,               &
     &          mat_press, mat_magp)
!
      use t_solver_djds
!
      use solver_DJDS11_struct
      use solver_DJDS33_struct
!
      character(len=kchara),  intent(in) :: PRECOND_11, PRECOND_33
      real(kind = kreal), intent(in) :: sigma_diag
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
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
     &     PRECOND_11, sigma_diag)
      end if
!
      if (fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
        call precond_DJDS33_struct(np_smp, djds_tbl_fl, mat_velo,       &
     &      PRECOND_33, sigma_diag)
      end if
!
      if (ht_prop%iflag_scheme .ge. id_Crank_nicolson) then
        call precond_DJDS11_struct(np_smp, djds_tbl_fl, mat_temp,       &
     &     PRECOND_11, sigma_diag)
      end if
!
      if (cp_prop%iflag_scheme .ge. id_Crank_nicolson) then
        call precond_DJDS11_struct(np_smp, djds_tbl_fl, mat_light,      &
     &     PRECOND_11, sigma_diag)
      end if
!
      if    (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution             &
     &  .or. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call precond_DJDS11_struct(np_smp, djds_tbl_l, mat_magp,        &
     &     PRECOND_11, sigma_diag)
      end if
!
      if    (cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson           &
     &  .or. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call precond_DJDS33_struct(np_smp, djds_tbl, mat_magne,         &
     &      PRECOND_33, sigma_diag)
      end if
!
      end subroutine matrix_precondition
!
!-----------------------------------------------------------------------
!
      end module precond_djds_MHD
