!>@file   set_control_4_solver.f90
!!@brief  module set_control_4_solver
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Okuda in 2000
!!@n    modified by H. Matsui in 2001
!!@n    modified by H. Matsui in Aug., 2007
!
!> @brief set parameters for linear solver for MHD simulation
!!        from control data
!!
!!@verbatim
!!      subroutine s_set_control_4_solver                               &
!!     &         (iflag_scheme, mevo_ctl, CG_ctl, FEM_prm,              &
!!     &          MGCG_WK, MGCG_FEM, MGCG_MHD_FEM)
!!        type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
!!        type(solver_control), intent(inout) :: CG_ctl
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!        type(DJDS_poarameter), intent(inout) :: DJDS_param
!!        type(MGCG_data), intent(inout) ::   MGCG_WK
!!        type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
!!        type(MGCG_MHD_data), intent(inout) :: MGCG_MHD_FEM
!!@endverbatim
!
      module set_control_4_solver
!
      use m_precision
!
      use m_machine_parameter
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_solver                                 &
     &         (iflag_scheme, mevo_ctl, CG_ctl, FEM_prm,                &
     &          MGCG_WK, MGCG_FEM, MGCG_MHD_FEM)
!
      use calypso_mpi
      use m_error_IDs
      use t_iccg_parameter
      use t_FEM_control_parameter
      use t_physical_property
      use t_ctl_data_4_solvers
      use t_ctl_data_mhd_evo_scheme
      use t_MGCG_data
      use t_MGCG_data_4_MHD
      use skip_comment_f
!
      integer (kind=kint), intent(in) :: iflag_scheme
      type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
      type(solver_control), intent(inout) :: CG_ctl
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(MGCG_data), intent(inout) ::   MGCG_WK
      type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
      type(MGCG_MHD_data), intent(inout) :: MGCG_MHD_FEM
!
!
!   control for solvers
      call set_control_4_CG_solver(CG_ctl, FEM_PRM%CG11_param)
!
!   control for number of processores for DJDS solver
      call set_control_4_DJDS_solver                                    &
     &   (CG_ctl%DJDS_ctl, FEM_prm%DJDS_param)
!
!
!   control for time evolution scheme
!
        if ( iflag_scheme .eq. id_Crank_nicolson                        &
     &     .or. iflag_scheme .eq. id_Crank_nicolson) then
!
          if((mevo_ctl%method_4_CN%iflag*mevo_ctl%precond_4_CN%iflag)   &
     &      .eq. 0) then
            e_message                                                   &
     &      = 'Set CG method and preconditioning for implicit solver'
            call calypso_MPI_abort(ierr_CG, e_message)
          else
            FEM_PRM%method_33 =  mevo_ctl%method_4_CN%charavalue
            FEM_PRM%precond_33 = mevo_ctl%precond_4_CN%charavalue
          end if
!
          if (mevo_ctl%eps_crank_ctl%iflag .eq. 0) then
            e_message                                                   &
     &      = 'Set convergence area for implicit solver'
            call calypso_MPI_abort(ierr_CG, e_message)
          else
            FEM_PRM%eps_crank  = mevo_ctl%eps_crank_ctl%realvalue
          end if
!
          if(mevo_ctl%eps_B_crank_ctl%iflag .gt. 0) then
            FEM_prm%eps_4_magne_crank                                   &
     &               = mevo_ctl%eps_B_crank_ctl%realvalue
          end if
        end if
!
!
        if (    FEM_PRM%CG11_param%PRECOND .eq. 'DIAG'                  &
     &    .and. FEM_prm%DJDS_param%iflag_ordering .eq. iflag_MultiColor &
     &    .and. FEM_prm%DJDS_param%mc_color .eq. 0 ) then
          if(FEM_PRM%precond_33 .eq. 'DIAG') then
            FEM_prm%DJDS_param%iflag_ordering = iflag_OFF
          end if
        end if
!
        if (iflag_debug.eq.1) then
          write(*,*) 'eps_4_crank:       ', FEM_PRM%eps_crank
          write(*,*) 'eps_4_magne_crank: ', FEM_prm%eps_4_magne_crank
          write(*,*) 'method_4_velo:     ', trim(FEM_PRM%method_33)
          write(*,*) 'precond_4_crank:   ', trim(FEM_PRM%precond_33)
        end if
!
      if (cmp_no_case(FEM_PRM%CG11_param%METHOD, 'MGCG')) then
        if (iflag_debug.eq.1) write(*,*) 'set_ctl_data_4_Multigrid'
        call set_ctl_data_4_Multigrid                                   &
     &     (CG_ctl%MG_ctl, FEM_PRM%MG_param, FEM_PRM%MG_file,           &
     &      MGCG_WK, MGCG_FEM)
        call alloc_MGCG_MHD_data(MGCG_WK, MGCG_MHD_FEM)
      end if
!
      end subroutine s_set_control_4_solver
!
! -----------------------------------------------------------------------
!
      end module set_control_4_solver
