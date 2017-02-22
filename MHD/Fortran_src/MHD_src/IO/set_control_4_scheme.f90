!>@file   set_control_4_scheme.f90
!!@brief  module set_control_4_scheme
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2002
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set schemes for time integration from control
!!
!!@verbatim
!!      subroutine set_control_4_FEM_params                             &
!!     &         (mevo_ctl, fint_ctl, evo_B, evo_A, fl_prop, FEM_prm)
!!        type(time_evolution_params), intent(in) :: evo_B, evo_A
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
!!        type(fem_intergration_control), intent(in)  :: fint_ctl
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!@endverbatim
!
      module set_control_4_scheme
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_FEM_params                               &
     &         (mevo_ctl, fint_ctl, evo_B, evo_A, fl_prop, FEM_prm)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use t_time_stepping_parameter
      use t_physical_property
      use t_FEM_control_parameter
      use t_ctl_data_mhd_evo_scheme
      use t_ctl_data_4_fem_int_pts
      use skip_comment_f
!
      type(time_evolution_params), intent(in) ::  evo_B, evo_A
      type(fluid_property), intent(in) :: fl_prop
      type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
      type(fem_intergration_control), intent(in)  :: fint_ctl
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!
      integer (kind=kint) :: iflag_4_supg = id_turn_OFF
!
!
        if (mevo_ctl%num_multi_pass_ctl%iflag .eq. 0) then
          FEM_prm%num_multi_pass = 1
        else
          FEM_prm%num_multi_pass = mevo_ctl%num_multi_pass_ctl%intvalue
        end if
!
        if (mevo_ctl%maxiter_ctl%iflag .eq. 0) then
          FEM_prm%maxiter_stokes = 0
        else
          FEM_prm%maxiter_stokes = mevo_ctl%maxiter_ctl%intvalue
        end if
        FEM_prm%maxiter_coulomb = FEM_prm%maxiter_stokes
!
        iflag_4_supg = id_turn_OFF
        if (mevo_ctl%iflag_supg_ctl%iflag .gt. 0                        &
     &     .and. yes_flag(mevo_ctl%iflag_supg_ctl%charavalue)) then
          iflag_4_supg = id_turn_ON
        end if
!
        FEM_prm%iflag_velo_supg = iflag_4_supg
        if (mevo_ctl%iflag_supg_v_ctl%iflag .gt. 0                      &
     &     .and. yes_flag(mevo_ctl%iflag_supg_v_ctl%charavalue)) then
          FEM_prm%iflag_velo_supg = id_turn_ON
        end if
!
        FEM_prm%iflag_temp_supg = iflag_4_supg
        if (mevo_ctl%iflag_supg_t_ctl%iflag .gt. 0                      &
     &    .and. yes_flag(mevo_ctl%iflag_supg_t_ctl%charavalue)) then
          FEM_prm%iflag_temp_supg = id_turn_ON
        end if
!
        FEM_prm%iflag_magne_supg =  iflag_4_supg
        if (mevo_ctl%iflag_supg_b_ctl%iflag .gt. 0                      &
     &    .and. yes_flag(mevo_ctl%iflag_supg_b_ctl%charavalue)) then
          FEM_prm%iflag_magne_supg = id_turn_ON
        end if
!
        FEM_prm%iflag_comp_supg = iflag_4_supg
        if (mevo_ctl%iflag_supg_c_ctl%iflag .gt. 0                      &
     &    .and.  yes_flag(mevo_ctl%iflag_supg_c_ctl%charavalue)) then
          FEM_prm%iflag_comp_supg = id_turn_ON
        end if
!
        if(FEM_prm%maxiter_stokes.gt.1) then
          if(fl_prop%iflag_scheme .gt. id_no_evolution) then
            if(mevo_ctl%eps_4_velo_ctl%iflag .eq. 0) then
              e_message                                                 &
     &         = 'Set convergence area for velocity iteration'
              call calypso_MPI_abort(ierr_CG, e_message)
            else
              FEM_prm%eps_4_stokes = mevo_ctl%eps_4_velo_ctl%realvalue
            end if
          end if
!
          if     (evo_B%iflag_scheme .gt. id_no_evolution               &
     &       .or. evo_A%iflag_scheme .gt. id_no_evolution) then
            if (mevo_ctl%eps_4_magne_ctl%iflag .eq. 0) then
              e_message                                                 &
     &         = 'Set convergence area for magnetic iteration'
              call calypso_MPI_abort(ierr_CG, e_message)
            else
              FEM_prm%eps_4_coulomb                                     &
     &          = mevo_ctl%eps_4_magne_ctl%realvalue
            end if
          end if
        end if
!
        if (iflag_debug .gt. iflag_routine_msg) then
          write(*,*) 'num_multi_pass  ', FEM_prm%num_multi_pass
          write(*,*) 'maxiter_stokes ',  FEM_prm%maxiter_stokes
          write(*,*) 'maxiter_coulomb ',    FEM_prm%maxiter_coulomb
          write(*,*) 'eps_4_velo:        ', FEM_prm%eps_4_stokes
          write(*,*) 'eps_4_magne:       ', FEM_prm%eps_4_coulomb
        end if
!
!  control for number of points for integration
!
        if (fint_ctl%intg_point_poisson_ctl%iflag .eq. 0) then
          e_message  = 'Set number of integration points for Poisson'
          call calypso_MPI_abort(ierr_FEM, e_message)
        else
          FEM_prm%npoint_poisson_int                                    &
     &        = fint_ctl%intg_point_poisson_ctl%intvalue
        end if
!
        if (fint_ctl%intg_point_t_evo_ctl%iflag .eq. 0) then
          e_message                                                     &
     &       = 'Set number of integration points for time integration'
          call calypso_MPI_abort(ierr_FEM, e_message)
        else
          FEM_prm%npoint_t_evo_int                                      &
     &       = fint_ctl%intg_point_t_evo_ctl%intvalue
        end if
!
        if (iflag_debug .gt. iflag_routine_msg) then
          write(*,*) 'intg_point_poisson ', FEM_prm%npoint_poisson_int
          write(*,*) 'intg_point_t_evo ',   FEM_prm%npoint_t_evo_int
        end if
!
      end subroutine set_control_4_FEM_params
!
! -----------------------------------------------------------------------
!
      end module set_control_4_scheme
