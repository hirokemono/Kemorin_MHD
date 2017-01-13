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
!!     subroutine set_control_4_FEM_params(mevo_ctl, fint_ctl)
!!        type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
!!        type(fem_intergration_control), intent(in)  :: fint_ctl
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
      subroutine set_control_4_FEM_params(mevo_ctl, fint_ctl)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_control_parameter
      use t_ctl_data_mhd_evo_scheme
      use t_ctl_data_4_fem_int_pts
      use skip_comment_f
!
      type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
      type(fem_intergration_control), intent(in)  :: fint_ctl
!
      integer (kind=kint) :: iflag_4_supg = id_turn_OFF
!
!
        if (mevo_ctl%num_multi_pass_ctl%iflag .eq. 0) then
          num_multi_pass = 1
        else
          num_multi_pass = mevo_ctl%num_multi_pass_ctl%intvalue
        end if
!
        if (mevo_ctl%maxiter_ctl%iflag .eq. 0) then
          maxiter = 0
        else
          maxiter = mevo_ctl%maxiter_ctl%intvalue
        end if
        maxiter_vecp = maxiter
!
        iflag_4_supg = id_turn_OFF
        if (mevo_ctl%iflag_supg_ctl%iflag .gt. 0                        &
     &     .and. yes_flag(mevo_ctl%iflag_supg_ctl%charavalue)) then
          iflag_4_supg = id_turn_ON
        end if
!
        iflag_velo_supg = iflag_4_supg
        iflag_temp_supg = iflag_4_supg
        iflag_mag_supg =  iflag_4_supg
        iflag_comp_supg = iflag_4_supg
        if (mevo_ctl%iflag_supg_v_ctl%iflag .gt. 0                      &
     &     .and. yes_flag(mevo_ctl%iflag_supg_v_ctl%charavalue)) then
          iflag_velo_supg = id_turn_ON
        end if
        if (mevo_ctl%iflag_supg_t_ctl%iflag .gt. 0                      &
     &    .and. yes_flag(mevo_ctl%iflag_supg_t_ctl%charavalue)) then
          iflag_temp_supg = id_turn_ON
        end if
        if (mevo_ctl%iflag_supg_b_ctl%iflag .gt. 0                      &
     &    .and. yes_flag(mevo_ctl%iflag_supg_b_ctl%charavalue)) then
          iflag_mag_supg = id_turn_ON
        end if
        if (mevo_ctl%iflag_supg_c_ctl%iflag .gt. 0                      &
     &    .and.  yes_flag(mevo_ctl%iflag_supg_c_ctl%charavalue)) then
          iflag_comp_supg = id_turn_ON
        end if
!
        if (maxiter.gt.1) then
          if (evo_velo%iflag_scheme .gt. id_no_evolution) then
            if (mevo_ctl%eps_4_velo_ctl%iflag .eq. 0) then
              e_message                                                 &
     &         = 'Set convergence area for velocity iteration'
              call calypso_MPI_abort(ierr_CG, e_message)
            else
              eps_4_velo = mevo_ctl%eps_4_velo_ctl%realvalue
            end if
          end if
!
          if (evo_magne%iflag_scheme .gt. id_no_evolution               &
     &        .or. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
            if (mevo_ctl%eps_4_magne_ctl%iflag .eq. 0) then
              e_message                                                 &
     &         = 'Set convergence area for magnetic iteration'
              call calypso_MPI_abort(ierr_CG, e_message)
            else
              eps_4_magne = mevo_ctl%eps_4_magne_ctl%realvalue
            end if
          end if
        end if
!
        if (iflag_debug .gt. iflag_routine_msg) then
          write(*,*) 'num_multi_pass  ',num_multi_pass
          write(*,*) 'maxiter ',        maxiter
          write(*,*) 'maxiter_vecp ',   maxiter_vecp
        end if
!
!  control for number of points for integration
!
        if (fint_ctl%intg_point_poisson_ctl%iflag .eq. 0) then
          e_message  = 'Set number of integration points for Poisson'
          call calypso_MPI_abort(ierr_FEM, e_message)
        else
          intg_point_poisson                                            &
     &        = fint_ctl%intg_point_poisson_ctl%intvalue
        end if
!
        if (fint_ctl%intg_point_t_evo_ctl%iflag .eq. 0) then
          e_message                                                     &
     &       = 'Set number of integration points for time integration'
          call calypso_MPI_abort(ierr_FEM, e_message)
        else
          intg_point_t_evo = fint_ctl%intg_point_t_evo_ctl%intvalue
        end if
!
        if (iflag_debug .gt. iflag_routine_msg) then
          write(*,*) 'intg_point_poisson ', intg_point_poisson
          write(*,*) 'intg_point_t_evo ',   intg_point_t_evo
        end if
!
      end subroutine set_control_4_FEM_params
!
! -----------------------------------------------------------------------
!
      end module set_control_4_scheme
