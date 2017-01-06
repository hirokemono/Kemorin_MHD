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
!!     subroutine s_set_control_4_solver
!!@endverbatim
!
      module set_control_4_solver
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_ctl_data_mhd_evo_scheme
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_solver
!
      use calypso_mpi
      use m_error_IDs
      use m_iccg_parameter
      use m_ctl_data_4_solvers
      use m_ctl_parameter_Multigrid
      use skip_comment_f
!
!   control for solvers
!
        if ((method_ctl%iflag * precond_ctl%iflag) .eq. 0) then
          e_message                                                     &
     &    = 'Set CG method and preconditioning for Poisson solver'
            call calypso_MPI_abort(ierr_CG, e_message)
        else
          precond_4_solver = precond_ctl%charavalue
          method_4_solver  = method_ctl%charavalue
        end if
!
        if (itr_ctl%iflag .eq. 0) then
            e_message                                                   &
     &      = 'Set max iteration count for CG solver '
            call calypso_MPI_abort(ierr_CG, e_message)
        else
          itr   = itr_ctl%intvalue
        end if
!
        if (eps_ctl%iflag .eq. 0) then
            e_message                                                   &
     &      = 'Set conservation limit for CG solver '
            call calypso_MPI_abort(ierr_CG, e_message)
        else
          eps   = eps_ctl%realvalue
        end if
!
        if (sigma_ctl%iflag .eq. 0) then
            e_message                                                   &
     &      = 'Set coefficient of diagonal for SSOR preconditioning'
            call calypso_MPI_abort(ierr_CG, e_message)
        else
          sigma = sigma_ctl%realvalue
        end if
!
        if (sigma_ctl%iflag .eq. 0) then
          sigma = 1.0d0
        else
          sigma = sigma_ctl%realvalue
        end if
!
        if (sigma_diag_ctl%iflag .eq. 0) then
          sigma_diag = 1.0d0
        else
          sigma_diag = sigma_diag_ctl%realvalue
        end if
!
!   control for time evolution scheme
!
        if ( iflag_scheme .eq. id_Crank_nicolson                        &
     &     .or. iflag_scheme .eq. id_Crank_nicolson) then
!
          if ((method_4_velo_ctl%iflag*precond_4_crank_ctl%iflag)       &
     &      .eq. 0) then
            e_message                                                   &
     &      = 'Set CG method and preconditioning for implicit solver'
            call calypso_MPI_abort(ierr_CG, e_message)
          else
            method_4_velo =   method_4_velo_ctl%charavalue
            precond_4_crank = precond_4_crank_ctl%charavalue
          end if
!
          if (eps_crank_ctl%iflag .eq. 0) then
            e_message                                                   &
     &      = 'Set convergence area for implicit solver'
            call calypso_MPI_abort(ierr_CG, e_message)
          else
            eps_crank  = eps_crank_ctl%realvalue
          end if
!
          if(eps_B_crank_ctl%iflag .gt. 0) then
            eps_4_magne_crank = eps_B_crank_ctl%realvalue
          end if
        end if
!
!   control for number of processores for DJDS solver
!
        call set_control_4_DJDS_solver(DJDS_ctl1)
!
        if (       precond_4_solver .eq. 'DIAG'                         &
     &       .and. iflag_ordering .eq. 2                                &
     &       .and. mc_color .eq. 0 ) then
          if(precond_4_crank .eq. 'DIAG') then
            iflag_ordering = 0
          end if
        end if
!
        if (iflag_debug.eq.1) then
          write(*,*) 'itr:        ', itr
          write(*,*) 'eps:        ', eps
          write(*,*) 'sigma:      ', sigma
          write(*,*) 'sigma_diag: ', sigma_diag
          write(*,*) 'precond_4_solver: ',  trim(precond_4_solver)
          write(*,*) 'method_4_solver:  ',  trim(method_4_solver)
          write(*,*) 'ordering_name: , iflag_ordering ',                &
     &                trim(ordering_name), iflag_ordering
          write(*,*) 'eps_4_velo:        ', eps_4_velo
          write(*,*) 'eps_4_magne:       ', eps_4_magne
          write(*,*) 'eps_4_crank:       ', eps_crank
          write(*,*) 'eps_4_magne_crank: ', eps_4_magne_crank
          write(*,*) 'method_4_velo:     ', trim(method_4_velo)
          write(*,*) 'precond_4_crank:   ', trim(precond_4_crank)
        end if
!
      if (cmp_no_case(method_4_solver, 'MGCG')) then
        if (iflag_debug.eq.1) write(*,*) 'set_ctl_data_4_Multigrid'
        call set_ctl_data_4_Multigrid(MG_ctl1)
      end if
!
      end subroutine s_set_control_4_solver
!
! -----------------------------------------------------------------------
!
      end module set_control_4_solver
