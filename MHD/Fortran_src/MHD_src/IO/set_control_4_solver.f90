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
      use m_parallel_var_dof
      use m_iccg_parameter
      use m_ctl_data_4_solvers
      use m_ctl_parameter_Multigrid
!
!   control for solvers
!
        if ((i_method*i_precond) .eq. 0) then
          e_message                                                     &
     &    = 'Set CG method and preconditioning for Poisson solver'
            call parallel_abort(90, e_message)
        else
          precond_4_solver = precond_ctl
          method_4_solver  = method_ctl
        end if
!
        if (i_itr.eq.0) then
            e_message                                                   &
     &      = 'Set max iteration count for CG solver '
            call parallel_abort(90, e_message)
        else
          itr   = itr_ctl
        end if
!
        if (i_eps.eq.0) then
            e_message                                                   &
     &      = 'Set conservation limit for CG solver '
            call parallel_abort(90, e_message)
        else
          eps   = eps_ctl
        end if
!
        if (i_sigma.eq.0) then
            e_message                                                   &
     &      = 'Set coefficient of diagonal for SSOR preconditioning'
            call parallel_abort(90, e_message)
        else
          sigma = sigma_ctl
        end if
!
        if (i_sigma.eq.0) then
          sigma = 1.0d0
        else
          sigma = sigma_ctl
        end if
!
        if (i_sigma_diag.eq.0) then
          sigma_diag = 1.0d0
        else
          sigma_diag = sigma_diag_ctl
        end if
!
!   control for time evolution scheme
!
        if ( iflag_scheme.eq.3 .or. iflag_scheme.eq.4) then
!
          if ((i_method_4_velo*i_precond_4_crank) .eq. 0) then
            e_message                                                   &
     &      = 'Set CG method and preconditioning for implicit solver'
            call parallel_abort(90, e_message)
          else
            method_4_velo = method_4_velo_ctl
            precond_4_crank = precond_4_crank_ctl
          end if
!
          if (i_eps_crank .eq. 0) then
            e_message                                                   &
     &      = 'Set conservation area for implicit solver'
            call parallel_abort(90, e_message)
          else
            eps_crank  = eps_crank_ctl
          end if
!
        end if
!
!   control for number of processores for DJDS solver
!
        if (i_order_method.eq.0) then
              e_message                                                 &
     &         = 'Set ordering scheme for DJDS solver'
              call parallel_abort(90, e_message)
        else
          ordering_name = order_method_ctl
        end if
!
        if ( ordering_name .eq. 'RCM_DJDS') then 
          iflag_ordering = 1
          mc_color = 0
          if (i_min_color.eq.0) then
            min_color = 0
          else
            min_color = min_color_ctl
          end if
        else if  ( ordering_name .eq. 'MC_DJDS') then
          iflag_ordering = 2
          if (i_mc_color.eq.0) then
            mc_color = 0
          else
            mc_color = mc_color_ctl
          end if
          min_color = mc_color_ctl
        end if
!
        if (       precond_4_solver .eq. 'DIAG'                         &
     &       .and. iflag_ordering .eq. 2                                &
     &       .and. mc_color .eq. 0 ) then
          if ( iflag_scheme .le. 2) then
            iflag_ordering = 0
          else if (precond_4_crank .eq. 'DIAG') then
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
          write(*,*) 'min_color:       ', min_color
          write(*,*) 'mc_color:        ', mc_color
          write(*,*) 'eps_4_velo:      ', eps_4_velo
          write(*,*) 'eps_4_magne:     ', eps_4_magne
          write(*,*) 'eps_4_crank:     ', eps_crank
          write(*,*) 'method_4_velo:   ', trim(method_4_velo)
          write(*,*) 'precond_4_crank: ', trim(precond_4_crank)
        end if
!
      if (     ((method_4_solver(1:1).eq.'M')                           &
     &      .or.(method_4_solver(1:1).eq.'m'))                          &
     &   .and. ((method_4_solver(2:2).eq.'G')                           &
     &      .or.(method_4_solver(2:2).eq.'g'))                          &
     &   .and. ((method_4_solver(3:3).eq.'C')                           &
     &      .or.(method_4_solver(3:3).eq.'c'))                          &
     &   .and. ((method_4_solver(4:4).eq.'G')                           &
     &      .or.(method_4_solver(4:4).eq.'g')) ) then
        if (iflag_debug.eq.1) write(*,*) 'set_ctl_data_4_Multigrid'
        call set_ctl_data_4_Multigrid
      end if
!
      end subroutine s_set_control_4_solver
!
! -----------------------------------------------------------------------
!
      end module set_control_4_solver
