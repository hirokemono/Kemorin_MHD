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
!!     subroutine set_control_4_FEM_params
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
      subroutine set_control_4_FEM_params
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_control_parameter
      use m_ctl_data_4_fem_int_pts
      use m_ctl_data_mhd_evo_scheme
!
      integer (kind=kint) :: iflag_4_supg = id_turn_OFF
!
!
        if (i_num_multi_pass.eq.0) then
          num_multi_pass = 1
        else
          num_multi_pass = num_multi_pass_ctl
        end if
!
        if (i_maxiter.eq.0) then
          maxiter = 0
        else
          maxiter = maxiter_ctl
        end if
        maxiter_vecp = maxiter
!
        iflag_4_supg = id_turn_OFF
        if (i_iflag_supg .eq. 0) then
          if(   iflag_supg_ctl .eq. 'ON'                              &
     &     .or. iflag_supg_ctl .eq. 'On'                              &
     &     .or. iflag_supg_ctl .eq. 'on'                              &
     &     .or. iflag_supg_ctl .eq. '1')   iflag_4_supg = id_turn_ON
        end if
!
        iflag_velo_supg = iflag_4_supg
        iflag_temp_supg = iflag_4_supg
        iflag_mag_supg =  iflag_4_supg
        iflag_comp_supg = iflag_4_supg
        if (i_iflag_v_supg .eq. 0) then
          if(   iflag_supg_v_ctl .eq. 'ON'                              &
     &     .or. iflag_supg_v_ctl .eq. 'On'                              &
     &     .or. iflag_supg_v_ctl .eq. 'on'                              &
     &     .or. iflag_supg_v_ctl .eq. '1') iflag_velo_supg = id_turn_ON
        end if
        if (i_iflag_t_supg .eq. 0) then
          if(   iflag_supg_t_ctl .eq. 'ON'                              &
     &     .or. iflag_supg_t_ctl .eq. 'On'                              &
     &     .or. iflag_supg_t_ctl .eq. 'on'                              &
     &     .or. iflag_supg_t_ctl .eq. '1') iflag_temp_supg = id_turn_ON
        end if
        if (i_iflag_b_supg .eq. 0) then
          if(   iflag_supg_b_ctl .eq. 'ON'                              &
     &     .or. iflag_supg_b_ctl .eq. 'On'                              &
     &     .or. iflag_supg_b_ctl .eq. 'on'                              &
     &     .or. iflag_supg_b_ctl .eq. '1') iflag_mag_supg = id_turn_ON
        end if
        if (i_iflag_c_supg .eq. 0) then
          if(   iflag_supg_c_ctl .eq. 'ON'                              &
     &     .or. iflag_supg_c_ctl .eq. 'On'                              &
     &     .or. iflag_supg_c_ctl .eq. 'on'                              &
     &     .or. iflag_supg_c_ctl .eq. '1') iflag_comp_supg = id_turn_ON
        end if
!
        if (maxiter.gt.1) then
          if (iflag_t_evo_4_velo .gt. id_no_evolution) then
            if (i_eps_4_velo.eq.0) then
              e_message                                                 &
     &         = 'Set convergence area for velocity iteration'
              call calypso_MPI_abort(ierr_CG, e_message)
            else
              eps_4_velo = eps_4_velo_ctl
            end if
          end if
!
          if (iflag_t_evo_4_magne .gt. id_no_evolution                  &
     &            .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
            if (i_eps_4_magne.eq.0) then
              e_message                                                 &
     &         = 'Set convergence area for magnetic iteration'
              call calypso_MPI_abort(ierr_CG, e_message)
            else
              eps_4_magne = eps_4_magne_ctl
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
        if (intg_point_poisson_ctl%iflag .eq. 0) then
          e_message  = 'Set number of integration points for Poisson'
          call calypso_MPI_abort(ierr_FEM, e_message)
        else
          intg_point_poisson =  intg_point_poisson_ctl%intvalue
        end if
!
        if (intg_point_t_evo_ctl%iflag .eq. 0) then
          e_message                                                     &
     &       = 'Set number of integration points for time integration'
          call calypso_MPI_abort(ierr_FEM, e_message)
        else
          intg_point_t_evo =    intg_point_t_evo_ctl%intvalue
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
