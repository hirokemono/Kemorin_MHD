!
!      module set_control_4_scheme
!
!        programmed by H.Matsui
!        modified by H.Matsui on Aug., 2007
!
!     subroutine set_control_4_FEM_params
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
      use m_machine_parameter
      use m_parallel_var_dof
      use m_control_parameter
      use m_ctl_data_4_fem_int_pts
      use m_ctl_data_mhd_evo_scheme
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
        if (i_iflag_supg.eq.0) then
          iflag_4_supg = 0
        else
          iflag_4_supg = iflag_supg_ctl
        end if
!
        if (maxiter.gt.1) then
          if (iflag_t_evo_4_velo.gt.0) then
            if (i_eps_4_velo.eq.0) then
              e_message                                                 &
     &         = 'Set conservation area for velocity iteration'
              call parallel_abort(90, e_message)
            else
              eps_4_velo = eps_4_velo_ctl
            end if
          end if
!
          if (iflag_t_evo_4_magne.gt.0                                  &
     &              .or. iflag_t_evo_4_vect_p.gt.0) then
            if (i_eps_4_magne.eq.0) then
              e_message                                                 &
     &         = 'Set conservation area for magnetic iteration'
              call parallel_abort(90, e_message)
            else
              eps_4_magne = eps_4_magne_ctl
            end if
          end if
        end if
!
        if (iflag_debug.eq.1) then
         write(12,*) 'num_multi_pass  ',num_multi_pass
         write(12,*) 'maxiter ',maxiter
         write(12,*) 'maxiter_vecp ',maxiter_vecp
        end if
!
!  control for number of points for integration
!
        if (i_intg_point_poisson.eq.0.0d0) then
          e_message  = 'Set number of integration points for Poisson'
          call parallel_abort(90, e_message)
        else
          intg_point_poisson =  intg_point_poisson_ctl
        end if
!
        if (i_intg_point_t_evo.eq.0.0d0) then
          e_message                                                     &
     &       = 'Set number of integration points for time integration'
          call parallel_abort(90, e_message)
        else
          intg_point_t_evo =    intg_point_t_evo_ctl
        end if
!
        if (iflag_debug.eq.1) then
         write(12,*) 'intg_point_poisson ',intg_point_poisson
         write(12,*) 'intg_point_t_evo ',intg_point_t_evo
        end if
!
      end subroutine set_control_4_FEM_params
!
! -----------------------------------------------------------------------
!
      end module set_control_4_scheme
