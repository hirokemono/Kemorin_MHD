!
!     module material_property
!
!      Written by H.Matsui
!      Moified by H. Matsui on Sep., 2007
!
!      subroutine set_material_property
!
!
!  This subroutine is for set coefficients of diffusion and forces.
!       ak_d_temp is coefficient for thermal diffusion,
!       ak_d_velo is for kinetic viscousity,  and 
!       ak_buo are coefficients for forces.
!     Please edit this routine to apply to your model.
!
!  This subroutine supports three types of normalization \\
! \begin{itemize}
!  \item Forced convection ( normalize_control = 0 )
!   \begin{eqnarray}
!      \partial_{t} v_{i} + v_{j} v_{i,j}
!                &=& - P_{,i} + \frac{1}{R_{e}} v_{i,jj} \\
!      \partial_{t} T + v_{j} T_{,j}
!                &=& \frac{1}{P_{r}R_{e}} T_{i,jj} .\\
!  \end{eqnarray}
!     where, $ R_{e}$ and $P_{r}$ are the Reynolds number and 
!     Prandtl number, respectively.
!     In this model, we set dimless(1) = $ R_{e} $
!     and dimless(2) = $ P_{r} $.
!
!  \item Thermal convection ( normalize_control = 1 )
!   \begin{eqnarray}
!      \partial_{t} v_{i} + v_{j} v_{i,j}
!                &=& - P_{,i} + \frac{1}{P_{r}G_{r}} v_{i,jj} \\
!      \partial_{t} T + v_{j} T_{,j}
!                &=& \frac{1}{R_{a}} T_{i,jj} .\\
!  \end{eqnarray}
!     where, $ R_{a}$ and $P_{r}$ are the Rayleigh number and 
!     Prandtl number, respectively.
!     In this model, we set dimless(1) = $ R_{a} $
!     and dimless(2) = $ P_{r} $.
!
!  \item Thermal convection with Coriolis force
!                  ( normalize_control = -1 )
!   \begin{eqnarray}
!      \partial_{t} v_{i} + v_{j} v_{i,j}
!                &=& - P_{,i} + P_{r} v_{i,jj}
!                    - P_{r}\sqrt{T_{a}} e_{ijk} \Omega_{j} v_{k}
!                    - P_{r} R_{a} \left( T - T_{0} \right) \hat{g} \\
!      \partial_{t} T + v_{j} T_{,j}
!                &=& T_{i,jj} .\\
!  \end{eqnarray}
!   where, $\Omega_{i}$ is the angular velocity vector; 
!   and $ R_{a}$, $P_{r}$, $T_{a}$ are the Rayleigh, Prandtl,
!     and Taylor numbers, respectively.
!     In this model, we set dimless(1) = $ R_{a} $
!     and dimless(2) = $ P_{r} $.
!     When you carry out simulations where only the Coriolis force
!     works the fluid,  please set the Rayleigh number
!      ( =dimless(2) ) to be 0.
!  \end{itemize}
!  \item Thermal convection with Coriolis force for dynamo Benchmark
!                  ( normalize_control = -2 )
!   \begin{eqnarray}
!      E_{k} [ \partial_{t} v_{i} + v_{j} v_{i,j} - v_{i,jj} ]
!                &=& - P_{,i} -  e_{ijk} \Omega_{j} v_{k}
!                    - \tilde{R}_{a} \left( T - T_{0} \right) r/r_{0} \\
!      \partial_{t} T + v_{j} T_{,j}
!                &=& P_{r}^{-1} T_{i,jj} .\\
!  \end{eqnarray}
!   where, $\Omega_{i}$ is the angular velocity vector; 
!   and $ R_{a}$, $P_{r}$, $T_{a}$ are the Rayleigh, Prandtl,
!     and Taylor numbers, respectively.
!     In this model, we set dimless(1) = $ R_{a} $
!     and dimless(2) = $ P_{r} $.
!     When you carry out simulations where only the Coriolis force
!     works the fluid,  please set the Rayleigh number
!      ( =dimless(2) ) to be 0.
!  \end{itemize}
!\endDESC
!
      module material_property
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_material_property
!
      use m_parallel_var_dof
      use m_control_parameter
      use m_normalize_parameter
      use m_geometry_parameter
      use m_node_phys_address
      use m_physical_property
      use m_t_int_parameter
      use construct_MHD_coefficient
!
!    For thermal
!
!
      if (iflag_t_evo_4_temp.ge.1) then
!
        coef_temp =   one
        coef_d_temp = one
!
        call construct_coefficient(coef_temp, num_dimless, dimless,     &
     &      name_dimless, num_coef_4_termal, coef_4_termal_name,        &
     &      coef_4_termal_power, depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_d_temp, num_dimless, dimless,   &
     &      name_dimless, num_coef_4_t_diffuse, coef_4_t_diffuse_name,  &
     &      coef_4_t_diffuse_power, depth_low_t, depth_high_t)
!
        call set_implicit_4_inf_viscous(coef_temp,                      &
     &      coef_imp_t, coef_exp_t)
!
        coef_nega_t = - coef_temp
      end if
!
!    For convection
!
      if (iflag_t_evo_4_velo.ge.1) then
!
        coef_velo =     one
        coef_d_velo =   one
        coef_buo =      one
        coef_comp_buo = one
        coef_cor =      one
        coef_lor =      one
        coef_press =    one
        acoef_press =   one
!
        call construct_coefficient(coef_velo, num_dimless, dimless,     &
     &      name_dimless, num_coef_4_velocity, coef_4_velocity_name,    &
     &      coef_4_velocity_power, depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_press, num_dimless, dimless,    &
     &      name_dimless, num_coef_4_press, coef_4_press_name,          &
     &      coef_4_press_power, depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_d_velo, num_dimless, dimless,   &
     &      name_dimless, num_coef_4_v_diffuse, coef_4_v_diffuse_name,  &
     &      coef_4_v_diffuse_power, depth_low_t, depth_high_t)
!
        call set_implicit_4_inf_viscous(coef_velo,                      &
     &      coef_imp_v, coef_exp_v)
!
        acoef_press = one / coef_press
        coef_nega_v = - coef_velo
!
        if (iflag_4_gravity.gt.0 .or. iflag_4_filter_gravity.gt.0) then
          call construct_coefficient(coef_buo, num_dimless, dimless,    &
     &       name_dimless, num_coef_4_buoyancy, coef_4_buoyancy_name,   &
     &       coef_4_buoyancy_power, depth_low_t, depth_high_t)
        end if
!
        if (iflag_4_composit_buo .gt. 0) then
          call construct_coefficient(coef_comp_buo, num_dimless,        &
     &       dimless, name_dimless, num_coef_4_comp_buo,                &
     &       coef_4_comp_buo_name, coef_4_comp_buo_power,               &
     &       depth_low_t, depth_high_t)
        end if
!
        if (iflag_4_coriolis .gt. 0) then
          call construct_coefficient(coef_cor, num_dimless, dimless,    &
     &       name_dimless, num_coef_4_Coriolis, coef_4_Coriolis_name,   &
     &       coef_4_Coriolis_power, depth_low_t, depth_high_t)
        end if
!
        if ( iflag_4_lorentz.ge.1 ) then
          call construct_coefficient(coef_lor, num_dimless, dimless,    &
     &       name_dimless, num_coef_4_Lorentz, coef_4_Lorentz_name,     &
     &       coef_4_Lorentz_power, depth_low_t, depth_high_t)
        end if
!
      end if
!
!   For Induction
!
      if (iflag_t_evo_4_magne.ge.1 .or. iflag_t_evo_4_vect_p.ge.1) then
!
        coef_magne =   one
        coef_mag_p =   one
        coef_d_magne = one
        coef_induct =  one
!
        call construct_coefficient(coef_magne, num_dimless, dimless,    &
     &     name_dimless, num_coef_4_magnetic, coef_4_magnetic_name,     &
     &     coef_4_magnetic_power, depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_mag_p, num_dimless, dimless,    &
     &     name_dimless, num_coef_4_mag_p, coef_4_mag_p_name,           &
     &     coef_4_mag_p_power, depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_d_magne, num_dimless, dimless,  &
     &     name_dimless, num_coef_4_m_diffuse, coef_4_m_diffuse_name,   &
     &     coef_4_m_diffuse_power, depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_induct, num_dimless, dimless,   &
     &     name_dimless, num_coef_4_induction, coef_4_induction_name,   &
     &     coef_4_induction_power, depth_low_t, depth_high_t)
!
        call set_implicit_4_inf_viscous(coef_magne,                     &
     &      coef_imp_b, coef_exp_b)
!
      end if
!
!   For light element
!
      if (iflag_t_evo_4_composit.ge.1) then
        coef_scalar =   one
        coef_d_light = one
!
        call construct_coefficient(coef_scalar, num_dimless, dimless,   &
     &      name_dimless, num_coef_4_composition, coef_4_composit_name, &
     &      coef_4_composit_power, depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_d_light, num_dimless, dimless,  &
     &      name_dimless, num_coef_4_c_diffuse, coef_4_c_diffuse_name,  &
     &      coef_4_c_diffuse_power, depth_low_t, depth_high_t)
!
        call set_implicit_4_inf_viscous(coef_scalar,                    &
     &      coef_imp_c, coef_exp_c)
!
        coef_nega_c = - coef_scalar
      end if
!
!  verification
!
      if (my_rank .eq. 0) then
       write(*,*)' coefficient for velocity:            ',coef_velo
       write(*,*)' coefficient for pressure:            ',coef_press
       write(*,*)' coefficient for viscous diffusion:   ',coef_d_velo
       write(*,*)' coefficient for buoyancy:            ',coef_buo
       write(*,*)' coefficient for composit buoyancy:   ',coef_comp_buo
       write(*,*)' coefficient for coriolis force:      ',coef_cor
       write(*,*)' coefficient for Lorentz force:       ',coef_lor
       write(*,*)' coefficient for temperature:         ',coef_temp
       write(*,*)' coefficient for thermal diffusion:   ',coef_d_temp
       write(*,*)' coefficient for magnetic field:      ',coef_magne
       write(*,*)' coefficient for magnetic potential:  ',coef_mag_p
       write(*,*)' coefficient for magnetic diffusion:  ',coef_d_magne
       write(*,*)' coefficient for induction:           ',coef_induct
       write(*,*)' coefficient for dummy scalar:        ',coef_scalar
       write(*,*)' coefficient for composite diffusion: ',coef_d_light
       write(*,*)''
      end if
!
      end subroutine set_material_property
!
! -----------------------------------------------------------------------
!
      end module material_property
