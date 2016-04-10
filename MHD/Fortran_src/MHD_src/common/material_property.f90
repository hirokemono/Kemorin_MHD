!>@file   material_property.f90
!!@brief  module material_property
!!
!!@author H. Matsui
!!@date Programmed in 2001
!!@date Modified in Jan., 2007
!
!>@brief  Subroutines to set coeffiecient of each term
!!
!!@verbatim
!!      subroutine set_material_property
!!@endverbatim
!!
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
      use calypso_mpi
      use m_control_parameter
      use m_normalize_parameter
      use m_node_phys_data
      use m_physical_property
      use m_t_int_parameter
      use construct_MHD_coefficient
!
!    For thermal
!
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
!
        coef_temp =   one
        coef_d_temp = one
        coef_h_src =  one
!
        call construct_coefficient(coef_temp,                           &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_termal,     &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_d_temp,                         &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_t_diffuse,  &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_h_src,                          &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_h_source,   &
     &      depth_low_t, depth_high_t)
!
        call set_implicit_4_inf_viscous(coef_temp,                      &
     &      coef_imp_t, coef_exp_t)
!
        coef_nega_t = - coef_temp
      end if
!
!    For convection
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
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
        call construct_coefficient(coef_velo,                           &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_momentum,   &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_press,                          &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_pressure,   &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_d_velo,                         &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_v_diffuse,  &
     &      depth_low_t, depth_high_t)
!
        call set_implicit_4_inf_viscous(coef_velo,                      &
     &      coef_imp_v, coef_exp_v)
!
        acoef_press = one / coef_press
        coef_nega_v = - coef_velo
!
        if (iflag_4_gravity .gt. id_turn_OFF                            &
     &     .or. iflag_4_filter_gravity .gt. id_turn_OFF) then
          call construct_coefficient(coef_buo,                          &
     &       MHD_coef_list%dimless_list, MHD_coef_list%coefs_buoyancy,  &
     &      depth_low_t, depth_high_t)
        end if
!
        if (iflag_4_composit_buo .gt. id_turn_OFF) then
          call construct_coefficient(coef_comp_buo,                     &
     &       MHD_coef_list%dimless_list, MHD_coef_list%coefs_comp_buo,  &
     &       depth_low_t, depth_high_t)
        end if
!
        if (iflag_4_coriolis .gt. id_turn_OFF) then
          call construct_coefficient(coef_cor,                          &
     &       MHD_coef_list%dimless_list, MHD_coef_list%coefs_Coriolis,  &
     &       depth_low_t, depth_high_t)
        end if
!
        if ( iflag_4_lorentz .gt. id_turn_OFF) then
          call construct_coefficient(coef_lor,                          &
     &       MHD_coef_list%dimless_list, MHD_coef_list%coefs_Lorentz,   &
     &       depth_low_t, depth_high_t)
        end if
!
      end if
!
!   For Induction
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &     .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
!
        coef_magne =   one
        coef_mag_p =   one
        coef_d_magne = one
        coef_induct =  one
!
        call construct_coefficient(coef_magne,                          &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_magnetic,   &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_mag_p,                          &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_magne_p,    &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_d_magne,                        &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_m_diffuse,  &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_induct,                         &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_induction,  &
     &      depth_low_t, depth_high_t)
!
        call set_implicit_4_inf_viscous(coef_magne,                     &
     &      coef_imp_b, coef_exp_b)
!
      end if
!
!   For light element
!
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        coef_light =    one
        coef_d_light =  one
        coef_c_src =    one
!
        call construct_coefficient(coef_light,                          &
     &     MHD_coef_list%dimless_list, MHD_coef_list%coefs_composition, &
     &     depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_d_light,                        &
     &     MHD_coef_list%dimless_list, MHD_coef_list%coefs_c_diffuse,   &
     &     depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_c_src,                          &
     &     MHD_coef_list%dimless_list, MHD_coef_list%coefs_c_source,    &
     &     depth_low_t, depth_high_t)
!
        call set_implicit_4_inf_viscous(coef_light,                     &
     &      coef_imp_c, coef_exp_c)
!
        coef_nega_c = - coef_light
      end if
!
!  Check
!
      if (my_rank .eq. 0) then
        write(*,*)''
        if (iflag_t_evo_4_velo .gt. id_no_evolution) then
          write(*,*) 'coefficient for velocity:            ',           &
     &              coef_velo
          write(*,*) 'coefficient for pressure:            ',           &
     &              coef_press
          write(*,*) 'coefficient for viscous diffusion:   ',           &
     &              coef_d_velo
        if (iflag_4_gravity .gt. id_turn_OFF)       write(*,*)          &
     &         'coefficient for buoyancy:            ', coef_buo
        if (iflag_4_composit_buo .gt. id_turn_OFF)  write(*,*)          &
     &         'coefficient for composit buoyancy:   ', coef_comp_buo
        if (iflag_4_coriolis .gt. id_turn_OFF)      write(*,*)          &
     &         'coefficient for coriolis force:      ', coef_cor
        if (iflag_4_lorentz .gt. id_turn_OFF)       write(*,*)          &
     &         'coefficient for Lorentz force:       ', coef_lor
        end if
!
        if (iflag_t_evo_4_temp .gt. id_no_evolution) then
          write(*,*) 'coefficient for temperature:         ',           &
     &              coef_temp
          write(*,*) 'coefficient for thermal diffusion:   ',           &
     &              coef_d_temp
          if(iphys%i_heat_source .gt. 0) write(*,*)                     &
     &         'coefficient for heat source:         ', coef_h_src
        end if
!
        if (iflag_t_evo_4_magne .gt. id_no_evolution                    &
     &     .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
          write(*,*) 'coefficient for magnetic field:      ',           &
     &              coef_magne
          write(*,*) 'coefficient for magnetic potential:  ',           &
     &              coef_mag_p
          write(*,*) 'coefficient for magnetic diffusion:  ',           &
     &              coef_d_magne
          write(*,*) 'coefficient for induction:           ',           &
     &              coef_induct
        end if
!
        if (iflag_t_evo_4_composit .gt. id_no_evolution) then
          write(*,*) 'coefficient for composition:         ',           &
     &              coef_light
          write(*,*) 'coefficient for composite diffusion: ',           &
     &              coef_d_light
          if(iphys%i_light_source .gt. 0) write(*,*)                    &
     &         'coefficient for light element source:', coef_c_src
          write(*,*)''
        end if
      end if
!
      end subroutine set_material_property
!
! -----------------------------------------------------------------------
!
      end module material_property
