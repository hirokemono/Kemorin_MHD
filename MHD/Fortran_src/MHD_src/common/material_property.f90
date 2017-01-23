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
!!      subroutine set_material_property(iphys)
!!        type(phys_address), intent(in) :: iphys
!!@endverbatim
!!
!
      module material_property
!
      use m_precision
      use m_constants
!
      use t_phys_address
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_material_property(iphys)
!
      use calypso_mpi
      use m_control_parameter
      use m_normalize_parameter
      use m_physical_property
      use m_t_int_parameter
      use construct_MHD_coefficient
!
      type(phys_address), intent(in) :: iphys
!
!    For thermal
!
      if (evo_temp%iflag_scheme .gt. id_no_evolution) then
!
        ht_prop1%coef_advect =  one
        ht_prop1%coef_diffuse = one
        coef_h_src =  one
!
        call construct_coefficient(ht_prop1%coef_advect,                &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_termal,     &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(ht_prop1%coef_diffuse,               &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_t_diffuse,  &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(coef_h_src,                          &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_h_source,   &
     &      depth_low_t, depth_high_t)
!
        call set_implicit_4_inf_viscous(ht_prop1%coef_advect,           &
     &      evo_temp%coef_imp, evo_temp%coef_exp)
!
        ht_prop1%coef_nega_adv = - ht_prop1%coef_advect
      end if
!
!    For convection
!
      if(evo_velo%iflag_scheme .gt. id_no_evolution) then
!
        fl_prop1%coef_velo =     one
        fl_prop1%coef_diffuse =  one
        fl_prop1%coef_buo =      one
        fl_prop1%coef_comp_buo = one
        fl_prop1%coef_cor =      one
        fl_prop1%coef_lor =      one
        fl_prop1%coef_press =    one
        fl_prop1%acoef_press =   one
!
        call construct_coefficient(fl_prop1%coef_velo,                  &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_momentum,   &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(fl_prop1%coef_press,                 &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_pressure,   &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(fl_prop1%coef_diffuse,               &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_v_diffuse,  &
     &      depth_low_t, depth_high_t)
!
        call set_implicit_4_inf_viscous(fl_prop1%coef_velo,             &
     &      evo_velo%coef_imp, evo_velo%coef_exp)
!
        fl_prop1%acoef_press = one / fl_prop1%coef_press
        fl_prop1%coef_nega_v = - fl_prop1%coef_velo
!
        if (iflag_4_gravity .gt. id_turn_OFF                            &
     &     .or. iflag_4_filter_gravity .gt. id_turn_OFF) then
          call construct_coefficient(fl_prop1%coef_buo,                 &
     &       MHD_coef_list%dimless_list, MHD_coef_list%coefs_buoyancy,  &
     &      depth_low_t, depth_high_t)
        end if
!
        if (iflag_4_composit_buo .gt. id_turn_OFF) then
          call construct_coefficient(fl_prop1%coef_comp_buo,            &
     &       MHD_coef_list%dimless_list, MHD_coef_list%coefs_comp_buo,  &
     &       depth_low_t, depth_high_t)
        end if
!
        if (iflag_4_coriolis .gt. id_turn_OFF) then
          call construct_coefficient(fl_prop1%coef_cor,                 &
     &       MHD_coef_list%dimless_list, MHD_coef_list%coefs_Coriolis,  &
     &       depth_low_t, depth_high_t)
        end if
!
        if ( iflag_4_lorentz .gt. id_turn_OFF) then
          call construct_coefficient(fl_prop1%coef_lor,                 &
     &       MHD_coef_list%dimless_list, MHD_coef_list%coefs_Lorentz,   &
     &       depth_low_t, depth_high_t)
        end if
!
      end if
!
!   For Induction
!
      if (evo_magne%iflag_scheme .gt. id_no_evolution                   &
     &     .or. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
!
        cd_prop1%coef_magne =   one
        cd_prop1%coef_mag_p =   one
        cd_prop1%coef_diffuse = one
        cd_prop1%coef_induct =  one
!
        call construct_coefficient(cd_prop1%coef_magne,                 &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_magnetic,   &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(cd_prop1%coef_mag_p,                 &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_magne_p,    &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(cd_prop1%coef_diffuse,               &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_m_diffuse,  &
     &      depth_low_t, depth_high_t)
!
        call construct_coefficient(cd_prop1%coef_induct,                &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_induction,  &
     &      depth_low_t, depth_high_t)
      end if
!
      if(evo_magne%iflag_scheme .gt. id_no_evolution) then
        call set_implicit_4_inf_viscous(cd_prop1%coef_magne,            &
     &      evo_magne%coef_imp, evo_magne%coef_exp)
      end if
      if(evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        call set_implicit_4_inf_viscous(cd_prop1%coef_magne,            &
     &      evo_vect_p%coef_imp, evo_vect_p%coef_exp)
      end if
!
!   For light element
!
      if (evo_comp%iflag_scheme .gt. id_no_evolution) then
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
     &      evo_comp%coef_imp, evo_comp%coef_exp)
!
        coef_nega_c = - coef_light
      end if
!
!  Check
!
      if (my_rank .eq. 0) then
        write(*,*)''
        if(evo_velo%iflag_scheme .gt. id_no_evolution) then
          write(*,*) 'coefficient for velocity:            ',           &
     &              fl_prop1%coef_velo
          write(*,*) 'coefficient for pressure:            ',           &
     &              fl_prop1%coef_press
          write(*,*) 'coefficient for viscous diffusion:   ',           &
     &              fl_prop1%coef_diffuse
        if (iflag_4_gravity .gt. id_turn_OFF)       write(*,*)          &
     &         'coefficient for buoyancy:            ',                 &
     &              fl_prop1%coef_buo
        if (iflag_4_composit_buo .gt. id_turn_OFF)  write(*,*)          &
     &         'coefficient for composit buoyancy:   ',                 &
     &              fl_prop1%coef_comp_buo
        if (iflag_4_coriolis .gt. id_turn_OFF)      write(*,*)          &
     &         'coefficient for coriolis force:      ',                 &
     &              fl_prop1%coef_cor
        if (iflag_4_lorentz .gt. id_turn_OFF)       write(*,*)          &
     &         'coefficient for Lorentz force:       ',                 &
     &              fl_prop1%coef_lor
        end if
!
        if (evo_temp%iflag_scheme .gt. id_no_evolution) then
          write(*,*) 'coefficient for temperature:         ',           &
     &              ht_prop1%coef_advect
          write(*,*) 'coefficient for thermal diffusion:   ',           &
     &              ht_prop1%coef_diffuse
          if(iphys%i_heat_source .gt. 0) write(*,*)                     &
     &         'coefficient for heat source:         ',                 &
     &              coef_h_src
        end if
!
        if (evo_magne%iflag_scheme .gt. id_no_evolution                 &
     &     .or. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
          write(*,*) 'coefficient for magnetic field:      ',           &
     &              cd_prop1%coef_magne
          write(*,*) 'coefficient for magnetic potential:  ',           &
     &              cd_prop1%coef_mag_p
          write(*,*) 'coefficient for magnetic diffusion:  ',           &
     &              cd_prop1%coef_diffuse
          write(*,*) 'coefficient for induction:           ',           &
     &              cd_prop1%coef_induct
        end if
!
        if (evo_comp%iflag_scheme .gt. id_no_evolution) then
          write(*,*) 'coefficient for composition:         ',           &
     &              coef_light
          write(*,*) 'coefficient for composite diffusion: ',           &
     &              coef_d_light
          if(iphys%i_light_source .gt. 0) write(*,*)                    &
     &         'coefficient for light element source:',                 &
     &              coef_c_src
          write(*,*)''
        end if
      end if
!
      end subroutine set_material_property
!
! -----------------------------------------------------------------------
!
      end module material_property
