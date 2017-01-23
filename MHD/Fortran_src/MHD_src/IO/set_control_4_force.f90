!>@file   set_control_4_force.f90
!!@brief  module set_control_4_force
!!
!!@author H. Matsui
!!@date Programmed in 2002
!
!> @brief Set parameters for forces from control data
!!
!!@verbatim
!!      subroutine s_set_control_4_force                                &
!!     &         (frc_ctl, g_ctl, cor_ctl, mcv_ctl)
!!       type(forces_control), intent(inout) :: frc_ctl
!!       type(gravity_control), intent(inout) :: g_ctl
!!       type(coriolis_control), intent(inout) :: cor_ctl
!!       type(magneto_convection_control), intent(inout) :: mcv_ctl
!!@endverbatim
!
      module set_control_4_force
!
      use m_precision
      use m_constants
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_force                                  &
     &         (frc_ctl, g_ctl, cor_ctl, mcv_ctl)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_control_parameter
      use m_physical_property
      use t_ctl_data_mhd_forces
      use skip_comment_f
!
      type(forces_control), intent(inout) :: frc_ctl
      type(gravity_control), intent(inout) :: g_ctl
      type(coriolis_control), intent(inout) :: cor_ctl
      type(magneto_convection_control), intent(inout) :: mcv_ctl
!
      integer (kind = kint) :: i, iflag
      character(len=kchara) :: tmpchara
!
!
      iflag_4_gravity =        id_turn_OFF
      iflag_4_coriolis =       id_turn_OFF
      iflag_4_lorentz =        id_turn_OFF
      iflag_4_rotate =         id_turn_OFF
      iflag_4_composit_buo =   id_turn_OFF
      iflag_4_filter_gravity = id_turn_OFF
!
      if (evo_velo%iflag_scheme .eq. id_no_evolution) then
        num_force = 0
      else
        if (frc_ctl%force_names%icou .gt. 0) then
          num_force = frc_ctl%force_names%num
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &      write(*,*) 'num_force ', num_force
        end if
      end if
!
      if (num_force .gt. 0) then
!
        call allocate_force_list
        name_force(1:num_force)                                         &
     &          = frc_ctl%force_names%c_tbl(1:num_force)
        call dealloc_name_force_ctl(frc_ctl)
!
        do i = 1, num_force
          if(    cmp_no_case(name_force(i), 'Gravity')                  &
     &      .or. cmp_no_case(name_force(i), 'Gravity_ele')              &
     &      .or. cmp_no_case(name_force(i), 'Gravity_element')          &
     &      .or. cmp_no_case(name_force(i), 'Buoyancy')                 &
     &      .or. cmp_no_case(name_force(i), 'Buoyancy_ele')             &
     &      .or. cmp_no_case(name_force(i), 'Buoyancy_element')         &
     &      .or. cmp_no_case(name_force(i), 'Thermal_buoyancy')         &
     &      .or. cmp_no_case(name_force(i), 'Thermal_buoyancy_ele')     &
     &      .or. cmp_no_case(name_force(i), 'Thermal_buoyancy_element') &
     &      .or. cmp_no_case(name_force(i), 'Thermal_gravity')          &
     &      .or. cmp_no_case(name_force(i), 'Thermal_gravity_ele')      &
     &      .or. cmp_no_case(name_force(i), 'Thermal_gravity_element')  &
     &      ) iflag_4_gravity =  id_FORCE_ele_int
!
          if(     cmp_no_case(name_force(i), 'Gravity_nod')             &
     &       .or. cmp_no_case(name_force(i), 'Buoyancy_nod')            &
     &       .or. cmp_no_case(name_force(i), 'Thermal_buoyancy_nod')    &
     &       .or. cmp_no_case(name_force(i), 'Thermal_gravity_nod')     &
     &       .or. cmp_no_case(name_force(i), 'Gravity_node')            &
     &       .or. cmp_no_case(name_force(i), 'Buoyancy_node')           &
     &       .or. cmp_no_case(name_force(i), 'Thermal_buoyancy_node')   &
     &       .or. cmp_no_case(name_force(i), 'Thermal_gravity_node')    &
     &      ) then
            if(evo_velo%iflag_scheme .eq. id_Crank_nicolson_cmass) then
              iflag_4_gravity = id_FORCE_ele_int
            else
              iflag_4_gravity = id_FORCE_at_node
            end if
          end if
!
          if(  cmp_no_case(name_force(i), 'Composite_buoyancy')         &
     &    .or. cmp_no_case(name_force(i), 'Composite_buoyancy_ele')     &
     &    .or. cmp_no_case(name_force(i), 'Composite_buoyancy_element') &
     &    .or. cmp_no_case(name_force(i), 'Composite_gravity')          &
     &    .or. cmp_no_case(name_force(i), 'Composite_gravity_ele')      &
     &    .or. cmp_no_case(name_force(i), 'Composite_gravity_element')  &
     &       ) iflag_4_composit_buo =  id_FORCE_ele_int
!
          if(     cmp_no_case(name_force(i), 'Composite_buoyancy_nod')  &
     &       .or. cmp_no_case(name_force(i), 'Composite_gravity_nod')   &
     &       .or. cmp_no_case(name_force(i), 'Composite_buoyancy_node') &
     &       .or. cmp_no_case(name_force(i), 'Composite_gravity_node')  &
     &       ) then
            if(evo_velo%iflag_scheme .eq. id_Crank_nicolson_cmass) then
              iflag_4_composit_buo = id_FORCE_ele_int
            else
              iflag_4_composit_buo = id_FORCE_at_node
            end if
          end if
!
          if(     cmp_no_case(name_force(i), 'Filtered_gravity')        &
     &       .or. cmp_no_case(name_force(i), 'Filtered_buoyancy')       &
     &       ) iflag_4_filter_gravity =  id_FORCE_ele_int
!
          if (cmp_no_case(name_force(i), 'Coriolis')                    &
     &        )  iflag_4_coriolis = id_FORCE_ele_int
!
          if (cmp_no_case(name_force(i), 'Coriolis_node')) then
            if(evo_velo%iflag_scheme .eq. id_Crank_nicolson_cmass) then
              iflag_4_coriolis = id_FORCE_ele_int
            else
              iflag_4_coriolis = id_FORCE_at_node
            end if
          end if
!
          if(cmp_no_case(name_force(i), 'Coriolis_imp')) then
            if(evo_velo%iflag_scheme .eq. id_Crank_nicolson) then
              iflag_4_coriolis = id_Coriolis_ele_imp
            else if(evo_velo%iflag_scheme .eq. id_Crank_nicolson_cmass) &
     &          then
              iflag_4_coriolis = id_Coriolis_ele_imp
            else
              iflag_4_coriolis = id_FORCE_ele_int
            end if
          end if
!
          if(cmp_no_case(name_force(i), 'Coriolis_node_imp')) then
            if(evo_velo%iflag_scheme .eq. id_Crank_nicolson) then
              iflag_4_coriolis = id_Coriolis_nod_imp
            else if(evo_velo%iflag_scheme .eq. id_Crank_nicolson_cmass) &
     &               then
              iflag_4_coriolis = id_Coriolis_ele_imp
            else
              iflag_4_coriolis = id_FORCE_ele_int
            end if
          end if
!
          if(cmp_no_case(name_force(i), 'Lorentz')                      &
     &           )  iflag_4_lorentz = id_turn_ON
          if(cmp_no_case(name_force(i), 'Lorentz_full')                 &
     &           )  iflag_4_lorentz = id_Lorentz_w_Emag
!
          if(cmp_no_case(name_force(i), 'Rotation_form')                &
     &           )  iflag_4_rotate =  id_turn_ON
        end do
      end if
!
!  direction of gravity
!
      fl_prop1%i_grav = iflag_no_gravity
      iflag = iflag_4_gravity + iflag_4_composit_buo                    &
     &       + iflag_4_filter_gravity
      if (iflag .gt. 0) then
        if (g_ctl%gravity%iflag .eq. 0) then
          fl_prop1%i_grav = iflag_self_r_g
        else
          tmpchara = g_ctl%gravity%charavalue
!
          if     (cmp_no_case(tmpchara, 'constant')) then
             fl_prop1%i_grav = iflag_const_g
          else if(cmp_no_case(tmpchara, 'constant_radial')) then
             fl_prop1%i_grav = iflag_radial_g
          else if(cmp_no_case(tmpchara, 'radial')) then
             fl_prop1%i_grav = iflag_self_r_g
           end if
        end if
!
        if (fl_prop1%i_grav .eq. iflag_const_g) then
          if (g_ctl%gravity_vector%icou .eq. 0) then
            e_message = 'Set gravity vector'
            call calypso_MPI_abort(ierr_force, e_message)
          else
!
            do i = 1, g_ctl%gravity_vector%num
              if(cmp_no_case(g_ctl%gravity_vector%c_tbl(i),'X')         &
     &            ) fl_prop1%grav(1) = - g_ctl%gravity_vector%vect(i)
              if(cmp_no_case(g_ctl%gravity_vector%c_tbl(i),'Y')         &
     &            ) fl_prop1%grav(2) = - g_ctl%gravity_vector%vect(i)
              if(cmp_no_case(g_ctl%gravity_vector%c_tbl(i),'Z')         &
     &            ) fl_prop1%grav(3) = - g_ctl%gravity_vector%vect(i)
            end do
            call dealloc_control_array_c_r(g_ctl%gravity_vector)
          end if
        end if
      end if
      if (iflag_debug .eq. iflag_routine_msg)                           &
     &               write(*,*) 'i_grav ',fl_prop1%i_grav
!
!  direction of angular velocity of rotation
!
      fl_prop1%sys_rot(1:2) = zero
      fl_prop1%sys_rot(3) =   one
!
      if ((iflag_4_coriolis*cor_ctl%system_rotation%icou) .gt. 0) then
        do i = 1, cor_ctl%system_rotation%num
          if(cmp_no_case(cor_ctl%system_rotation%c_tbl(i),'X')          &
     &       )  fl_prop1%sys_rot(1) = cor_ctl%system_rotation%vect(i)
          if(cmp_no_case(cor_ctl%system_rotation%c_tbl(i),'Y')          &
     &       )  fl_prop1%sys_rot(2) = cor_ctl%system_rotation%vect(i)
          if(cmp_no_case(cor_ctl%system_rotation%c_tbl(i),'Z')          &
     &       )  fl_prop1%sys_rot(3) = cor_ctl%system_rotation%vect(i)
        end do
        call dealloc_control_array_c_r(cor_ctl%system_rotation)
      end if
!
!
!  setting for external mangnetic field
!
      if (mcv_ctl%magneto_cv%iflag .eq. 0) then
        iflag_magneto_cv = id_turn_OFF
      else
        if(yes_flag(mcv_ctl%magneto_cv%charavalue))                     &
     &                     iflag_magneto_cv = id_turn_ON
      end if
!
      cd_prop1%ex_magne(1:3) = 0.0d0
!
      if (iflag_magneto_cv .gt. id_turn_OFF) then
        if (mcv_ctl%ext_magne%icou .eq. 0) then
          e_message = 'Set external magnetic field'
          call calypso_MPI_abort(ierr_force, e_message)
        else
!
          do i = 1, mcv_ctl%ext_magne%num
            if(cmp_no_case(mcv_ctl%ext_magne%c_tbl(i),'X')              &
     &            ) cd_prop1%ex_magne(1) = mcv_ctl%ext_magne%vect(i)
            if(cmp_no_case(mcv_ctl%ext_magne%c_tbl(i),'Y')              &
     &            ) cd_prop1%ex_magne(2) = mcv_ctl%ext_magne%vect(i)
            if(cmp_no_case(mcv_ctl%ext_magne%c_tbl(i),'Z')              &
     &            ) cd_prop1%ex_magne(3) = mcv_ctl%ext_magne%vect(i)
          end do
          call dealloc_control_array_c_r(mcv_ctl%ext_magne)
        end if
      end if
!
     if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'name_force '
        do i = 1, num_force
          write(*,*) i, trim(name_force(i))
        end do
!
        if(fl_prop1%i_grav .eq. iflag_const_g) then
          write(*,'(a, 1p3E25.15e3)') 'gravity ', fl_prop1%grav(1:3)
        end if
!
        write(*,*) 'magneto_cv ',iflag_magneto_cv
        write(*,'(a,1p3E25.15e3)') 'ex_magne ',cd_prop1%ex_magne
        write(*,*) 'iflag_4_coriolis', iflag_4_coriolis
        if(iflag_4_coriolis .gt. id_turn_OFF) then
          write(*,'(a, 1p3E25.15e3)') 'rotation:', fl_prop1%sys_rot(1:3)
        end if
      end if
!
!
      end subroutine s_set_control_4_force
!
! -----------------------------------------------------------------------
!
      end module set_control_4_force
