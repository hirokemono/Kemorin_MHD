!count_num_bc_nod_type.f90
!      module count_num_bc_nod_type
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        modified by Kemorin on Oct. 2005
!        modified by Kemorin on Jan.. 2009
!
!      subroutine count_num_bc_velo(nod_grp, velocity, sgs_velo,        &
!     &          rotation, free_plane, free_sphere, no_radial_v)
!        type(group_data), intent(in) :: nod_grp
!        type(vect_fixed_nod_bc_type),     intent(inout) :: velocity
!        type(vect_fixed_nod_bc_type),     intent(inout) :: sgs_velo
!        type(scaler_rotaion_nod_bc_type), intent(inout) :: rotation
!        type(scaler_fixed_nod_bc_type),   intent(inout) :: free_plane
!        type(scaler_fixed_nod_bc_type),   intent(inout) :: free_sphere
!        type(scaler_fixed_nod_bc_type),   intent(inout) :: no_radial_v
!      subroutine count_num_bc_press(nod_grp, press, sgs_press)
!        type(group_data), intent(in) :: nod_grp
!        type(scaler_fixed_nod_bc_type), intent(inout) :: press
!        type(scaler_fixed_nod_bc_type), intent(inout) :: sgs_press
!      subroutine count_num_bc_temp(nod_grp, temp, sgs_temp)
!        type(group_data), intent(in) :: nod_grp
!        type(scaler_fixed_nod_bc_type), intent(inout) :: temp
!        type(scaler_fixed_nod_bc_type), intent(inout) :: sgs_temp
!      subroutine count_num_bc_vecp(nod_grp, vector_p, sgs_vect_p)
!        type(group_data), intent(in) :: nod_grp
!        type(vect_fixed_nod_bc_type), intent(inout) :: vector_p
!        type(vect_fixed_nod_bc_type), intent(inout) :: sgs_vect_p
!      subroutine count_num_bc_magne(nod_grp, magne, sgs_magne)
!        type(group_data), intent(in) :: nod_grp
!        type(vect_fixed_nod_bc_type), intent(inout) :: magne
!        type(vect_fixed_nod_bc_type), intent(inout) :: sgs_magne
!      subroutine count_num_bc_magp(nod_grp, magne_p, sgs_mag_p)
!        type(group_data), intent(in) :: nod_grp
!        type(scaler_fixed_nod_bc_type), intent(inout) :: magne_p
!        type(scaler_fixed_nod_bc_type), intent(inout) :: sgs_mag_p
!      subroutine count_num_bc_current(nod_grp, current)
!        type(group_data), intent(in) :: nod_grp
!        type(vect_fixed_nod_bc_type), intent(inout) :: current
!
!      subroutine count_num_bc_composit(nod_grp, composition)
!        type(group_data), intent(in) :: nod_grp
!        type(scaler_fixed_nod_bc_type), intent(inout) :: composition
!
      module count_num_bc_nod_type
!
      use m_precision
!
      use m_constants
      use m_bc_data_list
      use m_boundary_condition_IDs
      use t_group_data
      use t_nodal_bc_data
      use count_num_nodal_fields
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_velo(nod_grp, velocity, sgs_velo,         &
     &          rotation, free_plane, free_sphere, no_radial_v)
!
      type(group_data), intent(in) :: nod_grp
      type(vect_fixed_nod_bc_type),     intent(inout) :: velocity
      type(vect_fixed_nod_bc_type),     intent(inout) :: sgs_velo
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rotation
      type(scaler_fixed_nod_bc_type),   intent(inout) :: free_plane
      type(scaler_fixed_nod_bc_type),   intent(inout) :: free_sphere
      type(scaler_fixed_nod_bc_type),   intent(inout) :: no_radial_v
!
!
      call count_num_bc_vector(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, velo_nod%num_bc, velo_nod%bc_name,          &
     &    velo_nod%ibc_type, velocity%num_bc_nod, iflag_bc_fixed)
!
      call count_num_bc_vector(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, velo_nod%num_bc, velo_nod%bc_name,          &
     &    velo_nod%ibc_type, sgs_velo%num_bc_nod, iflag_bc_sgs)
!
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, velo_nod%num_bc, velo_nod%bc_name,          &
     &    velo_nod%ibc_type, rotation%num_bc_nod, iflag_bc_rot_x)
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, velo_nod%num_bc, velo_nod%bc_name,          &
     &    velo_nod%ibc_type, free_plane%num_bc_nod, iflag_free_sph)
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, velo_nod%num_bc, velo_nod%bc_name,          &
     &    velo_nod%ibc_type, no_radial_v%num_bc_nod, iflag_no_vr)
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, velo_nod%num_bc, velo_nod%bc_name,          &
     &    velo_nod%ibc_type, free_sphere%num_bc_nod, iflag_bc_special)
!
!
      call cal_max_int_4_vector(velocity%nmax_bc, velocity%num_bc_nod)
      call cal_max_int_4_vector(sgs_velo%nmax_bc, sgs_velo%num_bc_nod)
!
      end subroutine count_num_bc_velo
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_press(nod_grp, press, sgs_press)
!
      type(group_data), intent(in) :: nod_grp
      type(scaler_fixed_nod_bc_type), intent(inout) :: press
      type(scaler_fixed_nod_bc_type), intent(inout) :: sgs_press
!
!
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, press_nod%num_bc,                           &
     &    press_nod%bc_name, press_nod%ibc_type,                        &
     &    press%num_bc_nod, iflag_bc_fix_s)
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, press_nod%num_bc,                           &
     &    press_nod%bc_name, press_nod%ibc_type,                        &
     &    sgs_press%num_bc_nod, iflag_bc_sgs_s)
!
      end subroutine count_num_bc_press
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_temp(nod_grp, temp, sgs_temp)
!
      type(group_data), intent(in) :: nod_grp
      type(scaler_fixed_nod_bc_type), intent(inout) :: temp
      type(scaler_fixed_nod_bc_type), intent(inout) :: sgs_temp
!
!
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, temp_nod%num_bc,                            &
     &    temp_nod%bc_name, temp_nod%ibc_type,                          &
     &    temp%num_bc_nod, iflag_bc_fix_s)
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, temp_nod%num_bc,                            &
     &    temp_nod%bc_name, temp_nod%ibc_type,                          &
     &    sgs_temp%num_bc_nod, iflag_bc_sgs_s)
!
      end subroutine count_num_bc_temp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_vecp(nod_grp, vector_p, sgs_vect_p)
!
      type(group_data), intent(in) :: nod_grp
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_p
      type(vect_fixed_nod_bc_type), intent(inout) :: sgs_vect_p
!
!
      call count_num_bc_vector(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, a_potential_nod%num_bc,                     &
     &    a_potential_nod%bc_name, a_potential_nod%ibc_type,            &
     &    vector_p%num_bc_nod, iflag_bc_fixed)
!
      call count_num_bc_vector(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, a_potential_nod%num_bc,                     &
     &    a_potential_nod%bc_name, a_potential_nod%ibc_type,            &
     &    sgs_vect_p%num_bc_nod, iflag_bc_sgs)
!
!
      call cal_max_int_4_vector(vector_p%nmax_bc, vector_p%num_bc_nod)
      call cal_max_int_4_vector(sgs_vect_p%nmax_bc,                     &
     &    sgs_vect_p%num_bc_nod)
!
      end subroutine count_num_bc_vecp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_magne(nod_grp, magne, sgs_magne)
!
      type(group_data), intent(in) :: nod_grp
      type(vect_fixed_nod_bc_type), intent(inout) :: magne
      type(vect_fixed_nod_bc_type), intent(inout) :: sgs_magne
!
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    magne_nod%num_bc, magne_nod%bc_name, magne_nod%ibc_type,      &
     &    magne%num_bc_nod, iflag_bc_fixed)
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    magne_nod%num_bc, magne_nod%bc_name, magne_nod%ibc_type,      &
     &    sgs_magne%num_bc_nod, iflag_bc_sgs)
!
      call add_num_bc_magne                                             &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    magne_nod%num_bc, magne_nod%bc_name, magne_nod%ibc_type,      &
     &    magne%num_bc_nod)
!
!
      call cal_max_int_4_vector(magne%nmax_bc, magne%num_bc_nod)
      call cal_max_int_4_vector(sgs_magne%nmax_bc,                      &
     &    sgs_magne%num_bc_nod)
!
      end subroutine count_num_bc_magne
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_magp(nod_grp, magne_p, sgs_mag_p)
!
      type(group_data), intent(in) :: nod_grp
      type(scaler_fixed_nod_bc_type), intent(inout) :: magne_p
      type(scaler_fixed_nod_bc_type), intent(inout) :: sgs_mag_p
!
!
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, e_potential_nod%num_bc,                     &
     &    e_potential_nod%bc_name, e_potential_nod%ibc_type,            &
     &    magne_p%num_bc_nod, iflag_bc_fix_s)
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, e_potential_nod%num_bc,                     &
     &    e_potential_nod%bc_name, e_potential_nod%ibc_type,            &
     &    sgs_mag_p%num_bc_nod, iflag_bc_sgs_s)
!
      call add_num_bc_mag_p(nod_grp%num_grp, nod_grp%istack_grp,        &
     &    nod_grp%grp_name, e_potential_nod%num_bc,                     &
     &    e_potential_nod%bc_name, e_potential_nod%ibc_type,            &
     &    sgs_mag_p%num_bc_nod)
!
      end subroutine count_num_bc_magp
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_current(nod_grp, current)
!
      type(group_data), intent(in) :: nod_grp
      type(vect_fixed_nod_bc_type), intent(inout) :: current
!
!
      call count_num_bc_vector(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, current_nod%num_bc, current_nod%bc_name,    &
     &    current_nod%ibc_type, current%num_bc_nod, izero)
!
      call cal_max_int_4_vector(current%nmax_bc, current%num_bc_nod)
!
      end subroutine count_num_bc_current
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_composit(nod_grp, composition)
!
      type(group_data), intent(in) :: nod_grp
      type(scaler_fixed_nod_bc_type), intent(inout) :: composition
!
!
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, light_nod%num_bc, light_nod%bc_name,        &
     &    light_nod%ibc_type, composition%num_bc_nod, iflag_bc_fix_s)
!
      end subroutine count_num_bc_composit
!
!  ---------------------------------------------------------------------
!
      end module count_num_bc_nod_type
