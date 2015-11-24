!
!      module count_num_nod_bc_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        modified by Kemorin on Oct. 2005
!        modified by Kemorin on Oct. 2009
!
!      subroutine count_num_bc_scl                                      &
!     &         (iflag_bc, nod_grp, nod_bc_list, scalar_bc)
!      subroutine count_num_bc_vect                                     &
!     &         (iflag_bc, nod_grp, bc_list, nod_bc_vect)
!
!      subroutine count_num_bc_scl_w_SGS                                &
!     &         (nod_grp, nod_bc_list, scalar_bc, sgs_bc)
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
!      subroutine count_num_bc_magne                                    &
!     &         (iflag_bc,  nod_grp,  bc_list,  nod_bc_vect)
!      subroutine count_num_bc_magp(nod_grp, magne_p, sgs_mag_p)
!        type(group_data), intent(in) :: nod_grp
!        type(scaler_fixed_nod_bc_type), intent(inout) :: magne_p
!        type(scaler_fixed_nod_bc_type), intent(inout) :: sgs_mag_p
!
      module count_num_nod_bc_MHD
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_scl                                       &
     &         (iflag_bc, nod_grp, nod_bc_list, scalar_bc)
!
      use m_bc_data_list
      use m_boundary_condition_IDs
      use t_group_data
      use t_nodal_bc_data
      use count_num_nodal_fields
!
!
      integer(kind = kint), intent(in) :: iflag_bc
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: nod_bc_list
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, nod_bc_list%num_bc, nod_bc_list%bc_name,    &
     &    nod_bc_list%ibc_type, scalar_bc%num_bc_nod, iflag_bc)
!
      end subroutine count_num_bc_scl
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_vect                                      &
     &         (iflag_bc, nod_grp, bc_list, nod_bc_vect)
!
      use m_boundary_condition_IDs
      use m_bc_data_list
      use t_group_data
      use t_nodal_bc_data
      use count_num_nodal_fields
!
!
      integer(kind = kint), intent(in) :: iflag_bc
!
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
      type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_vect
!
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    bc_list%num_bc, bc_list%bc_name, bc_list%ibc_type,            &
     &    nod_bc_vect%num_bc_nod, iflag_bc)
      call cal_max_int_4_vector                                         &
     &   (nod_bc_vect%nmax_bc, nod_bc_vect%num_bc_nod)
!
      end subroutine count_num_bc_vect
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_scl_w_SGS                                 &
     &         (nod_grp, nod_bc_list, scalar_bc, sgs_bc)
!
      use m_bc_data_list
      use m_boundary_condition_IDs
      use t_group_data
      use t_nodal_bc_data
      use count_num_nodal_fields
!
!
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: nod_bc_list
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
      type(scaler_fixed_nod_bc_type), intent(inout) :: sgs_bc
!
!
      call count_num_bc_scl                                             &
     &   (iflag_bc_fix_s, nod_grp, nod_bc_list, scalar_bc)
      call count_num_bc_scl                                             &
     &   (iflag_bc_sgs_s, nod_grp, nod_bc_list, sgs_bc)
!
      end subroutine count_num_bc_scl_w_SGS
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_velo(nod_grp, velocity, sgs_velo,         &
     &          rotation, free_plane, free_sphere, no_radial_v)
!
      use m_bc_data_list
      use m_boundary_condition_IDs
      use t_group_data
      use t_nodal_bc_data
      use count_num_nodal_fields
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
      call count_num_bc_vect                                            &
     &   (iflag_bc_fixed, nod_grp, velo_nod, velocity)
      call count_num_bc_vect                                            &
     &   (iflag_bc_sgs, nod_grp, velo_nod, sgs_velo)
!
      call count_num_bc_scalar(nod_grp%num_grp, nod_grp%istack_grp,     &
     &    nod_grp%grp_name, velo_nod%num_bc, velo_nod%bc_name,          &
     &    velo_nod%ibc_type, rotation%num_bc_nod, iflag_bc_rot_x)
!
      call count_num_bc_scl                                             &
     &   (iflag_free_sph, nod_grp, velo_nod, free_plane)
      call count_num_bc_scl                                             &
     &   (iflag_no_vr, nod_grp, velo_nod, no_radial_v)
      call count_num_bc_scl                                             &
     &   (iflag_bc_special, nod_grp, velo_nod, free_sphere)
!
      end subroutine count_num_bc_velo
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_magne                                     &
     &         (iflag_bc,  nod_grp,  bc_list,  nod_bc_vect)
!
      use m_boundary_condition_IDs
      use m_bc_data_list
      use t_group_data
      use t_nodal_bc_data
      use count_num_nodal_fields
!
!
      integer(kind = kint), intent(in) :: iflag_bc
!
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
      type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_vect
!
!
      call count_num_bc_vector                                          &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    bc_list%num_bc, bc_list%bc_name, bc_list%ibc_type,            &
     &     nod_bc_vect%num_bc_nod, iflag_bc)
!
      call add_num_bc_magne                                             &
     &   (nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    bc_list%num_bc, bc_list%bc_name, bc_list%ibc_type,            &
     &    nod_bc_vect%num_bc_nod)
!
!
      call cal_max_int_4_vector                                        &
     &   (nod_bc_vect%nmax_bc, nod_bc_vect%num_bc_nod)
!
      end subroutine count_num_bc_magne
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_magp(nod_grp, magne_p, sgs_mag_p)
!
      use m_bc_data_list
      use m_boundary_condition_IDs
      use t_group_data
      use t_nodal_bc_data
      use count_num_nodal_fields
!
      type(group_data), intent(in) :: nod_grp
      type(scaler_fixed_nod_bc_type), intent(inout) :: magne_p
      type(scaler_fixed_nod_bc_type), intent(inout) :: sgs_mag_p
!
!
      call count_num_bc_scl_w_SGS                                       &
     &   (nod_grp, e_potential_nod, magne_p, sgs_mag_p)
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
      end module count_num_nod_bc_MHD
