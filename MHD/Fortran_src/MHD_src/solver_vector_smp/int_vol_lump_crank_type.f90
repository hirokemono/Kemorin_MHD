!
!      module int_vol_lump_crank_type
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     modified by H. Matsui on Aug., 2005
!
!!      subroutine int_vol_crank_mat_lump_type(mesh, MHD_mesh,          &
!!     &          mk_MHD, djds_tbl, djds_tbl_fl,                        &
!!     &          mat_velo, mat_magne, mat_temp, mat_d_scalar)
!!      subroutine add_lumped_coriolis_mat_type(node, fld, djds_tbl,    &
!!     &          lump, mat33)
!!        type(mesh_geometry), intent(in) ::          mesh
!!        type(mesh_data_MHD), intent(in) ::          MHD_mesh
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
!
      module int_vol_lump_crank_type
!
      use m_precision
      use m_constants
!
      use t_solver_djds
      use t_geometry_data_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_crank_mat_lump_type(mesh, MHD_mesh,            &
     &          mk_MHD, djds_tbl, djds_tbl_fl,                          &
     &          mat_velo, mat_magne, mat_temp, mat_d_scalar)
!
      use t_mesh_data
      use t_finite_element_mat_MHD
!
      use m_control_parameter
      use m_physical_property
!
      type(mesh_geometry), intent(in) ::          mesh
      type(mesh_data_MHD), intent(in) ::          MHD_mesh
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
      type(DJDS_ordering_table), intent(in) ::  djds_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl_fl
!
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
!
!
!$omp parallel
       if (iflag_t_evo_4_velo .eq. id_Crank_nicolson                    &
     &     .and. coef_velo .gt. zero ) then
         call init_33_mat_type_lump(mesh%node, MHD_mesh%fluid,          &
     &       djds_tbl_fl, mk_MHD%fluid,  mat_velo)
       end if
!
      if (iflag_t_evo_4_temp .eq. id_Crank_nicolson                     &
     &     .and. coef_temp .gt. zero ) then
        call init_11_mat_type_lump(mesh%node, MHD_mesh%fluid,           &
     &      djds_tbl_fl, mk_MHD%fluid, mat_temp)
      end if
!
      if (iflag_t_evo_4_magne .eq. id_Crank_nicolson                    &
     &     .and. coef_magne .gt. zero) then
        call init_33_mat_type_lump(mesh%node, MHD_mesh%conduct,         &
     &     djds_tbl, mk_MHD%conduct, mat_magne)
      end if
!
      if (iflag_t_evo_4_vect_p .eq. id_Crank_nicolson                   &
     &     .and. coef_magne .gt. zero) then
        call init_33_mat_type_lump(mesh%node, MHD_mesh%conduct,         &
     &     djds_tbl, mk_MHD%conduct, mat_magne)
       end if
!
      if(iflag_t_evo_4_composit .eq. id_Crank_nicolson                  &
     &     .and. coef_light .gt. zero) then
        call init_11_mat_type_lump(mesh%node, MHD_mesh%fluid,           &
     &      djds_tbl_fl, mk_MHD%fluid, mat_d_scalar)
      end if
!$omp end parallel
!
      end subroutine int_vol_crank_mat_lump_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_11_mat_type_lump(node, fld, djds_tbl,             &
                lump, mat11)
!
      use t_geometry_data
      use t_finite_element_mat
!
      use init_djds_matrix_lump
!
      type(node_data), intent(in) ::            node
      type(field_geometry_data), intent(in) ::  fld
      type(lumped_mass_mat_node), intent(in) :: lump
      type(DJDS_ordering_table), intent(in) ::  djds_tbl
      type(DJDS_MATRIX), intent(inout) ::       mat11
!
!
      call init_11_matrix_lump(node%numnod,                             &
     &    fld%numnod_fld, fld%inod_fld, djds_tbl%OLDtoNEW, lump%ml_o,   &
     &    mat11%num_comp, mat11%aiccg)
!
      end subroutine init_11_mat_type_lump
!
! ----------------------------------------------------------------------
!
      subroutine init_33_mat_type_lump(node, fld, djds_tbl,             &
     &          lump, mat33)
!
!
      use t_geometry_data
      use t_finite_element_mat
!
      use init_djds_matrix_lump
!
      type(node_data), intent(in) ::            node
      type(field_geometry_data), intent(in) ::  fld
      type(DJDS_ordering_table), intent(in) ::  djds_tbl
      type(lumped_mass_mat_node), intent(in) :: lump
      type(DJDS_MATRIX), intent(inout) ::       mat33
!
!
      call init_33_matrix_lump(node%numnod,                             &
     &    fld%numnod_fld, fld%inod_fld, djds_tbl%OLDtoNEW, lump%ml_o,   &
     &    mat33%num_comp, mat33%aiccg)
!
      end subroutine init_33_mat_type_lump
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_lumped_coriolis_mat_type(node, fld, djds_tbl,      &
     &          lump, mat33)
!
      use m_physical_property
      use t_geometry_data
      use t_finite_element_mat
!
      use cal_coriolis_mat33
!
      type(node_data), intent(in) ::            node
      type(field_geometry_data), intent(in) ::  fld
      type(DJDS_ordering_table), intent(in) ::  djds_tbl
      type(lumped_mass_mat_node), intent(in) :: lump
      type(DJDS_MATRIX), intent(inout) ::       mat33
!
!
      call cal_lumped_coriolis_matrix(node%numnod,                      &
     &    fld%numnod_fld, fld%inod_fld, djds_tbl%OLDtoNEW,              &
     &    coef_cor, angular, lump%ml_o, mat33%num_comp, mat33%aiccg)
!
      end subroutine add_lumped_coriolis_mat_type
!
! ----------------------------------------------------------------------
!
      end module int_vol_lump_crank_type
