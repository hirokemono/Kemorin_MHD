!reset_MHD_aiccg_mat_type.f90
!     module reset_MHD_aiccg_mat_type
!
!        programmed by H.Matsui on Dec., 2008
!
!      subroutine s_reset_MHD_aiccg_mat_type(femmesh, MHD_mesh,         &
!     &          djds_tbl_fl, djds_tbl_cd, djds_tbl_l, djds_tbl_fl_l,   &
!     &          mat_velo, mat_magne, mat_temp, mat_d_scalar,           &
!     &          mat_press, mat_magp)
!        type(mesh_data), intent(in) ::             femmesh
!        type(mesh_data_MHD), intent(in) ::         MHD_mesh
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_cd
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!        type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
      module reset_MHD_aiccg_mat_type
!
      use m_precision
!
      use m_constants
      use m_control_parameter
      use m_geometry_constants
      use t_mesh_data
      use t_geometry_data
      use t_solver_djds
!
      implicit none
!
      private :: reset_whole_scalar_mat_type
      private :: reset_whole_vector_mat_type
      private :: reset_scalar_mat_layer_type
      private :: reset_vector_mat_layer_type
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_reset_MHD_aiccg_mat_type(femmesh, MHD_mesh,          &
     &          djds_tbl_fl, djds_tbl_cd, djds_tbl_l, djds_tbl_fl_l,    &
     &          mat_velo, mat_magne, mat_temp, mat_d_scalar,            &
     &          mat_press, mat_magp)
!
      use t_geometry_data_MHD
!
      type(mesh_data), intent(in) ::             femmesh
      type(mesh_data_MHD), intent(in) ::         MHD_mesh
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_cd
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call reset_scalar_mat_layer_type(num_t_linear,                  &
     &      MHD_mesh%fluid%iele_start_fld, MHD_mesh%fluid%iele_end_fld, &
     &      femmesh%mesh%node, femmesh%mesh%ele, djds_tbl_fl_l,         &
     &      mat_press)
      end if
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call reset_vector_mat_layer_type(femmesh%mesh%ele%nnod_4_ele,   &
     &      MHD_mesh%fluid%iele_start_fld, MHD_mesh%fluid%iele_end_fld, &
     &      femmesh%mesh%node, femmesh%mesh%ele, djds_tbl_fl,           &
     &      mat_velo)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call reset_scalar_mat_layer_type(femmesh%mesh%ele%nnod_4_ele,   &
     &      MHD_mesh%fluid%iele_start_fld, MHD_mesh%fluid%iele_end_fld, &
     &      femmesh%mesh%node, femmesh%mesh%ele, djds_tbl_fl,           &
     &      mat_temp)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call reset_scalar_mat_layer_type(femmesh%mesh%ele%nnod_4_ele,   &
     &      MHD_mesh%fluid%iele_start_fld, MHD_mesh%fluid%iele_end_fld, &
     &      femmesh%mesh%node, femmesh%mesh%ele, djds_tbl_fl,           &
     &      mat_d_scalar)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &     .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call reset_whole_scalar_mat_type(femmesh%mesh%node, mat_magp)
        call reset_scalar_mat_layer_type(num_t_linear, ione,            &
     &      femmesh%mesh%ele%numele, femmesh%mesh%node, &
     &      femmesh%mesh%ele, djds_tbl_l,  mat_magp)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call reset_whole_vector_mat_type(femmesh%mesh%node, mat_magne)
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
        call reset_vector_mat_layer_type(femmesh%mesh%ele%nnod_4_ele,   &
     &      MHD_mesh%conduct%iele_start_fld,                            &
     &      MHD_mesh%conduct%iele_end_fld,                              &
     &      femmesh%mesh%node, femmesh%mesh%ele, djds_tbl_cd,           &
     &      mat_magne)
      end if
!
      end subroutine s_reset_MHD_aiccg_mat_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine reset_whole_scalar_mat_type(node, mat11)
!
      type(node_data), intent(in) ::    node
      type(DJDS_MATRIX), intent(inout) :: mat11
!
!
      mat11%aiccg(0:mat11%num_comp) = zero
!
      mat11%ALUG_U(1:node%internal_node) = one
      mat11%ALUG_L(1:node%internal_node) = one
!
      end subroutine reset_whole_scalar_mat_type
!
! ----------------------------------------------------------------------
!
      subroutine reset_whole_vector_mat_type(node, mat33)
!
      type(node_data), intent(in) ::    node
      type(DJDS_MATRIX), intent(inout) :: mat33
!
!
      mat33%aiccg(0:mat33%num_comp) = zero
!
      mat33%ALUG_U(1:node%internal_node) = one
      mat33%ALUG_L(1:node%internal_node) = one
!
      end subroutine reset_whole_vector_mat_type
!
! ----------------------------------------------------------------------
!
      subroutine reset_scalar_mat_layer_type(nnod, iele_st, iele_ed,    &
     &          node, ele, djds_fl, mat11)
!
      integer(kind = kint), intent(in) :: nnod, iele_st, iele_ed
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(DJDS_ordering_table), intent(in) :: djds_fl
      type(DJDS_MATRIX), intent(inout) :: mat11
!
      integer(kind = kint) :: inod, iele, in, k1
!
!
      mat11%aiccg(0:mat11%num_comp) =   zero
      mat11%aiccg(1:node%numnod) = one
!
      do k1 = 1, nnod
        do iele = iele_st, iele_ed
          inod = ele%ie(iele,k1)
          in = djds_fl%OLDtoNEW(inod)
          mat11%aiccg(in) = zero
        end do
      end do
!
      mat11%ALUG_U(1:node%internal_node) = one
      mat11%ALUG_L(1:node%internal_node) = one
!
      end subroutine reset_scalar_mat_layer_type
!
! ----------------------------------------------------------------------
!
      subroutine reset_vector_mat_layer_type(nnod, iele_st, iele_ed,    &
     &          node, ele, djds_fl, mat33)
!
      integer(kind = kint), intent(in) :: nnod, iele_st, iele_ed
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(DJDS_ordering_table), intent(in) :: djds_fl
      type(DJDS_MATRIX), intent(inout) :: mat33
!
      integer(kind = kint) :: inod, iele, in, k1
!
!
      mat33%aiccg(0:mat33%num_comp) = zero
!
      do inod = 1, node%numnod
         mat33%aiccg(inod*9-8) = one
         mat33%aiccg(inod*9-4) = one
         mat33%aiccg(inod*9  ) = one
      end do
!
      do k1 = 1, nnod
        do iele = iele_st, iele_ed
          inod = ele%ie(iele,k1)
          in = djds_fl%OLDtoNEW(inod)
          mat33%aiccg(in*9-8) = zero
          mat33%aiccg(in*9-4) = zero
          mat33%aiccg(in*9  ) = zero
        end do
      end do
!
      mat33%ALUG_U(1:9*node%internal_node) = one
      mat33%ALUG_L(1:9*node%internal_node) = one
!
      end subroutine reset_vector_mat_layer_type
!
! ----------------------------------------------------------------------
!
      end module reset_MHD_aiccg_mat_type
