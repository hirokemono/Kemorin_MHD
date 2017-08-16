!>@file   allocate_solver_djds_MHD.f90
!!@brief  module allocate_solver_djds_MHD
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!!@date Modified in Nov., 2013
!
!>     DJDS ordering table for MHD dynamo model
!!
!!      subroutine alloc_aiccg_matrices                                 &
!!     &          (node, fl_prop, cd_prop, ht_prop, cp_prop,            &
!!     &           djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fll,     &
!!     &           mat_velo, mat_magne, mat_temp, mat_light,            &
!!     &           mat_press, mat_magp)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(node_data), intent(in) :: node
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fll
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_light
!!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
!!      subroutine alloc_MG_zero_matrices                               &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop, mat_velo,         &
!!     &          mat_magne, mat_temp,  mat_light, mat_press, mat_magp)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_light
!!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
!!      subroutine dealloc_aiccg_matrices                               &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop, mat_velo,         &
!!     &          mat_magne, mat_temp, mat_light, mat_press, mat_magp)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_light
!!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
      module allocate_solver_djds_MHD
!
      use m_precision
!
      use t_control_parameter
      use t_physical_property
      use t_comm_table
      use t_solver_djds
      use t_vector_for_solver
      use t_interpolate_table
      use t_sorted_node_MHD
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_aiccg_matrices                                   &
     &          (node, fl_prop, cd_prop, ht_prop, cp_prop,              &
     &           djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fll,       &
     &           mat_velo, mat_magne, mat_temp, mat_light,              &
     &           mat_press, mat_magp)
!
      use t_geometry_data
      use t_geometry_data_MHD
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      type(node_data), intent(in) :: node
      type(DJDS_ordering_table),  intent(in) :: djds_tbl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fll
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      if ( fl_prop%iflag_scheme .gt. id_no_evolution) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &      djds_tbl_fll, mat_press)
!
        if ( fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(node%numnod, node%internal_node,   &
     &        djds_tbl_fl, mat_velo)
        end if
      end if
!
      if ( ht_prop%iflag_scheme .ge. id_Crank_nicolson) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &      djds_tbl_fl, mat_temp)
      end if
!
      if ( cp_prop%iflag_scheme .ge. id_Crank_nicolson) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &       djds_tbl_fl, mat_light)
      end if
!
      if ( cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &       djds_tbl_l, mat_magp)
!
        if ( cd_prop%iflag_Bevo_scheme .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(node%numnod, node%internal_node,   &
     &       djds_tbl, mat_magne)
        end if
      end if
!
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &      djds_tbl_l, mat_magp)
!
        if (cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(node%numnod, node%internal_node,   &
     &        djds_tbl, mat_magne)
        end if
      end if
!
      end subroutine alloc_aiccg_matrices
!
! ----------------------------------------------------------------------
!
      subroutine alloc_MG_zero_matrices                                 &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, mat_velo,           &
     &          mat_magne, mat_temp,  mat_light, mat_press, mat_magp)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      if ( fl_prop%iflag_scheme .gt. id_no_evolution) then
        call alloc_type_zero_mat(mat_press)
!
        if ( fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
          call alloc_type_zero_mat(mat_velo)
        end if
      end if
!
      if ( ht_prop%iflag_scheme .ge. id_Crank_nicolson) then
        call alloc_type_zero_mat(mat_temp)
      end if
!
      if(     cd_prop%iflag_Bevo_scheme .gt. id_no_evolution            &
     &   .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call alloc_type_zero_mat(mat_magp)
      end if
!
      if ( cd_prop%iflag_Bevo_scheme .ge. id_Crank_nicolson) then
        call alloc_type_zero_mat(mat_magne)
      end if
!
      if ( cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson) then
        call alloc_type_zero_mat(mat_magne)
      end if
!
      if ( cp_prop%iflag_scheme .gt. id_no_evolution) then
        call alloc_type_zero_mat(mat_light)
      end if
!
      end subroutine alloc_MG_zero_matrices
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_aiccg_matrices                                 &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, mat_velo,           &
     &          mat_magne, mat_temp, mat_light, mat_press, mat_magp)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        call dealloc_type_djds_mat(mat_press)
!
        if (fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
          call dealloc_type_djds_mat(mat_velo)
        end if
      end if
!
      if (ht_prop%iflag_scheme .ge. id_Crank_nicolson) then
         call dealloc_type_djds_mat(mat_temp)
      end if
!
      if (cp_prop%iflag_scheme .ge. id_Crank_nicolson) then
        call dealloc_type_djds_mat(mat_light)
      end if
!
      if(     cd_prop%iflag_Bevo_scheme .gt. id_no_evolution            &
     &   .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call dealloc_type_djds_mat(mat_magp)
        if(    cd_prop%iflag_Bevo_scheme .ge. id_Crank_nicolson         &
     &    .or. cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson) then
          call dealloc_type_djds_mat(mat_magne)
        end if
      end if
!
      end subroutine dealloc_aiccg_matrices
!
! ----------------------------------------------------------------------
!
      end module allocate_solver_djds_MHD
