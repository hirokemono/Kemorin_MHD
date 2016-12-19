!init_iccg_matrices.f90
!      module init_iccg_matrices
!
!        programmed by H.Matsui
!                                    on June 2005
!
!!      subroutine reset_MHD_aiccg_mat_type(node, ele, fluid,           &
!!     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,     &
!!     &          mat_velo, mat_magne, mat_temp, mat_light,             &
!!     &          mat_press, mat_magp)
!
      module init_iccg_matrices
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_control_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_geometry_data_MHD
      use t_solver_djds
!
      implicit none
!
      private :: reset_aiccg_11_MHD, reset_aiccg_33_MHD
      private :: reset_aiccg_vector_p
!
!  ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine reset_MHD_aiccg_mat_type(node, ele, fluid,             &
     &          djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,       &
     &          mat_velo, mat_magne, mat_temp, mat_light,               &
     &          mat_press, mat_magp)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(DJDS_ordering_table),  intent(in) :: djds_tbl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      if (evo_velo%iflag_scheme .gt. id_no_evolution) then
        call reset_aiccg_11_MHD(node, ele,                              &
     &      fluid%iele_start_fld, fluid%iele_end_fld,                   &
     &      num_t_linear, djds_tbl_fl_l, mat_press)
!
        if (evo_velo%iflag_scheme .ge. id_Crank_nicolson) then
          call reset_aiccg_33_MHD(node, ele,                            &
     &        fluid%iele_start_fld, fluid%iele_end_fld,                 &
     &        ele%nnod_4_ele, djds_tbl_fl, mat_velo)
        end if
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call reset_aiccg_11_MHD(node, ele,                              &
     &      fluid%iele_start_fld, fluid%iele_end_fld,                   &
     &      ele%nnod_4_ele, djds_tbl_fl, mat_temp)
      end if
!
      if (evo_comp%iflag_scheme .ge. id_Crank_nicolson) then
        call reset_aiccg_11_MHD(node, ele,                              &
     &      fluid%iele_start_fld, fluid%iele_end_fld,                   &
     &      ele%nnod_4_ele, djds_tbl_fl, mat_light)
      end if
!
      if (evo_magne%iflag_scheme .gt. id_no_evolution                   &
     &     .or. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        call reset_aiccg_11_MHD(node, ele, ione, ele%numele,            &
     &      num_t_linear, djds_tbl_l, mat_magp)
      end if
!
      if (evo_magne%iflag_scheme .gt. id_no_evolution) then
        call reset_aiccg_11_MHD(node, ele, ione, ele%numele,            &
     &      num_t_linear, djds_tbl_l, mat_magp)
        if (evo_magne%iflag_scheme .ge. id_Crank_nicolson) then
          call reset_aiccg_33_MHD(node, ele, ione, ele%numele,          &
     &        ele%nnod_4_ele, djds_tbl, mat_magne)
        end if
      end if
!
      if (evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        call reset_aiccg_11_MHD(node, ele, ione, ele%numele,            &
     &      num_t_linear, djds_tbl_l, mat_magp)
        if (evo_vect_p%iflag_scheme .ge. id_Crank_nicolson) then
          call reset_aiccg_vector_p(mat_magne)
        end if
      end if
!
      end subroutine reset_MHD_aiccg_mat_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_11_MHD(node, ele,                          &
     &          ist_ele, ied_ele, nnod_4_ele, DJDS, mat11_DJDS)
!
      integer(kind = kint) :: ist_ele, ied_ele, nnod_4_ele
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(DJDS_ordering_table), intent(in) :: DJDS
!
      type(DJDS_MATRIX), intent(inout) :: mat11_DJDS
!
      integer(kind = kint) :: inod, iele, in, k1
!
!$omp parallel workshare
      mat11_DJDS%ALUG_U= 1.0d0
      mat11_DJDS%ALUG_L= 1.0d0
!$omp end parallel workshare
!
!$omp parallel workshare
      mat11_DJDS%aiccg = 0.0d0
!$omp end parallel workshare
!
!$omp parallel workshare
      mat11_DJDS%aiccg(1:node%numnod) = 1.0d0
!$omp end parallel workshare
!
!$omp parallel
      do k1 = 1, nnod_4_ele
!$omp do private(iele, inod, in)
        do iele = ist_ele, ied_ele
          inod = ele%ie(iele,k1)
          in = DJDS%OLDtoNEW(inod)
          mat11_DJDS%aiccg(in) = 0.0d0
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine reset_aiccg_11_MHD
!
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_33_MHD(node, ele,                          &
     &          ist_ele, ied_ele, nnod_4_ele, DJDS, mat33_DJDS)
!
      integer(kind = kint) :: ist_ele, ied_ele, nnod_4_ele
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(DJDS_ordering_table), intent(in) :: DJDS
!
      type(DJDS_MATRIX), intent(inout) :: mat33_DJDS
!
      integer (kind=kint) :: in, inod, iele, k1
!
!
!$omp parallel workshare
      mat33_DJDS%ALUG_U= 1.d0
      mat33_DJDS%ALUG_L= 1.d0
!$omp end parallel workshare
!
!$omp parallel workshare
      mat33_DJDS%aiccg = 0.0d0
!$omp end parallel workshare
!
!$omp parallel do
      do inod = 1, node%numnod
        mat33_DJDS%aiccg(9*inod-8) = 1.0d0
        mat33_DJDS%aiccg(9*inod-4) = 1.0d0
        mat33_DJDS%aiccg(9*inod  ) = 1.0d0
      end do
!$omp end parallel do
!
      do k1 = 1, nnod_4_ele
        do iele = ist_ele, ied_ele
          inod = ele%ie(iele,k1)
          in = DJDS%OLDtoNEW(inod)
          mat33_DJDS%aiccg(9*in-8) = 0.0d0
          mat33_DJDS%aiccg(9*in-4) = 0.0d0
          mat33_DJDS%aiccg(9*in  ) = 0.0d0
        end do
      end do
!
      end subroutine reset_aiccg_33_MHD
!
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_vector_p(mat33_DJDS)
!
      type(DJDS_MATRIX), intent(inout) :: mat33_DJDS
!
!
!$omp parallel workshare
      mat33_DJDS%ALUG_U= 1.d0
      mat33_DJDS%ALUG_L= 1.d0
!$omp end parallel workshare
!
!$omp parallel workshare
      mat33_DJDS%aiccg = 0.0d0
!$omp end parallel workshare
!
      end subroutine reset_aiccg_vector_p
!
! ----------------------------------------------------------------------
!
      end module init_iccg_matrices
