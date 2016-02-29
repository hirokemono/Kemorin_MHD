!init_iccg_matrices.f90
!      module init_iccg_matrices
!
!        programmed by H.Matsui
!                                    on June 2005
!
!      subroutine allocate_aiccg_matrices(node)
!      subroutine reset_aiccg_matrices(node, ele, fluid)
!      subroutine deallocate_aiccg_matrices
!
      module init_iccg_matrices
!
      use m_precision
!
      use calypso_mpi
      use m_control_parameter
      use m_geometry_constants
      use m_solver_djds_MHD
!
      use t_geometry_data
      use t_geometry_data_MHD
      use t_solver_djds
!
      implicit none
!
!  ----------------------------------------------------------------------
!
      contains
!
!  ----------------------------------------------------------------------
!
      subroutine allocate_aiccg_matrices(node)
!
      use set_residual_limit
!
      type(node_data), intent(in) :: node
!
!
      call set_residual_4_crank
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &      DJDS_fl_l, MHD1_matrices%Pmat_MG_DJDS(0))
!
        if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(node%numnod, node%internal_node,   &
     &        DJDS_fluid, MHD1_matrices%Vmat_MG_DJDS(0))
        end if
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &      DJDS_fluid, MHD1_matrices%Tmat_MG_DJDS(0))
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &      DJDS_fluid, MHD1_matrices%Cmat_MG_DJDS(0))
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &      DJDS_linear, MHD1_matrices%Fmat_MG_DJDS(0))
        if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(node%numnod, node%internal_node,   &
     &        DJDS_entire, MHD1_matrices%Bmat_MG_DJDS(0))
        end if
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call alloc_type_djds11_mat(node%numnod, node%internal_node,     &
     &      DJDS_linear, MHD1_matrices%Fmat_MG_DJDS(0))
        if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(node%numnod, node%internal_node,   &
     &        DJDS_entire, MHD1_matrices%Bmat_MG_DJDS(0))
        end if
      end if
!
      end subroutine allocate_aiccg_matrices
!
!  ----------------------------------------------------------------------
!
      subroutine reset_aiccg_matrices(node, ele, fluid)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
!
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call reset_aiccg_11_MHD(node, ele,                              &
     &      fluid%iele_start_fld, fluid%iele_end_fld,                   &
     &      num_t_linear, DJDS_fl_l, MHD1_matrices%Pmat_MG_DJDS(0))
!
        if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
          call reset_aiccg_33_MHD(node, ele,                            &
     &        fluid%iele_start_fld, fluid%iele_end_fld, ele%nnod_4_ele, &
     &        DJDS_fluid, MHD1_matrices%Vmat_MG_DJDS(0))
        end if
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call reset_aiccg_11_MHD(node, ele,                              &
     &      fluid%iele_start_fld, fluid%iele_end_fld,                   &
     &      ele%nnod_4_ele, DJDS_fluid, MHD1_matrices%Tmat_MG_DJDS(0))
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call reset_aiccg_11_MHD(node, ele,                              &
     &      fluid%iele_start_fld, fluid%iele_end_fld,                   &
     &      ele%nnod_4_ele, DJDS_fluid, MHD1_matrices%Cmat_MG_DJDS(0))
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution) then
        call reset_aiccg_11_MHD(node, ele, ione, ele%numele,            &
     &      num_t_linear, DJDS_linear, MHD1_matrices%Fmat_MG_DJDS(0))
        if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
          call reset_aiccg_33_MHD(node, ele,  ione, ele%nnod_4_ele,     &
     &     ele%nnod_4_ele, DJDS_entire, MHD1_matrices%Bmat_MG_DJDS(0))
        end if
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call reset_aiccg_11_MHD(node, ele, ione, ele%numele,            &
     &      num_t_linear, DJDS_linear, MHD1_matrices%Fmat_MG_DJDS(0))
        if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
          call reset_aiccg_vector_p(MHD1_matrices%Bmat_MG_DJDS(0))
        end if
      end if
!
      end subroutine reset_aiccg_matrices
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_aiccg_matrices
!
      use set_residual_limit
!
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call dealloc_type_djds_mat(MHD1_matrices%Pmat_MG_DJDS(0))
!
        if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
          call dealloc_type_djds_mat(MHD1_matrices%Vmat_MG_DJDS(0))
        end if
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
         call dealloc_type_djds_mat(MHD1_matrices%Tmat_MG_DJDS(0))
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call dealloc_type_djds_mat(MHD1_matrices%Cmat_MG_DJDS(0))
      end if
!
      if(     iflag_t_evo_4_magne .gt. id_no_evolution                  &
     &   .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call dealloc_type_djds_mat(MHD1_matrices%Fmat_MG_DJDS(0))
        if(    iflag_t_evo_4_magne .ge. id_Crank_nicolson               &
     &    .or. iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
          call dealloc_type_djds_mat(MHD1_matrices%Bmat_MG_DJDS(0))
        end if
      end if
!
      end subroutine deallocate_aiccg_matrices
!
!  ----------------------------------------------------------------------
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
