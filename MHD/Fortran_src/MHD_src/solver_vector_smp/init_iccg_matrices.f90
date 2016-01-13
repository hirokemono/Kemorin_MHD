!init_iccg_matrices.f90
!      module init_iccg_matrices
!
!        programmed by H.Matsui
!                                    on June 2005
!
!      subroutine allocate_aiccg_matrices
!      subroutine reset_aiccg_matrices
!      subroutine deallocate_aiccg_matrices
!
      module init_iccg_matrices
!
      use m_precision
!
      use calypso_mpi
      use m_control_parameter
      use m_geometry_constants
      use m_geometry_data_MHD
      use m_geometry_data
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
      subroutine allocate_aiccg_matrices
!
      use set_residual_limit
!
!
      call set_residual_4_crank
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call alloc_type_djds11_mat(node1%numnod, node1%internal_node,   &
     &      DJDS_fl_l, Pmat_DJDS)
!
        if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(node1%numnod, node1%internal_node, &
     &        DJDS_fluid, Vmat_DJDS)
        end if
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call alloc_type_djds11_mat(node1%numnod, node1%internal_node,   &
     &      DJDS_fluid, Tmat_DJDS)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call alloc_type_djds11_mat(node1%numnod, node1%internal_node,   &
     &      DJDS_fluid, Cmat_DJDS)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution) then
        call alloc_type_djds11_mat(node1%numnod, node1%internal_node,   &
     &      DJDS_linear, Fmat_DJDS)
        if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(node1%numnod, node1%internal_node, &
     &        DJDS_entire, Bmat_DJDS)
        end if
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call alloc_type_djds11_mat(node1%numnod, node1%internal_node,   &
     &      DJDS_linear, Fmat_DJDS)
        if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
          call alloc_type_djds33_mat(node1%numnod, node1%internal_node, &
     &        DJDS_entire, Bmat_DJDS)
        end if
      end if
!
      end subroutine allocate_aiccg_matrices
!
!  ----------------------------------------------------------------------
!
      subroutine reset_aiccg_matrices
!
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call reset_aiccg_11_MHD(node1, ele1,                            &
     &      fluid1%iele_start_fld, fluid1%iele_end_fld,                 &
     &      num_t_linear, DJDS_fl_l, Pmat_DJDS)
!
        if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
          call reset_aiccg_33_MHD(node1, ele1,                          &
     &        fluid1%iele_start_fld, fluid1%iele_end_fld,               &
     &        ele1%nnod_4_ele, DJDS_fluid, Vmat_DJDS)
        end if
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call reset_aiccg_11_MHD(node1, ele1,                            &
     &      fluid1%iele_start_fld, fluid1%iele_end_fld,                 &
     &      ele1%nnod_4_ele, DJDS_fluid, Tmat_DJDS)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call reset_aiccg_11_MHD(node1, ele1,                            &
     &      fluid1%iele_start_fld, fluid1%iele_end_fld,                 &
     &      ele1%nnod_4_ele, DJDS_fluid, Cmat_DJDS)
      end if
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution) then
        call reset_aiccg_11_MHD(node1, ele1, ione, ele1%numele,         &
     &      num_t_linear, DJDS_linear, Fmat_DJDS)
        if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
          call reset_aiccg_33_MHD(node1, ele1,  ione, ele1%nnod_4_ele,  &
     &     ele1%nnod_4_ele, DJDS_entire, Bmat_DJDS)
        end if
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call reset_aiccg_11_MHD(node1, ele1, ione, ele1%numele,         &
     &      num_t_linear, DJDS_linear, Fmat_DJDS)
        if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
          call reset_aiccg_vector_p(Bmat_DJDS)
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
        call dealloc_type_djds_mat(Pmat_DJDS)
!
        if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
          call dealloc_type_djds_mat(Vmat_DJDS)
        end if
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
         call dealloc_type_djds_mat(Tmat_DJDS)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call dealloc_type_djds_mat(Cmat_DJDS)
      end if
!
      if(     iflag_t_evo_4_magne .gt. id_no_evolution                  &
     &   .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call dealloc_type_djds_mat(Fmat_DJDS)
        if(    iflag_t_evo_4_magne .ge. id_Crank_nicolson               &
     &    .or. iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
          call dealloc_type_djds_mat(Bmat_DJDS)
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
