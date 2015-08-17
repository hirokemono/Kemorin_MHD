!cvt_dynamic_scheme_coord.f90
!      module cvt_dynamic_scheme_coord
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!> @brief Change coordinate system for dynamic model
!
!      subroutine cvt_vector_dynamic_scheme_coord
!      subroutine cvt_tensor_dynamic_scheme_coord
!
      module cvt_dynamic_scheme_coord
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
      private :: convert_dynamic_vectors_2_sph
      private :: convert_dynamic_vectors_2_cyl
      private :: convert_dynamic_tensors_2_sph
      private :: convert_dynamic_tensors_2_cyl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cvt_vector_dynamic_scheme_coord
!
      use m_geometry_constants
      use m_control_parameter
!
!
      if(icoord_SGS_model_coef .eq. iflag_spherical) then
        call convert_dynamic_vectors_2_sph
      else if(icoord_SGS_model_coef .eq. iflag_cylindrical) then
        call convert_dynamic_vectors_2_cyl
      end if
!
      end subroutine cvt_vector_dynamic_scheme_coord
!
!  ---------------------------------------------------------------------
!
      subroutine cvt_tensor_dynamic_scheme_coord
!
      use m_geometry_constants
      use m_control_parameter
!
!
      if(icoord_SGS_model_coef .eq. iflag_spherical) then
        call convert_dynamic_tensors_2_sph
      else if(icoord_SGS_model_coef .eq. iflag_cylindrical) then
        call convert_dynamic_tensors_2_cyl
      end if
!
      end subroutine cvt_tensor_dynamic_scheme_coord
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine convert_dynamic_vectors_2_cyl
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use cvt_xyz_vector_2_cyl_smp
!
!
      if(iflag_debug.gt.0) write(*,*) 'convert cylindrical corrdinate'
!$omp parallel
      call overwrite_vector_2_cyl_smp(np_smp, node1%numnod,             &
     &    node1%istack_nod_smp, d_nod(1,iphys%i_sgs_simi),              &
     &    node1%xx(1:node1%numnod,1), node1%xx(1:node1%numnod,2),       &
     &    node1%ss, a_s_cylinder)

      call overwrite_vector_2_cyl_smp(np_smp, node1%numnod,             &
     &    node1%istack_nod_smp, d_nod(1,iphys%i_sgs_grad),              &
     &    node1%xx(1:node1%numnod,1), node1%xx(1:node1%numnod,2),       &
     &    node1%ss, a_s_cylinder)

      call overwrite_vector_2_cyl_smp(np_smp, node1%numnod,             &
     &    node1%istack_nod_smp, d_nod(1,iphys%i_sgs_grad_f),            &
     &    node1%xx(1:node1%numnod,1), node1%xx(1:node1%numnod,2),       &
     &    node1%ss, a_s_cylinder)
!$omp end parallel
!
      end subroutine convert_dynamic_vectors_2_cyl
!
!  ---------------------------------------------------------------------
!
      subroutine convert_dynamic_vectors_2_sph
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use cvt_xyz_vector_2_sph_smp
!
!
      if(iflag_debug .gt. 0) write(*,*) 'convert spherical corrdinate'
!$omp parallel
      call overwrite_vector_2_sph_smp                                   &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    d_nod(1,iphys%i_sgs_simi), node1%xx(1:node1%numnod,1),        &
     &    node1%xx(1:node1%numnod,2), node1%xx(1:node1%numnod,3),       &
     &    node1%rr, node1%ss, a_radius, a_s_cylinder)

      call overwrite_vector_2_sph_smp                                   &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    d_nod(1,iphys%i_sgs_grad), node1%xx(1:node1%numnod,1),        &
     &    node1%xx(1:node1%numnod,2), node1%xx(1:node1%numnod,3),       &
     &    node1%rr, node1%ss, a_radius, a_s_cylinder)

      call overwrite_vector_2_sph_smp                                   &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    d_nod(1,iphys%i_sgs_grad_f), node1%xx(1:node1%numnod,1),      &
     &    node1%xx(1:node1%numnod,2), node1%xx(1:node1%numnod,3),       &
     &    node1%rr, node1%ss, a_radius, a_s_cylinder)
!$omp end parallel
!
      end subroutine convert_dynamic_vectors_2_sph
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine convert_dynamic_tensors_2_cyl
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use cvt_xyz_tensor_2_cyl_smp
!
!
      if(iflag_debug.gt.0) write(*,*) 'convert cylindrical corrdinate'
!$omp parallel
      call overwrite_cyl_tensor_smp                                     &
         (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    d_nod(1,iphys%i_sgs_simi),                                    &
     &    node1%xx(1:node1%numnod,1), node1%xx(1:node1%numnod,2),       &
     &    node1%ss, a_s_cylinder)

      call overwrite_cyl_tensor_smp                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    d_nod(1,iphys%i_sgs_grad),                                    &
     &    node1%xx(1:node1%numnod,1), node1%xx(1:node1%numnod,2),       &
     &    node1%ss, a_s_cylinder)

      call overwrite_cyl_tensor_smp                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    d_nod(1,iphys%i_sgs_grad_f),                                  &
     &    node1%xx(1:node1%numnod,1), node1%xx(1:node1%numnod,2),       &
     &    node1%ss, a_s_cylinder)
!$omp end parallel
!
      end subroutine convert_dynamic_tensors_2_cyl
!
!  ---------------------------------------------------------------------
!
      subroutine convert_dynamic_tensors_2_sph
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use cvt_xyz_tensor_2_sph_smp
!
!
      if(iflag_debug .gt. 0) write(*,*) 'convert spherical corrdinate'
!$omp parallel
      call overwrite_sph_tensor_smp                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    d_nod(1,iphys%i_sgs_simi), node1%xx(1:node1%numnod,1),        &
     &    node1%xx(1:node1%numnod,2), node1%xx(1:node1%numnod,3),       &
     &    node1%rr, node1%ss, a_radius, a_s_cylinder)
!
      call overwrite_sph_tensor_smp                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    d_nod(1,iphys%i_sgs_grad), node1%xx(1:node1%numnod,1),        &
     &    node1%xx(1:node1%numnod,2), node1%xx(1:node1%numnod,3),       &
     &    node1%rr, node1%ss, a_radius, a_s_cylinder)
!
      call overwrite_sph_tensor_smp                                     &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    d_nod(1,iphys%i_sgs_grad_f), node1%xx(1:node1%numnod,1),      &
     &    node1%xx(1:node1%numnod,2), node1%xx(1:node1%numnod,3),       &
     &    node1%rr, node1%ss, a_radius, a_s_cylinder)
!$omp end parallel
!
      end subroutine convert_dynamic_tensors_2_sph
!
!  ---------------------------------------------------------------------
!
      end module cvt_dynamic_scheme_coord
