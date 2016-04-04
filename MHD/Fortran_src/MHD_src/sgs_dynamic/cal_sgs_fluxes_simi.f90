!
!      module cal_sgs_fluxes_simi
!
!      Written by H. Matsui
!
!!      subroutine cal_sgs_mf_simi(i_sgs, i_vect, i_vect_f, icm_sgs,    &
!!     &          nod_comm, node, filtering, sgs_coefs_nod,             &
!!     &          wk_filter, nod_fld)
!!      subroutine cal_sgs_hf_simi(i_sgs, ifield, ifield_f, icm_sgs,    &
!!     &          nod_comm, node, iphys, filtering, sgs_coefs_nod,      &
!!     &          wk_filter, nod_fld)
!!      subroutine cal_sgs_induct_t_simi                                &
!!     &         (i_sgs, i_v, i_b, i_fil_v, i_fil_b, icm_sgs,           &
!!     &          nod_comm, node, filtering, sgs_coefs_nod,             &
!!     &          wk_filter, nod_fld)
!!      subroutine cal_sgs_uxb_simi(i_sgs, i_v, i_b, i_fil_v, i_fil_b,  &
!!     &          nod_comm, node, filtering, wk_filter, nod_fld)
!!
!!      subroutine cal_sgs_uxb_2_ff_simi                                &
!!     &          (icomp_sgs_uxb, nod_comm, node, ele, conduct, iphys,  &
!!     &           iphys_ele, ele_fld, jac_3d, rhs_tbl, filtering,      &
!!     &           sgs_coefs, wk_filter, fem_wk, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(MHD_coefficients_type), intent(in) :: sgs_coefs
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_sgs_fluxes_simi
!
      use m_precision
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filtering_data
      use t_material_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_mf_simi(i_sgs, i_vect, i_vect_f, icm_sgs,      &
     &          nod_comm, node, filtering, sgs_coefs_nod,               &
     &          wk_filter, nod_fld)
!
      use cal_fluxes
      use cal_similarity_terms
      use cal_filtering_scalars
!
      integer (kind=kint), intent(in) :: i_sgs, i_vect, i_vect_f
      integer (kind=kint), intent(in) :: icm_sgs
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
      type(MHD_coefficients_type), intent(in) :: sgs_coefs_nod
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!  ----------   set filtered flux into array
!
      call cal_flux_tensor(node, nod_fld%ntot_phys,                     &
     &    i_vect, i_vect, i_sgs, nod_fld%d_fld)
      call cal_filtered_sym_tensor_whole                                &
     &   (nod_comm, node, filtering, i_sgs, i_sgs, wk_filter, nod_fld)
!
!  ----------   substruct flux obtained by filterd values
!
      call cal_sgs_flux_tensor(node%numnod, node%istack_nod_smp,        &
     &    nod_fld%ntot_phys, i_sgs, i_vect_f, i_vect_f,                 &
     &    sgs_coefs_nod%ntot_comp, icm_sgs, sgs_coefs_nod%ak,           &
     &    nod_fld%d_fld)
!
      end subroutine cal_sgs_mf_simi
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_hf_simi(i_sgs, ifield, ifield_f, icm_sgs,      &
     &          nod_comm, node, iphys, filtering, sgs_coefs_nod,        &
     &          wk_filter, nod_fld)
!
      use cal_fluxes
      use cal_similarity_terms
      use cal_filtering_scalars
!
      integer (kind=kint), intent(in) :: i_sgs, ifield, ifield_f
      integer (kind=kint), intent(in) :: icm_sgs
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(filtering_data_type), intent(in) :: filtering
      type(MHD_coefficients_type), intent(in) :: sgs_coefs_nod
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_flux_vector(node, nod_fld%ntot_phys,                     &
     &    iphys%i_velo, ifield, i_sgs, nod_fld%d_fld)
      call cal_filtered_vector_whole                                    &
     &   (nod_comm, node, filtering, i_sgs, i_sgs, wk_filter, nod_fld)
!
      call cal_sgs_flux_vector(node%numnod, node%istack_nod_smp,        &
     &    nod_fld%ntot_phys, i_sgs, iphys%i_filter_velo, ifield_f,      &
     &    sgs_coefs_nod%ntot_comp, icm_sgs, sgs_coefs_nod%ak,           &
     &    nod_fld%d_fld)
!
      end subroutine cal_sgs_hf_simi
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_simi                                  &
     &         (i_sgs, i_v, i_b, i_fil_v, i_fil_b, icm_sgs,             &
     &          nod_comm, node, filtering, sgs_coefs_nod,               &
     &          wk_filter, nod_fld)
!
      use cal_fluxes
      use cal_similarity_terms
      use cal_filtering_scalars
!
      integer (kind=kint), intent(in) :: i_sgs, i_v, i_b
      integer (kind=kint), intent(in) :: i_fil_v, i_fil_b, icm_sgs
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
      type(MHD_coefficients_type), intent(in) :: sgs_coefs_nod
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!  ----------   set filtered flux into array
!
      call cal_induction_tensor(node, nod_fld%ntot_phys,                &
     &    i_b, i_v, i_sgs, nod_fld%d_fld)
      call cal_filtered_vector_whole                                    &
     &   (nod_comm, node, filtering, i_sgs, i_sgs, wk_filter, nod_fld)
!
!  ----------   substruct flux obtained by filterd values
!
      call subctract_induction_tensor                                   &
     &   (node%numnod, node%istack_nod_smp, nod_fld%ntot_phys,          &
     &    i_sgs, i_fil_b, i_fil_v, sgs_coefs_nod%ntot_comp, icm_sgs,    &
     &    sgs_coefs_nod%ak, nod_fld%d_fld)
!
      end subroutine cal_sgs_induct_t_simi
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_simi(i_sgs, i_v, i_b, i_fil_v, i_fil_b,    &
     &          nod_comm, node, filtering, wk_filter, nod_fld)
!
      use cal_filtering_scalars
      use products_nodal_fields_smp
      use cal_similarity_terms
!
      integer (kind=kint), intent(in) :: i_sgs, i_v, i_b
      integer (kind=kint), intent(in) :: i_fil_v, i_fil_b
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
!$omp parallel
      call cal_phys_cross_product(node, nod_fld, i_v, i_b, i_sgs)
!$omp end parallel
!
      call cal_filtered_vector_whole                                    &
     &   (nod_comm, node, filtering, i_sgs, i_sgs, wk_filter, nod_fld)
!
      call subctract_uxb_vector(node%numnod, node%istack_nod_smp,       &
     &    nod_fld%ntot_phys, i_sgs, i_fil_v, i_fil_b, nod_fld%d_fld)
!
      end subroutine cal_sgs_uxb_simi
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_2_ff_simi                                  &
     &          (icomp_sgs_uxb, nod_comm, node, ele, conduct, iphys,    &
     &           iphys_ele, ele_fld, jac_3d, rhs_tbl, filtering,        &
     &           sgs_coefs, wk_filter, fem_wk, f_nl, nod_fld)
!
      use int_vol_similarity_uxb
!
      integer (kind=kint), intent(in) :: icomp_sgs_uxb
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(filtering_data_type), intent(in) :: filtering
      type(MHD_coefficients_type), intent(in) :: sgs_coefs
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_sgs_uxb_simi(iphys%i_sgs_simi, iphys%i_velo,             &
     &    iphys%i_magne, iphys%i_filter_velo, iphys%i_filter_magne,     &
     &    nod_comm, node, filtering, wk_filter, nod_fld)
!
      call sel_int_simi_vp_induct(icomp_sgs_uxb, node, ele, conduct,    &
     &    iphys, nod_fld, iphys_ele, ele_fld, jac_3d, rhs_tbl,          &
     &    sgs_coefs, fem_wk, f_nl)
!
      end subroutine cal_sgs_uxb_2_ff_simi
!
!-----------------------------------------------------------------------
!
      end module cal_sgs_fluxes_simi
